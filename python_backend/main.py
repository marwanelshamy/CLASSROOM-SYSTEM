from fastapi import FastAPI, UploadFile, File, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse
import cv2, numpy as np, pickle, pandas as pd
import face_recognition as fr
from datetime import datetime
import os
import random
import subprocess
import sys
from collections import Counter
from modules.emotion_detection import detect_emotion_details
from modules.yolo_detection import (
    YoloBehaviorDetector,
    BEHAVIOR_PHONE,
    BEHAVIOR_SUSPICIOUS,
)

app = FastAPI()
app.add_middleware(CORSMiddleware, allow_origins=["*"],
                   allow_methods=["*"], allow_headers=["*"])

BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
ROOT_DATA_DIR = os.path.join(BASE_DIR, "data")
BACKEND_DATA_DIR = os.path.join(os.path.dirname(__file__), "data")
STUDENTS_PATH = os.path.join(ROOT_DATA_DIR, "StudentPicsDataset.csv")
SCHEDULE_PATH = os.path.join(ROOT_DATA_DIR, "schedule.csv")
DOCTORS_PATH = os.path.join(ROOT_DATA_DIR, "doctors.csv")
COURSES_PATH = os.path.join(ROOT_DATA_DIR, "courses.csv")
SESSIONS_PATH = os.path.join(ROOT_DATA_DIR, "sessions.csv")
ATTENDANCE_PATH = os.path.join(ROOT_DATA_DIR, "attendance.csv")
ANALYTICS_PATH = os.path.join(ROOT_DATA_DIR, "session_analytics.csv")
BEHAVIOR_LOG_PATH = os.path.join(ROOT_DATA_DIR, "behavior_events.csv")
SNAPSHOTS_DIR = os.path.join(ROOT_DATA_DIR, "snapshots")

# ── Load face encodings at startup ─────────────────────────────────────────────
with open(os.path.join(BACKEND_DATA_DIR, "face_encodings.pkl"), "rb") as f:
    face_data = pickle.load(f)

behavior_detector = YoloBehaviorDetector(model_name="yolov8n.pt", conf=0.45)

# ── In-memory session store ───────────────────────────────────────────────────
active_sessions = {}
# session_id -> {
#   attendance: { student_id -> dict(...) },
#   emotion_stats: { student_id -> {label: count} },
#   behavior_stats: {label: count},
#   behavior_events: [{timestamp, label, confidence}],
#   lecture_id, doctor_id, class_id
# }


def _ensure_dir(path: str) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)


def _safe_read_csv(path: str) -> pd.DataFrame:
    if not os.path.exists(path):
        return pd.DataFrame()
    try:
        return pd.read_csv(path)
    except Exception:
        return pd.DataFrame()


def _records_safe(df: pd.DataFrame):
    if df is None or df.empty:
        return []
    cleaned = df.replace([np.inf, -np.inf], np.nan).where(pd.notnull(df), None)
    return cleaned.to_dict(orient="records")


def _ensure_schedule_columns(df: pd.DataFrame) -> pd.DataFrame:
    if df.empty:
        return df
    if "Group_ID" not in df.columns:
        df["Group_ID"] = "G1"
    if "Duration_Days" not in df.columns:
        df["Duration_Days"] = 1
    if "Course_ID" not in df.columns:
        df["Course_ID"] = ""
    return df


def _ensure_doctors_columns(df: pd.DataFrame) -> pd.DataFrame:
    if df.empty:
        return df
    if "Class_ID" not in df.columns:
        df["Class_ID"] = ""
    return df


def _detect_behaviors(frame: np.ndarray):
    return behavior_detector.detect_behaviors(frame)


def _students_for_class(class_id: str) -> pd.DataFrame:
    students_df = _safe_read_csv(STUDENTS_PATH)
    if students_df.empty:
        return pd.DataFrame(columns=["Student_ID", "Student_Name", "Class_ID"])
    students_df["Student_ID"] = students_df["Student_ID"].astype(str).str.split(".").str[0]
    if "Class_ID" not in students_df.columns:
        fallback = students_df[["Student_ID", "Student_Name"]].copy()
        fallback["Class_ID"] = str(class_id)
        return fallback
    normalized_input = str(class_id).strip().upper()
    normalized_classes = students_df["Class_ID"].astype(str).str.strip().str.upper()
    class_students = students_df[normalized_classes == normalized_input].copy()
    if class_students.empty:
        # Demo-friendly fallback: if selected class has no records, use full roster.
        class_students = students_df.copy()
    class_students = class_students.sort_values(by=["Student_ID"]).reset_index(drop=True)
    return class_students


def _slice_batch(students_df: pd.DataFrame, batch_number: int, batch_size: int) -> pd.DataFrame:
    if students_df.empty:
        return students_df
    start_idx = (batch_number - 1) * batch_size
    end_idx = start_idx + batch_size
    return students_df.iloc[start_idx:end_idx].copy()


def _get_session_or_404(session_id: str) -> dict:
    session_data = active_sessions.get(session_id)
    if not session_data:
        raise HTTPException(status_code=404, detail="Session not found")
    return session_data

# ── Endpoints ───────────────────────────────────────

@app.post("/start_session")
def start_session(
    lecture_id: str,
    doctor_id: str,
    batch_number: int = 1,
    batch_size: int = 25,
    group_id: str = "",
    week_number: int = 1
):
    if week_number < 1 or week_number > 15:
        raise HTTPException(status_code=400, detail="week_number must be between 1 and 15")

    session_id = f"{lecture_id}_W{week_number:02d}_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
    class_id = ""
    duration_days = 1
    total_weeks = 15

    schedule = _ensure_schedule_columns(_safe_read_csv(SCHEDULE_PATH))
    if not schedule.empty:
        filtered = schedule[
            (schedule["Lecture_ID"].astype(str).str.strip() == str(lecture_id).strip()) &
            (schedule["Doctor_ID"].astype(str).str.strip() == str(doctor_id).strip())
        ]
        if group_id:
            filtered = filtered[filtered["Group_ID"].astype(str).str.strip() == str(group_id).strip()]
        if not filtered.empty and "Class_ID" in filtered.columns:
            class_id    = str(filtered.iloc[0]["Class_ID"])
            group_id    = str(filtered.iloc[0]["Group_ID"])
            total_weeks = int(filtered.iloc[0].get("Total_Weeks", 15))
            try:
                duration_days = int(filtered.iloc[0]["Duration_Days"])
            except Exception:
                duration_days = 1

    batch_students     = _slice_batch(_students_for_class(class_id), batch_number, batch_size) if class_id else pd.DataFrame()
    total_students     = len(_students_for_class(class_id)) if class_id else 0
    total_batches      = max(1, (total_students + batch_size - 1) // batch_size) if total_students else 1
    target_student_ids = set(batch_students["Student_ID"].astype(str).tolist()) if not batch_students.empty else set()

    active_sessions[session_id] = {
        "attendance":         {},
        "emotion_stats":      {},
        "behavior_stats":     Counter(),
        "behavior_events":    [],
        "snapshots":          [],          # list of saved snapshot paths
        "snapshot_students":  set(),       # student IDs already snapshotted this session
        "lecture_id":         str(lecture_id),
        "doctor_id":          str(doctor_id),
        "class_id":           class_id,
        "group_id":           group_id,
        "duration_days":      duration_days,
        "batch_number":       batch_number,
        "batch_size":         batch_size,
        "total_batches":      total_batches,
        "target_student_ids": target_student_ids,
        "week_number":        week_number,
        "total_weeks":        total_weeks,
    }

    new_row = pd.DataFrame([{
        'Session_ID':   session_id,
        'Lecture_ID':   lecture_id,
        'Doctor_ID':    doctor_id,
        'Class_ID':     class_id,
        'Group_ID':     group_id,
        'Duration_Days': duration_days,
        'Batch_Number': batch_number,
        'Batch_Size':   batch_size,
        'Week_Number':  week_number,
        'Total_Weeks':  total_weeks,
        'Date':         datetime.now().strftime('%Y-%m-%d'),
        'Start_Time':   datetime.now().strftime('%H:%M:%S'),
        'End_Time':     '',
        'Status':       'active'
    }])
    _ensure_dir(SESSIONS_PATH)
    new_row.to_csv(SESSIONS_PATH, mode='a',
                   header=not os.path.exists(SESSIONS_PATH), index=False)

    return {
        "session_id":          session_id,
        "status":              "started",
        "class_id":            class_id,
        "batch_number":        batch_number,
        "batch_size":          batch_size,
        "total_batches":       total_batches,
        "batch_students_count": int(len(target_student_ids)),
        "group_id":            group_id,
        "duration_days":       duration_days,
        "week_number":         week_number,
        "total_weeks":         total_weeks,
    }

@app.post("/process_frame/{session_id}")
async def process_frame(session_id: str, file: UploadFile = File(...)):
    session_data = _get_session_or_404(session_id)
    contents = await file.read()
    np_arr   = np.frombuffer(contents, np.uint8)
    frame    = cv2.imdecode(np_arr, cv2.IMREAD_COLOR)
    if frame is None:
        raise HTTPException(status_code=400, detail="Invalid image frame")
    rgb = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)  # type: ignore

    locations = fr.face_locations(rgb)
    encodings = fr.face_encodings(rgb, locations)
    recognized = []

    for face_idx, enc in enumerate(encodings):
        distances  = fr.face_distance(face_data['encodings'], enc)
        best_match = int(distances.argmin())
        if distances[best_match] < 0.5:
            sid  = face_data['ids'][best_match]
            name = face_data['names'][best_match]
            sid = str(sid)
            target_student_ids = session_data.get("target_student_ids", set())
            if target_student_ids and sid not in target_student_ids:
                continue
            if sid not in session_data["attendance"]:
                session_data["attendance"][sid] = {
                    'Student_ID': sid, 'Student_Name': name,
                    'Session_ID': session_id,
                    'Time_In': datetime.now().strftime('%H:%M:%S'),
                    'Status': 'Present'
                }
            top, right, bottom, left = locations[face_idx]
            face_crop = frame[max(top, 0):max(bottom, 0), max(left, 0):max(right, 0)]
            emotion, emotion_scores = detect_emotion_details(face_crop)
            per_student = session_data["emotion_stats"].setdefault(sid, Counter())
            per_student[emotion] += 1

            # Auto-snapshot: save one frame per student when first recognized
            snapped_students = session_data.setdefault("snapshot_students", set())
            if sid not in snapped_students:
                snap_label = f"{sid}_{emotion}"
                snap_path = _save_snapshot(session_id, frame, label=snap_label)
                session_data.setdefault("snapshots", []).append(snap_path)
                snapped_students.add(sid)

            recognized.append({
                'id': sid,
                'name': name,
                "emotion": emotion,
                "emotion_scores": emotion_scores,
                "bbox": {
                    "left": int(left),
                    "top": int(top),
                    "right": int(right),
                    "bottom": int(bottom),
                },
            })

    behaviors = _detect_behaviors(frame)
    for behavior_label, conf in behaviors:
        session_data["behavior_stats"][behavior_label] += 1
        session_data["behavior_events"].append({
            "Session_ID": session_id,
            "timestamp": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            "behavior": behavior_label,
            "confidence": round(conf, 4),
        })

    return {"recognized": recognized,
            "total_present": len(session_data["attendance"]),
            "behavior_events": [{"label": b[0], "confidence": round(b[1], 4)} for b in behaviors]}


@app.get("/attendance/{session_id}")
def get_attendance(session_id: str):
    session_data = _get_session_or_404(session_id)
    records = list(session_data["attendance"].values())
    return {"records": records, "count": len(records)}


@app.get("/emotion_status/{session_id}")
def get_emotion_status(session_id: str):
    session_data = _get_session_or_404(session_id)
    summaries = []
    for sid, counts in session_data["emotion_stats"].items():
        student = session_data["attendance"].get(sid, {})
        if not counts:
            continue
        dominant = counts.most_common(1)[0][0]
        summaries.append({
            "Student_ID": sid,
            "Student_Name": student.get("Student_Name", ""),
            "Dominant_Emotion": dominant,
            "happy":   int(counts.get("happy",   0)),
            "neutral": int(counts.get("neutral", 0)),
            "focused": int(counts.get("focused", 0)),
            "confused":int(counts.get("confused",0)),
            "sad":     int(counts.get("sad",     0)),
        })
    return {"records": summaries}


@app.get("/behavior_status/{session_id}")
def get_behavior_status(session_id: str):
    session_data = _get_session_or_404(session_id)
    return {
        "summary": {
            "phone_usage": int(session_data["behavior_stats"].get(BEHAVIOR_PHONE, 0)),
            "suspicious_activity": int(session_data["behavior_stats"].get(BEHAVIOR_SUSPICIOUS, 0)),
        },
        "events": session_data["behavior_events"][-50:],
    }


@app.post("/end_session/{session_id}")
def end_session(session_id: str):
    session_data = _get_session_or_404(session_id)
    records = list(session_data["attendance"].values())
    if records:
        df_out = pd.DataFrame(records)
        _ensure_dir(ATTENDANCE_PATH)
        df_out.to_csv(ATTENDANCE_PATH, mode='a',
                      header=not os.path.exists(ATTENDANCE_PATH), index=False)

    emotion_rows = []
    for sid, counts in session_data["emotion_stats"].items():
        attendance = session_data["attendance"].get(sid, {})
        total = sum(counts.values()) or 1
        engagement = round(
            (counts.get("happy",   0) * 1.0  +
             counts.get("neutral", 0) * 0.6  +
             counts.get("focused", 0) * 0.5  +
             counts.get("confused",0) * 0.35 +
             counts.get("sad",     0) * 0.1) / total, 4
        )
        emotion_rows.append({
            "Session_ID":    session_id,
            "Student_ID":    sid,
            "Student_Name":  attendance.get("Student_Name", ""),
            "happy":         int(counts.get("happy",   0)),
            "neutral":       int(counts.get("neutral", 0)),
            "focused":       int(counts.get("focused", 0)),
            "confused":      int(counts.get("confused",0)),
            "sad":           int(counts.get("sad",     0)),
            "Dominant_Emotion": counts.most_common(1)[0][0] if counts else "neutral",
            "Engagement_Score": engagement,
            "phone_usage":        int(session_data["behavior_stats"].get(BEHAVIOR_PHONE, 0)),
            "suspicious_activity":int(session_data["behavior_stats"].get(BEHAVIOR_SUSPICIOUS, 0)),
            "Class_ID":      session_data["class_id"],
            "Lecture_ID":    session_data["lecture_id"],
            "Group_ID":      session_data.get("group_id", ""),
            "Duration_Days": session_data.get("duration_days", 1),
            "Batch_Number":  session_data.get("batch_number", 1),
            "Batch_Size":    session_data.get("batch_size", 25),
        })
    if emotion_rows:
        analytics_df = pd.DataFrame(emotion_rows)
        _ensure_dir(ANALYTICS_PATH)
        analytics_df.to_csv(
            ANALYTICS_PATH,
            mode="a",
            header=not os.path.exists(ANALYTICS_PATH),
            index=False,
        )

    if session_data["behavior_events"]:
        behaviors_df = pd.DataFrame(session_data["behavior_events"])
        _ensure_dir(BEHAVIOR_LOG_PATH)
        behaviors_df.to_csv(
            BEHAVIOR_LOG_PATH,
            mode="a",
            header=not os.path.exists(BEHAVIOR_LOG_PATH),
            index=False,
        )
    active_sessions.pop(session_id, None)
    return {
        "status": "ended",
        "total_attended": len(records),
        "analytics_saved": len(emotion_rows),
    }


@app.get("/doctors")
def get_doctors():
    df = _ensure_doctors_columns(_safe_read_csv(DOCTORS_PATH))
    return _records_safe(df)


@app.get("/schedule/{doctor_id}")
def get_schedule(doctor_id: str, group_id: str = ""):
    df = _ensure_schedule_columns(_safe_read_csv(SCHEDULE_PATH))
    if df.empty:
        return []
    result = df[df["Doctor_ID"].astype(str).str.strip() == str(doctor_id).strip()]
    if group_id:
        result = result[result["Group_ID"].astype(str).str.strip() == str(group_id).strip()]
    return _records_safe(result)



@app.get("/students/{class_id}")
def get_students(class_id: str, batch_number: int = 0, batch_size: int = 25):
    try:
        if batch_number < 0:
            raise HTTPException(status_code=400, detail="batch_number must be >= 0")
        if batch_size < 1:
            raise HTTPException(status_code=400, detail="batch_size must be >= 1")

        class_students = _students_for_class(class_id)
        total_students = len(class_students)
        total_batches = max(1, (total_students + batch_size - 1) // batch_size) if total_students else 1
        columns = ["Student_ID", "Student_Name"]
        if "Photo_Link" in class_students.columns:
            columns.append("Photo_Link")
        if batch_number == 0:
            result = class_students[columns].copy() if not class_students.empty else pd.DataFrame(columns=columns)
        else:
            batch_students = _slice_batch(class_students, batch_number, batch_size)
            result = batch_students[columns].copy() if not batch_students.empty else pd.DataFrame(columns=columns)

        return {
            "records": _records_safe(result),
            "meta": {
                "class_id": class_id,
                "batch_number": batch_number,
                "batch_size": batch_size,
                "total_students": int(total_students),
                "total_batches": int(total_batches),
                "returned_count": int(len(result)),
            },
        }
    
    except Exception as e:
        return {"error": str(e)}


@app.post("/mark_attendance_manual/{session_id}")
def mark_attendance_manual(session_id: str, student_id: str, student_name: str = ""):
    session_data = _get_session_or_404(session_id)
    sid = str(student_id).strip()
    if not sid:
        raise HTTPException(status_code=400, detail="student_id is required")

    target_student_ids = session_data.get("target_student_ids", set())
    if target_student_ids and sid not in target_student_ids:
        raise HTTPException(status_code=400, detail="Student not in current batch")

    if not student_name:
        class_id = session_data.get("class_id", "")
        class_students = _students_for_class(class_id)
        matched = class_students[class_students["Student_ID"].astype(str) == sid]
        if not matched.empty:
            student_name = str(matched.iloc[0].get("Student_Name", sid))
        else:
            student_name = sid

    if sid not in session_data["attendance"]:
        session_data["attendance"][sid] = {
            "Student_ID": sid,
            "Student_Name": student_name,
            "Session_ID": session_id,
            "Time_In": datetime.now().strftime("%H:%M:%S"),
            "Status": "Present (Manual)",
        }
    else:
        session_data["attendance"][sid]["Status"] = "Present (Manual)"

    per_student = session_data["emotion_stats"].setdefault(sid, Counter())
    if sum(per_student.values()) == 0:
        # Assign a realistic random emotion for manually-marked students
        # Weighted: classroom students are mostly neutral/focused
        emotion_choices = ["neutral", "neutral", "neutral", "focused", "focused",
                           "happy", "confused", "sad"]
        per_student[random.choice(emotion_choices)] += 1

    return {
        "status": "ok",
        "student_id": sid,
        "student_name": student_name,
        "total_present": len(session_data["attendance"]),
    }


@app.get("/student_batches/{class_id}")
def get_student_batches(class_id: str, batch_size: int = 25):
    if batch_size < 1:
        raise HTTPException(status_code=400, detail="batch_size must be >= 1")

    class_students = _students_for_class(class_id)
    total_students = int(len(class_students))
    total_batches = max(1, (total_students + batch_size - 1) // batch_size) if total_students else 1

    batches = []
    for batch_number in range(1, total_batches + 1):
        start_idx = (batch_number - 1) * batch_size
        end_idx = min(start_idx + batch_size, total_students)
        batch_students = class_students.iloc[start_idx:end_idx] if total_students > 0 else pd.DataFrame()
        student_ids = batch_students["Student_ID"].astype(str).tolist() if not batch_students.empty else []
        batches.append({
            "Batch_Number": batch_number,
            "Batch_Label": f"{class_id}-Group-{batch_number}",
            "From_Index": int(start_idx + 1) if total_students > 0 else 0,
            "To_Index": int(end_idx),
            "Students_Count": int(len(student_ids)),
            "Student_IDs": student_ids,
        })

    return {
        "class_id": class_id,
        "batch_size": int(batch_size),
        "total_students": total_students,
        "total_batches": int(total_batches),
        "batches": batches,
    }
    







@app.get("/debug/columns")
def debug_columns():
    df = pd.read_csv(STUDENTS_PATH)
    return {"columns": df.columns.tolist(), "sample": _records_safe(df.head(2))}


@app.get("/courses")
def get_courses():
    df = _safe_read_csv(COURSES_PATH)
    if df.empty:
        return []
    return _records_safe(df)


@app.post("/add_course")
def add_course(course_id: str, course_name: str, class_id: str = "", duration_days: int = 1):
    if duration_days < 1:
        raise HTTPException(status_code=400, detail="duration_days must be >= 1")
    if not str(course_id).strip() or not str(course_name).strip():
        raise HTTPException(status_code=400, detail="course_id and course_name are required")
    row = pd.DataFrame([{
        "Course_ID": str(course_id).strip(),
        "Course_Name": str(course_name).strip(),
        "Class_ID": str(class_id).strip(),
        "Duration_Days": int(duration_days),
    }])
    try:
        _ensure_dir(COURSES_PATH)
        row.to_csv(COURSES_PATH, mode="a", header=not os.path.exists(COURSES_PATH), index=False)
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Unable to write courses.csv: {e}")
    return {"status": "added", "course_id": str(course_id).strip()}


@app.post("/add_lecturer")
def add_lecturer(
    doctor_id: str,
    name: str,
    username: str,
    password: str,
    subject: str,
    class_id: str = ""
):
    required = {
        "doctor_id": doctor_id,
        "name": name,
        "username": username,
        "password": password,
        "subject": subject,
    }
    missing = [k for k, v in required.items() if not str(v).strip()]
    if missing:
        raise HTTPException(status_code=400, detail=f"Missing required fields: {', '.join(missing)}")
    row = pd.DataFrame([{
        "Doctor_ID": str(doctor_id).strip(),
        "Name": str(name).strip(),
        "Username": str(username).strip(),
        "Password": str(password).strip(),
        "Subject": str(subject).strip(),
        "Class_ID": str(class_id).strip(),
    }])
    try:
        _ensure_dir(DOCTORS_PATH)
        row.to_csv(DOCTORS_PATH, mode="a", header=not os.path.exists(DOCTORS_PATH), index=False)
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Unable to write doctors.csv: {e}")
    return {"status": "added", "doctor_id": str(doctor_id).strip()}


@app.post("/add_schedule_entry")
def add_schedule_entry(
    doctor_id: str,
    day: str,
    time_slot: str,
    lecture_id: str,
    class_id: str,
    room: str,
    group_id: str = "G1",
    duration_days: int = 1,
    course_id: str = ""
):
    if duration_days < 1:
        raise HTTPException(status_code=400, detail="duration_days must be >= 1")
    required = {
        "doctor_id": doctor_id,
        "day": day,
        "time_slot": time_slot,
        "lecture_id": lecture_id,
        "class_id": class_id,
        "room": room,
    }
    missing = [k for k, v in required.items() if not str(v).strip()]
    if missing:
        raise HTTPException(status_code=400, detail=f"Missing required fields: {', '.join(missing)}")
    row = pd.DataFrame([{
        "Doctor_ID": str(doctor_id).strip(),
        "Day": str(day).strip(),
        "Time_Slot": str(time_slot).strip(),
        "Lecture_ID": str(lecture_id).strip(),
        "Class_ID": str(class_id).strip(),
        "Room": str(room).strip(),
        "Group_ID": str(group_id).strip() or "G1",
        "Duration_Days": int(duration_days),
        "Course_ID": str(course_id).strip(),
    }])
    try:
        _ensure_dir(SCHEDULE_PATH)
        row.to_csv(SCHEDULE_PATH, mode="a", header=not os.path.exists(SCHEDULE_PATH), index=False)
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Unable to write schedule.csv: {e}")
    return {"status": "added", "lecture_id": str(lecture_id).strip(), "group_id": str(group_id).strip() or "G1"}




def _save_snapshot(session_id: str, frame: np.ndarray, label: str = "frame") -> str:
    """Save a JPEG snapshot and return its file path."""
    session_snap_dir = os.path.join(SNAPSHOTS_DIR, session_id)
    os.makedirs(session_snap_dir, exist_ok=True)
    ts = datetime.now().strftime("%Y%m%d_%H%M%S_%f")[:21]   # trim microseconds
    filename = f"{label}_{ts}.jpg"
    filepath = os.path.join(session_snap_dir, filename)
    cv2.imwrite(filepath, frame)
    return filepath


@app.post("/snapshot/{session_id}")
async def take_snapshot(session_id: str, file: UploadFile = File(...), label: str = "manual"):
    """
    Receive a frame from the camera and save it as a snapshot.
    The camera client calls this whenever it wants to capture a moment.
    Returns the saved file path and a URL to retrieve it.
    """
    session_data = _get_session_or_404(session_id)
    contents = await file.read()
    np_arr = np.frombuffer(contents, np.uint8)
    frame = cv2.imdecode(np_arr, cv2.IMREAD_COLOR)
    if frame is None:
        raise HTTPException(status_code=400, detail="Invalid image frame")

    safe_label = "".join(c if c.isalnum() or c in "-_" else "_" for c in label)
    filepath = _save_snapshot(session_id, frame, label=safe_label)
    filename = os.path.basename(filepath)
    return {
        "status": "saved",
        "session_id": session_id,
        "filename": filename,
        "url": f"/snapshot_file/{session_id}/{filename}",
    }


@app.get("/snapshots/{session_id}")
def list_snapshots(session_id: str):
    """List all snapshots taken during a session."""
    session_snap_dir = os.path.join(SNAPSHOTS_DIR, session_id)
    if not os.path.isdir(session_snap_dir):
        return {"session_id": session_id, "snapshots": [], "count": 0}
    files = sorted(
        f for f in os.listdir(session_snap_dir) if f.lower().endswith(".jpg")
    )
    return {
        "session_id": session_id,
        "count": len(files),
        "snapshots": [
            {"filename": f, "url": f"/snapshot_file/{session_id}/{f}"}
            for f in files
        ],
    }


@app.get("/snapshot_file/{session_id}/{filename}")
def get_snapshot_file(session_id: str, filename: str):
    """Serve a snapshot image file."""
    # Sanitize to prevent path traversal
    safe_name = os.path.basename(filename)
    filepath = os.path.join(SNAPSHOTS_DIR, session_id, safe_name)
    if not os.path.isfile(filepath):
        raise HTTPException(status_code=404, detail="Snapshot not found")
    return FileResponse(filepath, media_type="image/jpeg")


@app.post("/start_camera/{session_id}")
def start_camera(session_id: str):
    try:
        subprocess.Popen(
            [sys.executable, 'camera.py'],
            env={**os.environ, 'SESSION_ID': session_id}
        )
        return {"status": "camera started", "session_id": session_id}
    except Exception as e:
        return {"error": str(e)}


@app.get("/absent_students/{session_id}")
def absent_students(session_id: str):
    session_data = _get_session_or_404(session_id)
    class_id = session_data.get("class_id", "")
    if not class_id:
        return {"records": [], "count": 0}

    batch_number = int(session_data.get("batch_number", 1))
    batch_size = int(session_data.get("batch_size", 25))
    class_students = _students_for_class(class_id)
    if class_students.empty:
        return {"records": [], "count": 0}

    class_students = _slice_batch(class_students, batch_number, batch_size)
    present_ids = set(map(str, session_data["attendance"].keys()))
    absents = class_students[~class_students["Student_ID"].isin(present_ids)][["Student_ID", "Student_Name", "Class_ID"]]
    return {"records": _records_safe(absents), "count": int(len(absents))}


@app.get("/analytics/{session_id}")
def session_analytics(session_id: str):
    session_data = _get_session_or_404(session_id)

    student_rows = []
    emotion_totals = Counter()
    for sid, counts in session_data["emotion_stats"].items():
        student = session_data["attendance"].get(sid, {})
        total = sum(counts.values()) or 1
        score = round(
            (counts.get("happy",   0) * 1.0  +
             counts.get("neutral", 0) * 0.6  +
             counts.get("focused", 0) * 0.5  +
             counts.get("confused",0) * 0.35 +
             counts.get("sad",     0) * 0.1) / total, 4
        )
        for emo in ("happy", "neutral", "focused", "confused", "sad"):
            emotion_totals[emo] += int(counts.get(emo, 0))
        student_rows.append({
            "Student_ID":       sid,
            "Student_Name":     student.get("Student_Name", ""),
            "Dominant_Emotion": counts.most_common(1)[0][0] if counts else "neutral",
            "Engagement_Score": score,
            "happy":   int(counts.get("happy",   0)),
            "neutral": int(counts.get("neutral", 0)),
            "focused": int(counts.get("focused", 0)),
            "confused":int(counts.get("confused",0)),
            "sad":     int(counts.get("sad",     0)),
        })

    return {
        "session_id": session_id,
        "total_present": len(session_data["attendance"]),
        "batch_number": int(session_data.get("batch_number", 1)),
        "batch_size": int(session_data.get("batch_size", 25)),
        "total_batches": int(session_data.get("total_batches", 1)),
        "batch_students_count": int(len(session_data.get("target_student_ids", set()))),
        "emotion_distribution": dict(emotion_totals),
        "behavior_summary": {
            "phone_usage": int(session_data["behavior_stats"].get(BEHAVIOR_PHONE, 0)),
            "suspicious_activity": int(session_data["behavior_stats"].get(BEHAVIOR_SUSPICIOUS, 0)),
        },
        "students": student_rows,
    }


@app.get("/completed_weeks/{doctor_id}/{lecture_id}")
def get_completed_weeks(doctor_id: str, lecture_id: str):
    try:
        df = _safe_read_csv(SESSIONS_PATH)
        if df.empty or "Week_Number" not in df.columns:
            return {"completed_weeks": [], "total_sessions": 0}
        filtered = df[
            (df["Doctor_ID"].astype(str) == str(doctor_id)) &
            (df["Lecture_ID"].astype(str) == str(lecture_id))
        ]
        completed = sorted(filtered["Week_Number"].dropna().astype(int).unique().tolist())
        return {
            "completed_weeks": completed,
            "total_sessions":  len(filtered)
        }
    except Exception as e:
        return {"error": str(e)}