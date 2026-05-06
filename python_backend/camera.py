# camera.py
import cv2
import requests
import time

FASTAPI_URL = "http://127.0.0.1:8000"

# How long (seconds) to show the white flash overlay after a snapshot
FLASH_DURATION = 0.4


def start_camera_session(session_id: str):
    cap = cv2.VideoCapture(0)

    if not cap.isOpened():
        print("❌ Cannot open webcam")
        return

    print(f"Webcam started for session: {session_id}")
    print("Press S to take a snapshot  |  Press Q to stop")

    last_sent           = 0
    interval            = 3          # seconds between auto-process frames
    last_names          = []
    emotion_debug_lines = []
    tracked_faces       = []
    total               = 0
    flash_until         = 0.0        # timestamp until which the flash overlay is shown
    last_frame          = None       # keep a copy for manual snapshot

    while True:
        ret, frame = cap.read()
        if not ret:
            break

        last_frame = frame.copy()

        # ── Draw tracked face boxes with emotion labels ──────────────────────
        for person in tracked_faces:
            box    = person.get("bbox", {}) or {}
            left   = int(box.get("left",   0))
            top    = int(box.get("top",    0))
            right  = int(box.get("right",  0))
            bottom = int(box.get("bottom", 0))
            if right > left and bottom > top:
                emotion_label = str(person.get("emotion", "neutral")).upper()
                cv2.rectangle(frame, (left, top), (right, bottom), (0, 255, 0), 2)
                cv2.putText(
                    frame,
                    emotion_label,
                    (left, max(20, top - 8)),
                    cv2.FONT_HERSHEY_SIMPLEX,
                    0.7,
                    (0, 255, 0),
                    2,
                )

        # ── HUD info ─────────────────────────────────────────────────────────
        cv2.putText(frame, f"Session: {session_id}",
                    (10, 30), cv2.FONT_HERSHEY_SIMPLEX, 0.6, (0, 255, 0), 2)
        cv2.putText(frame, f"Present: {total}",
                    (10, 60), cv2.FONT_HERSHEY_SIMPLEX, 0.6, (0, 255, 0), 2)
        cv2.putText(frame, "S=Snapshot  Q=Quit",
                    (10, frame.shape[0] - 12),
                    cv2.FONT_HERSHEY_SIMPLEX, 0.45, (200, 200, 200), 1)

        # ── Recognized names ─────────────────────────────────────────────────
        for i, name in enumerate(last_names):
            cv2.putText(frame, f"OK {name}",
                        (10, 90 + i * 25),
                        cv2.FONT_HERSHEY_SIMPLEX, 0.5, (255, 255, 0), 1)

        # ── Emotion debug scores ──────────────────────────────────────────────
        debug_start_y = 90 + len(last_names) * 25 + 20
        cv2.putText(frame, "Emotion scores:",
                    (10, debug_start_y),
                    cv2.FONT_HERSHEY_SIMPLEX, 0.45, (0, 255, 255), 1)
        for i, line in enumerate(emotion_debug_lines):
            cv2.putText(frame, line,
                        (10, debug_start_y + 22 + i * 20),
                        cv2.FONT_HERSHEY_SIMPLEX, 0.45, (0, 255, 255), 1)

        # ── Snapshot flash overlay ────────────────────────────────────────────
        now = time.time()
        if now < flash_until:
            overlay = frame.copy()
            cv2.rectangle(overlay, (0, 0), (frame.shape[1], frame.shape[0]),
                          (255, 255, 255), -1)
            alpha = 0.45 * max(0.0, (flash_until - now) / FLASH_DURATION)
            cv2.addWeighted(overlay, alpha, frame, 1 - alpha, 0, frame)
            cv2.putText(frame, "SNAPSHOT SAVED",
                        (frame.shape[1] // 2 - 120, frame.shape[0] // 2),
                        cv2.FONT_HERSHEY_SIMPLEX, 1.0, (0, 0, 200), 3)

        cv2.imshow("Classroom Camera — S: Snapshot  Q: Quit", frame)

        # ── Auto-send frame every `interval` seconds ──────────────────────────
        if now - last_sent >= interval:
            try:
                _, buffer = cv2.imencode('.jpg', last_frame)
                jpg_bytes = buffer.tobytes()
                files = {'file': ('frame.jpg', jpg_bytes, 'image/jpeg')}

                res  = requests.post(
                    f"{FASTAPI_URL}/process_frame/{session_id}",
                    files=files, timeout=10
                )
                data            = res.json()
                recognized      = data.get('recognized', [])
                total           = data.get('total_present', 0)
                behavior_events = data.get('behavior_events', [])
                tracked_faces   = recognized
                last_names      = [
                    f"{s['name']} ({s.get('emotion', 'neutral')})"
                    for s in recognized
                ]
                emotion_debug_lines = []
                for s in recognized:
                    scores     = s.get("emotion_scores", {}) or {}
                    # Show ALL scores so we can see what DeepFace is outputting
                    all_scores = sorted(scores.items(), key=lambda x: x[1], reverse=True)
                    all_txt    = "  ".join(f"{k[:3]}={v:.0f}" for k, v in all_scores) if all_scores else "no-scores"
                    label      = s.get('emotion', 'neutral')
                    emotion_debug_lines.append(f"{s['name']}: [{label}]")
                    emotion_debug_lines.append(f"  {all_txt}")
                    print(f"  OK {s['name']} | label={label} | {all_txt}")

                if behavior_events:
                    for ev in behavior_events:
                        print(f"  ALERT {ev.get('label')} ({ev.get('confidence')})")
                print(f"  Total present: {total}")

                last_sent = now

            except Exception as e:
                print(f"  Error sending frame: {e}")

        # ── Key handling ──────────────────────────────────────────────────────
        key = cv2.waitKey(1) & 0xFF

        if key == ord('q'):
            print("Camera stopped")
            break

        elif key == ord('s') and last_frame is not None:
            # Manual snapshot — send current frame to /snapshot endpoint
            try:
                _, buffer = cv2.imencode('.jpg', last_frame)
                files = {'file': ('snapshot.jpg', buffer.tobytes(), 'image/jpeg')}
                res = requests.post(
                    f"{FASTAPI_URL}/snapshot/{session_id}",
                    files=files,
                    params={"label": "manual"},
                    timeout=10,
                )
                data = res.json()
                print(f"  Snapshot saved: {data.get('filename', '?')}")
                flash_until = time.time() + FLASH_DURATION
            except Exception as e:
                print(f"  Snapshot error: {e}")

    cap.release()
    cv2.destroyAllWindows()


if __name__ == "__main__":
    import os
    session_id = os.environ.get('SESSION_ID') or input("Enter session ID: ")
    start_camera_session(session_id)
