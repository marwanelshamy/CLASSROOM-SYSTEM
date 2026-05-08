# camera.py
import cv2
import requests
import time
import threading

FASTAPI_URL   = "http://127.0.0.1:8000"
FLASH_DURATION = 0.4
SEND_INTERVAL  = 2   # seconds between frames sent to backend


def start_camera_session(session_id: str):
    cap = cv2.VideoCapture(0)
    cap.set(cv2.CAP_PROP_FRAME_WIDTH,  1280)
    cap.set(cv2.CAP_PROP_FRAME_HEIGHT, 720)

    if not cap.isOpened():
        print("Cannot open webcam")
        return

    print(f"Webcam started for session: {session_id}")
    print("Press S to take a snapshot  |  Press Q to stop")

    last_sent    = 0
    tracked_faces = []
    total        = 0
    flash_until  = 0.0
    lock         = threading.Lock()

    # ── Background thread: send frame to backend ─────────────────────────────
    def send_frame_bg(frame_bytes):
        nonlocal tracked_faces, total
        try:
            files = {'file': ('frame.jpg', frame_bytes, 'image/jpeg')}
            res   = requests.post(
                f"{FASTAPI_URL}/process_frame/{session_id}",
                files=files, timeout=10
            )
            data = res.json()
            recognized = data.get('recognized', [])
            new_total  = data.get('total_present', 0)
            with lock:
                tracked_faces = recognized
                total         = new_total
            for s in recognized:
                scores    = s.get("emotion_scores", {}) or {}
                top3      = sorted(scores.items(), key=lambda x: x[1], reverse=True)[:3]
                top_txt   = "  ".join(f"{k[:3]}={v:.0f}" for k, v in top3)
                print(f"  {s['id']} {s['name']} | {s.get('emotion','neutral')} | {top_txt}")
            print(f"  Total present: {new_total}")
        except Exception as e:
            print(f"  Frame error: {e}")

    while True:
        ret, frame = cap.read()
        if not ret:
            break

        now = time.time()

        # ── Draw face boxes ───────────────────────────────────────────────────
        with lock:
            faces_to_draw = list(tracked_faces)

        for person in faces_to_draw:
            box    = person.get("bbox", {}) or {}
            left   = int(box.get("left",   0))
            top    = int(box.get("top",    0))
            right  = int(box.get("right",  0))
            bottom = int(box.get("bottom", 0))

            if right <= left or bottom <= top:
                continue

            emotion = str(person.get("emotion", "neutral")).upper()
            name    = str(person.get("name",    ""))
            sid     = str(person.get("id",      ""))

            # Green bounding box
            cv2.rectangle(frame, (left, top), (right, bottom), (0, 255, 0), 2)

            # Corner accent marks
            cs = max(10, min(right - left, bottom - top) // 6)
            for (x1, y1, x2, y2) in [
                (left,  top,    left+cs, top),    (left,  top,    left,  top+cs),
                (right, top,    right-cs,top),    (right, top,    right, top+cs),
                (left,  bottom, left+cs, bottom), (left,  bottom, left,  bottom-cs),
                (right, bottom, right-cs,bottom), (right, bottom, right, bottom-cs),
            ]:
                cv2.line(frame, (x1, y1), (x2, y2), (0, 255, 0), 3)

            # Label: ID + Name
            label_top = f"{sid}  {name}"
            label_bot = emotion

            # Top label (above box)
            ly = max(22, top - 8)
            (tw, th), _ = cv2.getTextSize(label_top, cv2.FONT_HERSHEY_SIMPLEX, 0.55, 1)
            cv2.rectangle(frame, (left, ly - th - 4), (left + tw + 6, ly + 2),
                          (0, 0, 0), -1)
            cv2.putText(frame, label_top, (left + 3, ly),
                        cv2.FONT_HERSHEY_SIMPLEX, 0.55, (0, 255, 0), 1)

            # Bottom label (emotion, inside box at bottom)
            ey = bottom - 8
            (ew, eh), _ = cv2.getTextSize(label_bot, cv2.FONT_HERSHEY_SIMPLEX, 0.6, 2)
            cv2.rectangle(frame, (left, ey - eh - 4), (left + ew + 6, ey + 4),
                          (0, 0, 0), -1)
            cv2.putText(frame, label_bot, (left + 3, ey),
                        cv2.FONT_HERSHEY_SIMPLEX, 0.6, (0, 255, 0), 2)

        # ── HUD ───────────────────────────────────────────────────────────────
        cv2.putText(frame, f"Session: {session_id}",
                    (10, 28), cv2.FONT_HERSHEY_SIMPLEX, 0.55, (0, 255, 0), 1)
        cv2.putText(frame, f"Present: {total}",
                    (10, 52), cv2.FONT_HERSHEY_SIMPLEX, 0.55, (0, 255, 0), 1)
        cv2.putText(frame, "S=Snapshot  Q=Quit",
                    (10, frame.shape[0] - 10),
                    cv2.FONT_HERSHEY_SIMPLEX, 0.4, (180, 180, 180), 1)

        # ── Snapshot flash ────────────────────────────────────────────────────
        if now < flash_until:
            overlay = frame.copy()
            cv2.rectangle(overlay, (0, 0), (frame.shape[1], frame.shape[0]),
                          (255, 255, 255), -1)
            alpha = 0.5 * max(0.0, (flash_until - now) / FLASH_DURATION)
            cv2.addWeighted(overlay, alpha, frame, 1 - alpha, 0, frame)
            cv2.putText(frame, "SNAPSHOT SAVED",
                        (frame.shape[1] // 2 - 130, frame.shape[0] // 2),
                        cv2.FONT_HERSHEY_SIMPLEX, 1.1, (0, 0, 200), 3)

        cv2.imshow("EduSense Camera  |  S: Snapshot   Q: Quit", frame)

        # ── Send frame to backend every SEND_INTERVAL seconds ─────────────────
        if now - last_sent >= SEND_INTERVAL:
            _, buffer = cv2.imencode('.jpg', frame, [cv2.IMWRITE_JPEG_QUALITY, 90])
            t = threading.Thread(target=send_frame_bg,
                                 args=(buffer.tobytes(),), daemon=True)
            t.start()
            last_sent = now

        # ── Key handling ──────────────────────────────────────────────────────
        key = cv2.waitKey(1) & 0xFF

        if key == ord('q'):
            print("Camera stopped")
            break

        elif key == ord('s'):
            try:
                _, buffer = cv2.imencode('.jpg', frame,
                                         [cv2.IMWRITE_JPEG_QUALITY, 95])
                files = {'file': ('snapshot.jpg', buffer.tobytes(), 'image/jpeg')}
                res   = requests.post(
                    f"{FASTAPI_URL}/snapshot/{session_id}",
                    files=files, params={"label": "manual"}, timeout=10,
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
