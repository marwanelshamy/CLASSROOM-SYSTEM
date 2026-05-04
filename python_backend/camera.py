# camera.py
import cv2
import requests
import time

FASTAPI_URL = "http://127.0.0.1:8000"

def start_camera_session(session_id: str):
    cap = cv2.VideoCapture(0)

    if not cap.isOpened():
        print("❌ Cannot open webcam")
        return

    print(f"✅ Webcam started for session: {session_id}")
    print("Press Q to stop")

    last_sent    = 0
    interval     = 3
    last_names   = []
    total        = 0

    while True:
        ret, frame = cap.read()
        if not ret:
            break

        # Draw info on frame
        cv2.putText(frame, f"Session: {session_id}",
                    (10, 30), cv2.FONT_HERSHEY_SIMPLEX,
                    0.6, (0, 255, 0), 2)
        cv2.putText(frame, f"Present: {total}",
                    (10, 60), cv2.FONT_HERSHEY_SIMPLEX,
                    0.6, (0, 255, 0), 2)

        # Show last recognized names
        for i, name in enumerate(last_names):
            cv2.putText(frame, f"✅ {name}",
                        (10, 90 + i * 25),
                        cv2.FONT_HERSHEY_SIMPLEX,
                        0.5, (255, 255, 0), 1)

        cv2.imshow("Classroom Camera — Press Q to quit", frame)

        # Send frame every 3 seconds
        current_time = time.time()
        if current_time - last_sent >= interval:
            try:
                _, buffer = cv2.imencode('.jpg', frame)
                files = {'file': ('frame.jpg', buffer.tobytes(), 'image/jpeg')}

                res  = requests.post(
                    f"{FASTAPI_URL}/process_frame/{session_id}",
                    files   = files,
                    timeout = 10
                )
                data       = res.json()
                recognized = data.get('recognized', [])
                total      = data.get('total_present', 0)
                behavior_events = data.get('behavior_events', [])
                last_names = [f"{s['name']} ({s.get('emotion', 'neutral')})" for s in recognized]

                if recognized:
                    for s in recognized:
                        print(f"  ✅ {s['name']} | emotion={s.get('emotion', 'neutral')}")
                if behavior_events:
                    for ev in behavior_events:
                        print(f"  🚨 {ev.get('label')} ({ev.get('confidence')})")
                print(f"  👥 Total present: {total}")

                last_sent = current_time

            except Exception as e:
                print(f"  ❌ Error: {e}")

        if cv2.waitKey(1) & 0xFF == ord('q'):
            print("🛑 Camera stopped")
            break

    cap.release()
    cv2.destroyAllWindows()

if __name__ == "__main__":
    import os
    session_id = os.environ.get('SESSION_ID') or input("Enter session ID: ")
    start_camera_session(session_id)