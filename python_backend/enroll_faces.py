# enroll_faces.py — Run this ONCE before anything else

import pandas as pd
import requests
import face_recognition
import pickle
import os
import re

# ── Paths ──────────────────────────────────────────
CSV_PATH      = '../data/StudentPicsDataset.csv'
PHOTOS_DIR    = 'student_photos/'
ENCODINGS_OUT = 'data/face_encodings.pkl'

os.makedirs(PHOTOS_DIR, exist_ok=True)
os.makedirs('data', exist_ok=True)

df = pd.read_csv(CSV_PATH)
df['Student ID'] = df['Student ID'].astype(str).str.split('.').str[0]

# ── Helper: Extract Google Drive File ID ────────────
def extract_file_id(url):
    url = str(url).strip()
    # Format 1: /open?id=XXXX  or  ?id=XXXX
    if 'id=' in url:
        return url.split('id=')[1].split('&')[0]
    # Format 2: /file/d/XXXX/view
    match = re.search(r'/d/([a-zA-Z0-9_-]+)', url)
    if match:
        return match.group(1)
    return None

# ── Step 1: Download Photos ─────────────────────────
print("=== Downloading Photos ===")
for _, row in df.iterrows():
    sid      = row['Student ID']
    img_path = f"{PHOTOS_DIR}{sid}.jpg"

    if os.path.exists(img_path):
        print(f"  Already exists: {sid}")
        continue

    file_id = extract_file_id(row['Photo Link'])
    if not file_id:
        print(f"  ❌ Can't parse URL for: {sid} → {row['Photo Link']}")
        continue

    url = f"https://drive.google.com/uc?export=download&id={file_id}"
    try:
        r = requests.get(url, timeout=15)
        # Check if we got an actual image not an HTML error page
        if 'text/html' in r.headers.get('Content-Type', ''):
            print(f"  ❌ Got HTML instead of image (private file?): {sid}")
            continue
        with open(img_path, 'wb') as f:
            f.write(r.content)
        print(f"  ✅ Downloaded: {sid}")
    except Exception as e:
        print(f"  ❌ Failed: {sid} → {e}")

# ── Step 2: Encode Faces ────────────────────────────
print("\n=== Encoding Faces ===")
known_encodings = []
known_ids       = []
known_names     = []

for _, row in df.iterrows():
    sid      = row['Student ID']
    name     = row['Student Name']
    img_path = f"{PHOTOS_DIR}{sid}.jpg"

    if not os.path.exists(img_path):
        print(f"  ⚠️  Missing photo: {sid} — {name}")
        continue

    try:
        image     = face_recognition.load_image_file(img_path)
        encodings = face_recognition.face_encodings(image)

        if encodings:
            known_encodings.append(encodings[0])
            known_ids.append(sid)
            known_names.append(name)
            print(f"  ✅ Encoded: {name}")
        else:
            print(f"  ❌ No face detected in photo: {name}")

    except Exception as e:
        print(f"  ❌ Error encoding: {name} → {e}")

# ── Step 3: Save Encodings ──────────────────────────
if known_encodings:
    with open(ENCODINGS_OUT, 'wb') as f:
        pickle.dump({
            'encodings': known_encodings,
            'ids':       known_ids,
            'names':     known_names
        }, f)
    print(f"\n✅ Done! Enrolled {len(known_ids)}/{len(df)} students")
    print(f"   Saved to: {ENCODINGS_OUT}")
else:
    print("\n❌ No faces were encoded — check your photos and CSV paths")