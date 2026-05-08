from __future__ import annotations

from typing import Any, Dict, Tuple

import cv2
import numpy as np

try:
    from deepface import DeepFace  # type: ignore
except Exception:
    DeepFace = None  # type: ignore

_SMILE_CASCADE = cv2.CascadeClassifier(
    cv2.data.haarcascades + "haarcascade_smile.xml"
)
_EYE_CASCADE = cv2.CascadeClassifier(
    cv2.data.haarcascades + "haarcascade_eye.xml"
)

# ── Classroom labels ──────────────────────────────────────────────────────────
#  happy    -> smiling, positive engagement
#  neutral  -> resting, paying attention
#  focused  -> furrowed brows, concentration (maps from angry/disgust)
#  confused -> raised brows, uncertainty   (maps from surprise/fear)
#  sad      -> visibly sad or disengaged


def _to_float(value: Any) -> float:
    try:
        return float(value)
    except Exception:
        return 0.0


# ─────────────────────────────────────────────────────────────────────────────
# Geometry helpers
# ─────────────────────────────────────────────────────────────────────────────

def _smile_detected(face_crop: np.ndarray) -> bool:
    """Return True only when a real smile is physically visible."""
    if face_crop is None or face_crop.size == 0:
        return False
    try:
        gray = cv2.cvtColor(face_crop, cv2.COLOR_BGR2GRAY)
        h, w = gray.shape[:2]
        mouth_roi = gray[int(h * 0.50):h, int(w * 0.10):int(w * 0.90)]
        smiles = _SMILE_CASCADE.detectMultiScale(
            mouth_roi,
            scaleFactor=1.4,
            minNeighbors=20,
            minSize=(max(25, w // 7), max(12, h // 12)),
        )
        return len(smiles) > 0
    except Exception:
        return False


def _brow_furrow(face_crop: np.ndarray) -> float:
    """
    Laplacian variance in the brow region.
    High value = lots of texture = furrowed / tense brows (focused/angry look).
    Returns 0.0 – 1.0.
    """
    if face_crop is None or face_crop.size == 0:
        return 0.0
    try:
        gray = cv2.cvtColor(face_crop, cv2.COLOR_BGR2GRAY)
        h, w = gray.shape[:2]
        brow = gray[int(h * 0.10):int(h * 0.42), int(w * 0.15):int(w * 0.85)]
        if brow.size == 0:
            return 0.0
        return float(min(cv2.Laplacian(brow, cv2.CV_64F).var() / 500.0, 1.0))
    except Exception:
        return 0.0


def _mouth_downturn(face_crop: np.ndarray) -> float:
    """
    Estimate how much the mouth corners are turned down.
    We compare the brightness of the left/right mouth corners vs the centre.
    A darker centre relative to corners suggests a downturned (sad) mouth.
    Returns 0.0 – 1.0.
    """
    if face_crop is None or face_crop.size == 0:
        return 0.0
    try:
        gray = cv2.cvtColor(face_crop, cv2.COLOR_BGR2GRAY)
        h, w = gray.shape[:2]
        # Lower face strip
        mouth_strip = gray[int(h * 0.62):int(h * 0.82), :]
        if mouth_strip.size == 0:
            return 0.0
        mh, mw = mouth_strip.shape
        left_mean   = float(np.mean(mouth_strip[:, :mw // 4]))
        right_mean  = float(np.mean(mouth_strip[:, 3 * mw // 4:]))
        centre_mean = float(np.mean(mouth_strip[:, mw // 4: 3 * mw // 4]))
        corners_avg = (left_mean + right_mean) / 2.0
        # If corners are brighter than centre → mouth curves down
        downturn = max(0.0, (corners_avg - centre_mean) / 60.0)
        return float(min(downturn, 1.0))
    except Exception:
        return 0.0


# ─────────────────────────────────────────────────────────────────────────────
# Core decision logic
# ─────────────────────────────────────────────────────────────────────────────

def _classify(scores: Dict[str, float], face_crop: np.ndarray) -> str:
    h  = scores.get("happy",    0.0)
    n  = scores.get("neutral",  0.0)
    s  = scores.get("sad",      0.0)
    an = scores.get("angry",    0.0)
    fe = scores.get("fear",     0.0)
    di = scores.get("disgust",  0.0)
    su = scores.get("surprise", 0.0)

    smile  = _smile_detected(face_crop)
    furrow = _brow_furrow(face_crop)   # 0.0 – 1.0

    # ── HAPPY ─────────────────────────────────────────────────────────────────
    if smile and h >= 35.0:
        return "happy"
    if smile and h >= 20.0 and h > n:
        return "happy"

    # ── If neutral is very dominant (>= 55%) → trust it unless geometry says otherwise
    if n >= 55.0:
        # Geometry override: strong brow furrow = focused even when neutral dominates
        if furrow >= 0.50:
            return "focused"
        return "neutral"

    # ── Use RELATIVE ranking of non-neutral emotions ──────────────────────────
    # Map DeepFace labels to classroom labels
    # focused  ← angry, disgust  (+ brow furrow geometry)
    # confused ← surprise, fear
    # sad      ← sad
    # happy    ← happy (with smile)

    focused_score  = an + di + (furrow * 30.0)
    confused_score = su + fe
    sad_score      = s
    happy_score    = h if smile else h * 0.3   # penalise happy without smile

    best = max(focused_score, confused_score, sad_score, happy_score)

    # Only override neutral if the best non-neutral signal is meaningful
    if best < 5.0:
        return "neutral"

    if best == happy_score and smile:
        return "happy"
    if best == focused_score:
        return "focused"
    if best == confused_score:
        return "confused"
    if best == sad_score:
        return "sad"

    return "neutral"

    # ── NEUTRAL fallback ──────────────────────────────────────────────────────
    return "neutral"


# ─────────────────────────────────────────────────────────────────────────────
# Fallback when DeepFace is not installed
# ─────────────────────────────────────────────────────────────────────────────

def _base_scores() -> Dict[str, float]:
    return {
        "happy": 0.0, "neutral": 0.0, "sad": 0.0,
        "angry": 0.0, "surprise": 0.0, "fear": 0.0, "disgust": 0.0,
    }


def _geometry_only(face_crop: np.ndarray) -> Tuple[str, Dict[str, float]]:
    """Pure OpenCV fallback — no DeepFace."""
    scores = _base_scores()
    try:
        smile   = _smile_detected(face_crop)
        furrow  = _brow_furrow(face_crop)
        downturn = _mouth_downturn(face_crop)

        gray = cv2.cvtColor(face_crop, cv2.COLOR_BGR2GRAY)
        h, w = gray.shape[:2]
        upper = gray[0:int(h * 0.55), :]
        eyes  = _EYE_CASCADE.detectMultiScale(
            upper, scaleFactor=1.1, minNeighbors=5,
            minSize=(max(18, w // 10), max(12, h // 14)),
        )

        if smile:
            scores["happy"] = 80.0
            scores["neutral"] = 15.0
            return "happy", scores

        if len(eyes) == 0:
            scores["sad"] = 60.0
            scores["neutral"] = 25.0
            return "sad", scores

        if furrow > 0.55:
            scores["angry"] = 55.0
            scores["neutral"] = 30.0
            return "focused", scores

        if downturn > 0.45:
            scores["sad"] = 50.0
            scores["neutral"] = 35.0
            return "sad", scores

        scores["neutral"] = 70.0
        return "neutral", scores

    except Exception:
        scores["neutral"] = 80.0
        return "neutral", scores


# ─────────────────────────────────────────────────────────────────────────────
# Public API
# ─────────────────────────────────────────────────────────────────────────────

def detect_emotion_details(face_crop: np.ndarray) -> Tuple[str, Dict[str, float]]:
    """Return (classroom_label, raw_deepface_scores)."""
    if face_crop is None or face_crop.size == 0:
        return "neutral", {}

    if DeepFace is None:
        return _geometry_only(face_crop)

    try:
        rgb = cv2.cvtColor(face_crop, cv2.COLOR_BGR2RGB)
        analysis: Any = DeepFace.analyze(
            rgb,
            actions=["emotion"],
            enforce_detection=False,
            detector_backend="opencv",
            silent=True,
        )
        result   = analysis[0] if isinstance(analysis, list) else analysis
        emotions = result.get("emotion", {}) or {}

        scores = {
            "happy":    round(_to_float(emotions.get("happy",    0.0)), 2),
            "neutral":  round(_to_float(emotions.get("neutral",  0.0)), 2),
            "sad":      round(_to_float(emotions.get("sad",      0.0)), 2),
            "angry":    round(_to_float(emotions.get("angry",    0.0)), 2),
            "surprise": round(_to_float(emotions.get("surprise", 0.0)), 2),
            "fear":     round(_to_float(emotions.get("fear",     0.0)), 2),
            "disgust":  round(_to_float(emotions.get("disgust",  0.0)), 2),
        }

        # DEBUG — print raw scores so we can tune thresholds
        print(f"[EMOTION] raw={scores} furrow={round(_brow_furrow(face_crop),3)}")

        label = _classify(scores, face_crop)
        print(f"[EMOTION] → {label}")
        return label, scores

    except Exception:
        return _geometry_only(face_crop)


def detect_emotion(face_crop: np.ndarray) -> str:
    """Backward-compatible single-label helper."""
    label, _ = detect_emotion_details(face_crop)
    return label
