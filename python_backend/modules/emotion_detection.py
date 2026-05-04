from __future__ import annotations

from typing import Any

import numpy as np

try:
    from deepface import DeepFace  # type: ignore
except Exception:
    DeepFace = None  # type: ignore


EMOTION_MAP = {
    "happy": "happy",
    "surprise": "happy",
    "neutral": "neutral",
    "sad": "bored",
    "angry": "bored",
    "fear": "confused",
    "disgust": "confused",
}


def detect_emotion(face_crop: np.ndarray) -> str:
    """Return one of: happy / bored / confused / neutral."""
    if face_crop is None or face_crop.size == 0:
        return "neutral"

    # If DeepFace is unavailable, keep server running with neutral fallback.
    if DeepFace is None:
        return "neutral"

    try:
        analysis: Any = DeepFace.analyze(
            face_crop,
            actions=["emotion"],
            enforce_detection=False,
            detector_backend="opencv",
            silent=True,
        )
        result = analysis[0] if isinstance(analysis, list) else analysis
        dominant = str(result.get("dominant_emotion", "neutral")).lower()
        return EMOTION_MAP.get(dominant, "neutral")
    except Exception:
        return "neutral"

