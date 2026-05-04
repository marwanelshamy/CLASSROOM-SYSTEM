from __future__ import annotations

from typing import List, Tuple

import numpy as np

try:
    from ultralytics import YOLO  # type: ignore
except Exception:
    YOLO = None  # type: ignore


BEHAVIOR_PHONE = "phone_usage"
BEHAVIOR_SUSPICIOUS = "suspicious_activity"
SUSPICIOUS_OBJECTS = {"knife", "scissors"}


class YoloBehaviorDetector:
    def __init__(self, model_name: str = "yolov8n.pt", conf: float = 0.45):
        self.conf = conf
        self.model = None
        if YOLO is not None:
            try:
                self.model = YOLO(model_name)
            except Exception:
                self.model = None

    def detect_behaviors(self, frame: np.ndarray) -> List[Tuple[str, float]]:
        """Return list of tuples: (behavior_label, confidence)."""
        if self.model is None:
            return []

        try:
            detections = self.model.predict(frame, conf=self.conf, verbose=False)
            if not detections:
                return []

            result = detections[0]
            names = result.names
            behaviors: List[Tuple[str, float]] = []
            boxes = getattr(result, "boxes", None)
            if boxes is None:
                return []

            for box in boxes:
                cls_id = int(box.cls.item())
                conf = float(box.conf.item())
                label = names.get(cls_id, "")
                if label == "cell phone":
                    behaviors.append((BEHAVIOR_PHONE, conf))
                elif label in SUSPICIOUS_OBJECTS:
                    behaviors.append((BEHAVIOR_SUSPICIOUS, conf))
            return behaviors
        except Exception:
            return []

