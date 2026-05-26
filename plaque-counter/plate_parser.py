"""
Multi-well plate image parser.

Splits a full plate image into individual well images,
supporting 96-well and 24-well plate formats.
"""

import cv2
import numpy as np


class PlateParser:
    FORMATS = {
        "96-well": (8, 12),
        "24-well": (4, 6),
        "12-well": (3, 4),
        "6-well":  (2, 3),
    }

    def __init__(self, plate_format="96-well"):
        if plate_format not in self.FORMATS:
            raise ValueError(f"Unknown format: {plate_format}. Choose from {list(self.FORMATS.keys())}")
        self.rows, self.cols = self.FORMATS[plate_format]
        self.plate_format = plate_format

    def split_plate(self, image_bgr):
        """
        Split a full plate image into individual well images.
        Returns dict mapping well label (e.g. 'A1') to image.
        """
        h, w = image_bgr.shape[:2]
        well_h = h // self.rows
        well_w = w // self.cols

        wells = {}
        for r in range(self.rows):
            for c in range(self.cols):
                label = f"{chr(65 + r)}{c + 1}"
                y1 = r * well_h
                y2 = (r + 1) * well_h
                x1 = c * well_w
                x2 = (c + 1) * well_w
                wells[label] = image_bgr[y1:y2, x1:x2].copy()

        return wells

    def detect_wells_circular(self, image_bgr):
        """
        Detect circular wells using Hough Circle Transform.
        Falls back to grid-based splitting if detection fails.
        """
        gray = cv2.cvtColor(image_bgr, cv2.COLOR_BGR2GRAY)
        blurred = cv2.GaussianBlur(gray, (9, 9), 2)

        h, w = gray.shape
        min_r = min(h, w) // (max(self.rows, self.cols) * 3)
        max_r = min(h, w) // (max(self.rows, self.cols))

        circles = cv2.HoughCircles(
            blurred, cv2.HOUGH_GRADIENT, dp=1.2,
            minDist=min_r * 2,
            param1=100, param2=30,
            minRadius=min_r, maxRadius=max_r
        )

        if circles is None or len(circles[0]) < self.rows * self.cols * 0.5:
            return self.split_plate(image_bgr)

        circles = np.round(circles[0]).astype(int)
        circles = sorted(circles, key=lambda c: (c[1], c[0]))

        wells = {}
        for i, (cx, cy, r) in enumerate(circles[:self.rows * self.cols]):
            row = i // self.cols
            col = i % self.cols
            label = f"{chr(65 + row)}{col + 1}"

            pad = int(r * 1.1)
            y1 = max(0, cy - pad)
            y2 = min(h, cy + pad)
            x1 = max(0, cx - pad)
            x2 = min(w, cx + pad)
            wells[label] = image_bgr[y1:y2, x1:x2].copy()

        return wells

    @staticmethod
    def well_labels(rows, cols):
        labels = []
        for r in range(rows):
            for c in range(cols):
                labels.append(f"{chr(65 + r)}{c + 1}")
        return labels
