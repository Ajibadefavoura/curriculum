"""
Core plaque detection engine.

Improvements over Viridot:
- Adaptive thresholding with multiple strategies
- Watershed segmentation for overlapping plaques
- Multi-scale Gaussian blur for noise reduction
- Automatic well detection and masking
- Color-space-aware processing (LAB, HSV, grayscale)
- Morphological refinement pipeline
- Per-plaque confidence scoring
"""

import cv2
import numpy as np
from scipy import ndimage
from skimage import morphology, measure, segmentation, feature


class PlaqueDetector:
    DEFAULT_PARAMS = {
        "color_channel": "auto",
        "blur_sigma": 2.0,
        "threshold_method": "adaptive",
        "threshold_block_size": 35,
        "threshold_c": 10,
        "min_plaque_area": 30,
        "max_plaque_area": 5000,
        "min_circularity": 0.3,
        "well_trim_px": 15,
        "watershed_enabled": True,
        "watershed_min_distance": 10,
        "contrast_alpha": 1.5,
        "contrast_beta": 0,
        "morphology_iterations": 1,
        "invert": True,
    }

    def __init__(self, params=None):
        self.params = dict(self.DEFAULT_PARAMS)
        if params:
            self.params.update(params)

    def detect(self, image_bgr):
        gray = self._to_grayscale(image_bgr)
        well_mask = self._detect_well_mask(gray)
        preprocessed = self._preprocess(gray)
        binary = self._threshold(preprocessed)
        binary = cv2.bitwise_and(binary, binary, mask=well_mask)
        refined = self._morphological_refinement(binary)

        if self.params["watershed_enabled"]:
            labels = self._watershed_segment(refined)
        else:
            labels, _ = ndimage.label(refined)

        plaques = self._extract_plaques(labels, gray, image_bgr)
        return plaques

    def detect_with_steps(self, image_bgr):
        """Return intermediate images for each processing step (for UI debugging)."""
        steps = {}
        steps["original"] = image_bgr.copy()

        gray = self._to_grayscale(image_bgr)
        steps["grayscale"] = cv2.cvtColor(gray, cv2.COLOR_GRAY2BGR)

        well_mask = self._detect_well_mask(gray)
        mask_vis = cv2.cvtColor(well_mask, cv2.COLOR_GRAY2BGR)
        steps["well_mask"] = mask_vis

        preprocessed = self._preprocess(gray)
        steps["preprocessed"] = cv2.cvtColor(preprocessed, cv2.COLOR_GRAY2BGR)

        binary = self._threshold(preprocessed)
        binary = cv2.bitwise_and(binary, binary, mask=well_mask)
        steps["thresholded"] = cv2.cvtColor(binary, cv2.COLOR_GRAY2BGR)

        refined = self._morphological_refinement(binary)
        steps["refined"] = cv2.cvtColor(refined, cv2.COLOR_GRAY2BGR)

        if self.params["watershed_enabled"]:
            labels = self._watershed_segment(refined)
        else:
            labels, _ = ndimage.label(refined)

        plaques = self._extract_plaques(labels, gray, image_bgr)

        annotated = self._draw_annotations(image_bgr, plaques)
        steps["annotated"] = annotated

        return plaques, steps

    def _to_grayscale(self, image_bgr):
        channel = self.params["color_channel"]
        if channel == "auto":
            channel = self._auto_select_channel(image_bgr)

        if channel == "gray":
            return cv2.cvtColor(image_bgr, cv2.COLOR_BGR2GRAY)
        elif channel == "red":
            return image_bgr[:, :, 2]
        elif channel == "green":
            return image_bgr[:, :, 1]
        elif channel == "blue":
            return image_bgr[:, :, 0]
        elif channel == "hue":
            hsv = cv2.cvtColor(image_bgr, cv2.COLOR_BGR2HSV)
            return hsv[:, :, 0]
        elif channel == "saturation":
            hsv = cv2.cvtColor(image_bgr, cv2.COLOR_BGR2HSV)
            return hsv[:, :, 1]
        elif channel == "value":
            hsv = cv2.cvtColor(image_bgr, cv2.COLOR_BGR2HSV)
            return hsv[:, :, 2]
        elif channel == "lab_l":
            lab = cv2.cvtColor(image_bgr, cv2.COLOR_BGR2LAB)
            return lab[:, :, 0]
        else:
            return cv2.cvtColor(image_bgr, cv2.COLOR_BGR2GRAY)

    def _auto_select_channel(self, image_bgr):
        channels = {
            "gray": cv2.cvtColor(image_bgr, cv2.COLOR_BGR2GRAY),
            "blue": image_bgr[:, :, 0],
            "green": image_bgr[:, :, 1],
            "red": image_bgr[:, :, 2],
        }
        best_channel = "gray"
        best_std = 0
        for name, ch in channels.items():
            std = np.std(ch.astype(np.float64))
            if std > best_std:
                best_std = std
                best_channel = name
        return best_channel

    def _detect_well_mask(self, gray):
        h, w = gray.shape
        mask = np.zeros((h, w), dtype=np.uint8)
        trim = self.params["well_trim_px"]
        center = (w // 2, h // 2)
        radius = min(h, w) // 2 - trim
        if radius < 10:
            radius = min(h, w) // 2 - 2
        cv2.circle(mask, center, max(radius, 10), 255, -1)
        return mask

    def _preprocess(self, gray):
        alpha = self.params["contrast_alpha"]
        beta = self.params["contrast_beta"]
        enhanced = cv2.convertScaleAbs(gray, alpha=alpha, beta=beta)

        sigma = self.params["blur_sigma"]
        ksize = int(sigma * 6) | 1
        blurred = cv2.GaussianBlur(enhanced, (ksize, ksize), sigma)
        return blurred

    def _threshold(self, gray):
        method = self.params["threshold_method"]
        if method == "adaptive":
            block = self.params["threshold_block_size"]
            if block % 2 == 0:
                block += 1
            block = max(block, 3)
            c = self.params["threshold_c"]
            binary = cv2.adaptiveThreshold(
                gray, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C,
                cv2.THRESH_BINARY_INV if self.params["invert"] else cv2.THRESH_BINARY,
                block, c
            )
        elif method == "otsu":
            _, binary = cv2.threshold(
                gray, 0, 255,
                (cv2.THRESH_BINARY_INV if self.params["invert"] else cv2.THRESH_BINARY) | cv2.THRESH_OTSU
            )
        else:
            _, binary = cv2.threshold(
                gray, 127, 255,
                cv2.THRESH_BINARY_INV if self.params["invert"] else cv2.THRESH_BINARY
            )
        return binary

    def _morphological_refinement(self, binary):
        iterations = self.params["morphology_iterations"]
        kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (3, 3))
        opened = cv2.morphologyEx(binary, cv2.MORPH_OPEN, kernel, iterations=iterations)
        closed = cv2.morphologyEx(opened, cv2.MORPH_CLOSE, kernel, iterations=iterations)
        return closed

    def _watershed_segment(self, binary):
        dist_transform = ndimage.distance_transform_edt(binary)
        min_dist = self.params["watershed_min_distance"]
        coords = feature.peak_local_max(
            dist_transform, min_distance=min_dist, labels=binary
        )
        mask = np.zeros(dist_transform.shape, dtype=bool)
        mask[tuple(coords.T)] = True
        markers, _ = ndimage.label(mask)
        labels = segmentation.watershed(-dist_transform, markers, mask=binary)
        return labels

    def _extract_plaques(self, labels, gray, image_bgr):
        props = measure.regionprops(labels, intensity_image=gray)
        plaques = []
        min_area = self.params["min_plaque_area"]
        max_area = self.params["max_plaque_area"]
        min_circ = self.params["min_circularity"]

        for prop in props:
            area = prop.area
            if area < min_area or area > max_area:
                continue

            perimeter = prop.perimeter
            if perimeter == 0:
                continue
            circularity = 4 * np.pi * area / (perimeter ** 2)
            if circularity < min_circ:
                continue

            cy, cx = prop.centroid
            radius = np.sqrt(area / np.pi)

            confidence = min(1.0, circularity * 0.7 + 0.3 * min(1.0, area / 200))

            plaques.append({
                "id": len(plaques) + 1,
                "x": float(cx),
                "y": float(cy),
                "radius": float(radius),
                "area": int(area),
                "circularity": float(circularity),
                "mean_intensity": float(prop.mean_intensity),
                "confidence": float(confidence),
                "bbox": [int(v) for v in prop.bbox],
            })

        return plaques

    def _draw_annotations(self, image_bgr, plaques):
        annotated = image_bgr.copy()
        for p in plaques:
            cx, cy = int(p["x"]), int(p["y"])
            r = max(int(p["radius"]) + 2, 4)
            conf = p["confidence"]
            if conf >= 0.7:
                color = (0, 255, 0)
            elif conf >= 0.4:
                color = (0, 200, 255)
            else:
                color = (0, 100, 255)
            cv2.circle(annotated, (cx, cy), r, color, 2)
            label = str(p["id"])
            cv2.putText(annotated, label, (cx + r + 2, cy - 2),
                        cv2.FONT_HERSHEY_SIMPLEX, 0.35, color, 1)
        return annotated

    def auto_detect_params(self, image_bgr):
        """Analyze image to automatically determine optimal parameters."""
        gray = cv2.cvtColor(image_bgr, cv2.COLOR_BGR2GRAY)
        h, w = gray.shape

        mean_val = np.mean(gray)
        std_val = np.std(gray)

        params = dict(self.DEFAULT_PARAMS)

        if std_val < 20:
            params["contrast_alpha"] = 2.5
        elif std_val < 40:
            params["contrast_alpha"] = 1.8
        else:
            params["contrast_alpha"] = 1.2

        pixel_count = h * w
        if pixel_count < 100000:
            params["min_plaque_area"] = 15
            params["max_plaque_area"] = 2000
            params["blur_sigma"] = 1.5
            params["threshold_block_size"] = 21
        elif pixel_count < 500000:
            params["min_plaque_area"] = 30
            params["max_plaque_area"] = 5000
            params["blur_sigma"] = 2.0
            params["threshold_block_size"] = 35
        else:
            params["min_plaque_area"] = 80
            params["max_plaque_area"] = 15000
            params["blur_sigma"] = 3.0
            params["threshold_block_size"] = 51

        if mean_val > 180:
            params["invert"] = True
        elif mean_val < 80:
            params["invert"] = False

        params["color_channel"] = self._auto_select_channel(image_bgr)

        return params
