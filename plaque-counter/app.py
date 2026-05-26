"""
PlaqueScope - Modern Virus Plaque Counter

A modern, web-based alternative to Viridot for automated virus plaque counting
and neutralizing antibody titer estimation.
"""

import os
import io
import json
import uuid
import csv
import base64
from pathlib import Path

import cv2
import numpy as np
from flask import (
    Flask, render_template, request, jsonify, send_file, send_from_directory
)
from PIL import Image

from plaque_detector import PlaqueDetector
from titer_estimator import TiterEstimator
from plate_parser import PlateParser

app = Flask(__name__)
app.config["MAX_CONTENT_LENGTH"] = 50 * 1024 * 1024  # 50 MB
app.config["UPLOAD_FOLDER"] = os.path.join(os.path.dirname(__file__), "uploads")
app.config["RESULTS_FOLDER"] = os.path.join(os.path.dirname(__file__), "results")

os.makedirs(app.config["UPLOAD_FOLDER"], exist_ok=True)
os.makedirs(app.config["RESULTS_FOLDER"], exist_ok=True)

ALLOWED_EXTENSIONS = {"png", "jpg", "jpeg", "tif", "tiff", "bmp"}


def allowed_file(filename):
    return "." in filename and filename.rsplit(".", 1)[1].lower() in ALLOWED_EXTENSIONS


def numpy_to_base64(img, fmt=".png"):
    _, buffer = cv2.imencode(fmt, img)
    return base64.b64encode(buffer).decode("utf-8")


def generate_sample_well():
    """Generate a synthetic well image for demonstration purposes."""
    size = 400
    img = np.ones((size, size, 3), dtype=np.uint8) * 220

    center = (size // 2, size // 2)
    radius = size // 2 - 10
    cv2.circle(img, center, radius, (240, 240, 240), -1)

    rng = np.random.RandomState(42)
    n_plaques = rng.randint(15, 40)
    plaque_info = []

    for i in range(n_plaques):
        angle = rng.uniform(0, 2 * np.pi)
        dist = rng.uniform(0, radius - 25)
        px = int(center[0] + dist * np.cos(angle))
        py = int(center[1] + dist * np.sin(angle))
        pr = rng.randint(5, 18)
        intensity = rng.randint(60, 140)
        color = (intensity, intensity, int(intensity * 1.2))
        cv2.circle(img, (px, py), pr, color, -1)
        plaque_info.append({"x": px, "y": py, "r": pr})

    noise = rng.randint(0, 15, img.shape, dtype=np.uint8)
    img = cv2.add(img, noise)

    cv2.circle(img, center, radius, (180, 180, 180), 2)

    return img


@app.route("/")
def index():
    return render_template("index.html")


@app.route("/api/sample-image", methods=["GET"])
def get_sample_image():
    """Generate and return a sample well image for testing."""
    img = generate_sample_well()
    b64 = numpy_to_base64(img)
    return jsonify({"image": b64, "width": img.shape[1], "height": img.shape[0]})


@app.route("/api/detect", methods=["POST"])
def detect_plaques():
    if "image" not in request.files:
        return jsonify({"error": "No image file provided"}), 400

    file = request.files["image"]
    if file.filename == "" or not allowed_file(file.filename):
        return jsonify({"error": "Invalid file type"}), 400

    file_bytes = file.read()
    nparr = np.frombuffer(file_bytes, np.uint8)
    image = cv2.imdecode(nparr, cv2.IMREAD_COLOR)

    if image is None:
        return jsonify({"error": "Could not decode image"}), 400

    params = {}
    param_json = request.form.get("params")
    if param_json:
        try:
            params = json.loads(param_json)
        except json.JSONDecodeError:
            pass

    auto_mode = request.form.get("auto_mode", "true").lower() == "true"
    show_steps = request.form.get("show_steps", "false").lower() == "true"

    detector = PlaqueDetector(params)

    if auto_mode:
        auto_params = detector.auto_detect_params(image)
        auto_params.update(params)
        detector = PlaqueDetector(auto_params)
        used_params = auto_params
    else:
        used_params = detector.params

    if show_steps:
        plaques, steps = detector.detect_with_steps(image)
        step_images = {}
        for name, img in steps.items():
            step_images[name] = numpy_to_base64(img)
    else:
        plaques = detector.detect(image)
        annotated = detector._draw_annotations(image, plaques)
        step_images = {"annotated": numpy_to_base64(annotated)}

    result_id = str(uuid.uuid4())[:8]
    result = {
        "result_id": result_id,
        "plaque_count": len(plaques),
        "plaques": plaques,
        "parameters": {k: v for k, v in used_params.items()},
        "images": step_images,
    }

    result_path = os.path.join(app.config["RESULTS_FOLDER"], f"{result_id}.json")
    with open(result_path, "w") as f:
        serializable = {k: v for k, v in result.items() if k != "images"}
        json.dump(serializable, f, indent=2)

    return jsonify(result)


@app.route("/api/detect-sample", methods=["POST"])
def detect_sample():
    """Detect plaques in the built-in sample image."""
    image = generate_sample_well()

    params = {}
    param_json = request.form.get("params") if request.form else None
    if param_json:
        try:
            params = json.loads(param_json)
        except json.JSONDecodeError:
            pass

    show_steps = False
    if request.form:
        show_steps = request.form.get("show_steps", "false").lower() == "true"

    detector = PlaqueDetector(params)
    auto_params = detector.auto_detect_params(image)
    auto_params.update(params)
    detector = PlaqueDetector(auto_params)

    if show_steps:
        plaques, steps = detector.detect_with_steps(image)
        step_images = {}
        for name, img in steps.items():
            step_images[name] = numpy_to_base64(img)
    else:
        plaques = detector.detect(image)
        annotated = detector._draw_annotations(image, plaques)
        step_images = {
            "original": numpy_to_base64(image),
            "annotated": numpy_to_base64(annotated),
        }

    result_id = str(uuid.uuid4())[:8]
    result = {
        "result_id": result_id,
        "plaque_count": len(plaques),
        "plaques": plaques,
        "parameters": auto_params,
        "images": step_images,
    }

    result_path = os.path.join(app.config["RESULTS_FOLDER"], f"{result_id}.json")
    with open(result_path, "w") as f:
        serializable = {k: v for k, v in result.items() if k != "images"}
        json.dump(serializable, f, indent=2)

    return jsonify(result)


@app.route("/api/detect-plate", methods=["POST"])
def detect_plate():
    """Detect plaques in a multi-well plate image."""
    if "image" not in request.files:
        return jsonify({"error": "No image file provided"}), 400

    file = request.files["image"]
    file_bytes = file.read()
    nparr = np.frombuffer(file_bytes, np.uint8)
    image = cv2.imdecode(nparr, cv2.IMREAD_COLOR)

    if image is None:
        return jsonify({"error": "Could not decode image"}), 400

    plate_format = request.form.get("plate_format", "96-well")
    parser = PlateParser(plate_format)
    wells = parser.split_plate(image)

    results = {}
    detector = PlaqueDetector()

    for label, well_img in wells.items():
        auto_params = detector.auto_detect_params(well_img)
        det = PlaqueDetector(auto_params)
        plaques = det.detect(well_img)
        annotated = det._draw_annotations(well_img, plaques)
        results[label] = {
            "plaque_count": len(plaques),
            "plaques": plaques,
            "image": numpy_to_base64(annotated),
        }

    return jsonify({
        "plate_format": plate_format,
        "total_wells": len(results),
        "wells": results,
    })


@app.route("/api/estimate-titer", methods=["POST"])
def estimate_titer():
    """Estimate neutralizing antibody titers from plaque count data."""
    data = request.get_json()
    if not data:
        return jsonify({"error": "No JSON data provided"}), 400

    dilutions = data.get("dilutions")
    plaque_counts = data.get("plaque_counts")
    control_count = data.get("control_count")
    model = data.get("model", "4PL")
    cutoffs = data.get("cutoffs", [50, 80, 90])

    if not dilutions or not plaque_counts:
        return jsonify({"error": "dilutions and plaque_counts are required"}), 400

    if len(dilutions) != len(plaque_counts):
        return jsonify({"error": "dilutions and plaque_counts must have the same length"}), 400

    estimator = TiterEstimator(model=model)
    success = estimator.fit(dilutions, plaque_counts, control_count)

    if not success:
        return jsonify({"error": "Could not fit dose-response curve", "success": False}), 200

    curve_data = estimator.get_curve_data()
    summary = estimator.get_results_summary(cutoffs)
    summary["success"] = True
    summary["curve"] = curve_data

    return jsonify(summary)


@app.route("/api/export/<result_id>", methods=["GET"])
def export_result(result_id):
    """Export results as CSV."""
    result_path = os.path.join(app.config["RESULTS_FOLDER"], f"{result_id}.json")
    if not os.path.exists(result_path):
        return jsonify({"error": "Result not found"}), 404

    with open(result_path) as f:
        result = json.load(f)

    output = io.StringIO()
    writer = csv.writer(output)
    writer.writerow(["Plaque ID", "X", "Y", "Radius", "Area", "Circularity",
                      "Mean Intensity", "Confidence"])
    for p in result.get("plaques", []):
        writer.writerow([
            p["id"], round(p["x"], 1), round(p["y"], 1),
            round(p["radius"], 1), p["area"],
            round(p["circularity"], 3), round(p["mean_intensity"], 1),
            round(p["confidence"], 3)
        ])

    output.seek(0)
    return send_file(
        io.BytesIO(output.getvalue().encode()),
        mimetype="text/csv",
        as_attachment=True,
        download_name=f"plaques_{result_id}.csv"
    )


@app.route("/uploads/<filename>")
def uploaded_file(filename):
    return send_from_directory(app.config["UPLOAD_FOLDER"], filename)


if __name__ == "__main__":
    app.run(debug=True, host="0.0.0.0", port=5000)
