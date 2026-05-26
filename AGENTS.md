## Cursor Cloud specific instructions

### Repository Overview

This repository contains the Enki open-source curriculum (Markdown lesson content) plus the `plaque-counter/` application — a modern web-based virus plaque counter.

### PlaqueScope Application (`plaque-counter/`)

- **Run**: `cd plaque-counter && python3 app.py` (starts Flask on port 5000)
- **Dependencies**: `pip install -r plaque-counter/requirements.txt`
- **Stack**: Python 3.12+, Flask, OpenCV, scikit-image, SciPy
- **No database required** — all processing is stateless/in-memory, results saved as JSON files in `results/`

### Important Notes

- The main repository content (all topic directories like `javascript/`, `python/`, `sql/`, etc.) is pure Markdown curriculum — no build/test system exists for it.
- The `plaque-counter/` app uses `opencv-python-headless` (no GUI dependencies needed on the server).
- Flask runs in debug mode by default (`python3 app.py`), enabling auto-reload on code changes.
- Uploaded images and detection results are stored locally in `plaque-counter/uploads/` and `plaque-counter/results/`.
