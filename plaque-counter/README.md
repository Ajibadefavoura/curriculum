# PlaqueScope — Modern Virus Plaque Counter

A modern, web-based alternative to [Viridot](https://github.com/leahkatzelnick/Viridot) for automated virus plaque counting and neutralizing antibody titer estimation.

## Key Improvements over Viridot

| Feature | Viridot | PlaqueScope |
|---------|---------|-------------|
| Language | R + Shiny | Python + Flask (web) |
| Installation | R, BiocManager, EBImage | `pip install` |
| UI | R Shiny (desktop) | Modern drag-and-drop web UI |
| Image processing | Basic thresholding | Adaptive threshold + watershed + morphology |
| Color spaces | RGB, HSV | RGB, HSV, LAB |
| Confidence | None | Per-plaque confidence scores |
| Pipeline viewer | Limited | Full step-by-step visualization |
| Plate formats | 96, 24-well | 6, 12, 24, 96-well |
| Titer models | 2-3PL | 2PL, 3PL, 4PL with CI |
| Export | Custom format | CSV, JSON, annotated images |

## Quick Start

```bash
cd plaque-counter
pip install -r requirements.txt
python app.py
```

Open http://localhost:5000 in your browser.

## Features

- **Plaque counting** with automatic parameter detection
- **Watershed segmentation** for separating overlapping plaques
- **Multi-color-space processing** (grayscale, RGB channels, HSV, LAB)
- **Neutralizing antibody titer estimation** (2PL, 3PL, 4PL logistic regression)
- **Interactive dose-response curves** with confidence intervals
- **Multi-well plate parsing** (6, 12, 24, 96-well formats)
- **Full processing pipeline visualization** for optimization
- **Export** as CSV, JSON, or annotated images

## API Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/` | GET | Web interface |
| `/api/detect` | POST | Detect plaques in uploaded image |
| `/api/detect-sample` | POST | Detect plaques in sample image |
| `/api/detect-plate` | POST | Detect plaques in full plate image |
| `/api/estimate-titer` | POST | Estimate neutralization titers |
| `/api/export/<id>` | GET | Export results as CSV |
| `/api/sample-image` | GET | Get sample well image |

## Citation

If you use this tool, please also cite the original Viridot paper:

> Katzelnick et al. (2018). Viridot: An automated virus plaque (immunofocus) counter for the measurement of serological neutralizing responses with application to dengue virus. *PLoS Negl Trop Dis* 12(10): e0006862.
