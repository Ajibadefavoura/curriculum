/* PlaqueScope — Frontend Application */

document.addEventListener("DOMContentLoaded", () => {
    initTabs();
    initDropZone();
    initParamControls();
    initTiterEstimation();
});

/* ── Tab Navigation ─────────────────────────────────────────── */

function initTabs() {
    document.querySelectorAll(".nav-link").forEach(link => {
        link.addEventListener("click", e => {
            e.preventDefault();
            const tab = link.dataset.tab;
            document.querySelectorAll(".nav-link").forEach(l => l.classList.remove("active"));
            document.querySelectorAll(".tab-content").forEach(t => t.classList.remove("active"));
            link.classList.add("active");
            document.getElementById(tab).classList.add("active");
        });
    });
}

/* ── Drop Zone & File Upload ────────────────────────────────── */

function initDropZone() {
    const dropZone = document.getElementById("dropZone");
    const fileInput = document.getElementById("fileInput");
    const sampleBtn = document.getElementById("sampleBtn");

    dropZone.addEventListener("click", () => fileInput.click());

    dropZone.addEventListener("dragover", e => {
        e.preventDefault();
        dropZone.classList.add("dragover");
    });

    dropZone.addEventListener("dragleave", () => {
        dropZone.classList.remove("dragover");
    });

    dropZone.addEventListener("drop", e => {
        e.preventDefault();
        dropZone.classList.remove("dragover");
        if (e.dataTransfer.files.length > 0) {
            uploadAndDetect(e.dataTransfer.files[0]);
        }
    });

    fileInput.addEventListener("change", () => {
        if (fileInput.files.length > 0) {
            uploadAndDetect(fileInput.files[0]);
        }
    });

    sampleBtn.addEventListener("click", () => {
        detectSample();
    });
}

function getParams() {
    const autoMode = document.getElementById("autoMode").checked;
    if (autoMode) return {};

    return {
        color_channel: document.getElementById("paramChannel").value,
        blur_sigma: parseFloat(document.getElementById("paramBlur").value),
        threshold_method: document.getElementById("paramThreshold").value,
        threshold_block_size: parseInt(document.getElementById("paramBlockSize").value),
        threshold_c: parseInt(document.getElementById("paramC").value),
        min_plaque_area: parseInt(document.getElementById("paramMinArea").value),
        max_plaque_area: parseInt(document.getElementById("paramMaxArea").value),
        min_circularity: parseFloat(document.getElementById("paramCirc").value),
        contrast_alpha: parseFloat(document.getElementById("paramAlpha").value),
        well_trim_px: parseInt(document.getElementById("paramTrim").value),
        watershed_enabled: document.getElementById("paramWatershed").checked,
        invert: document.getElementById("paramInvert").checked,
    };
}

async function uploadAndDetect(file) {
    showLoading(true);
    const formData = new FormData();
    formData.append("image", file);
    formData.append("auto_mode", document.getElementById("autoMode").checked);
    formData.append("show_steps", document.getElementById("showSteps").checked);
    formData.append("params", JSON.stringify(getParams()));

    try {
        const resp = await fetch("/api/detect", { method: "POST", body: formData });
        const data = await resp.json();
        if (data.error) {
            showError(data.error);
        } else {
            renderResults(data);
        }
    } catch (err) {
        showError("Failed to analyze image: " + err.message);
    } finally {
        showLoading(false);
    }
}

async function detectSample() {
    showLoading(true);
    const formData = new FormData();
    formData.append("auto_mode", "true");
    formData.append("show_steps", document.getElementById("showSteps").checked);
    formData.append("params", JSON.stringify(getParams()));

    try {
        const resp = await fetch("/api/detect-sample", { method: "POST", body: formData });
        const data = await resp.json();
        if (data.error) {
            showError(data.error);
        } else {
            renderResults(data);
        }
    } catch (err) {
        showError("Failed to analyze sample: " + err.message);
    } finally {
        showLoading(false);
    }
}

function showLoading(show) {
    document.getElementById("loadingOverlay").classList.toggle("hidden", !show);
}

function showError(msg) {
    document.getElementById("resultsArea").innerHTML = `
        <div class="placeholder" style="color: var(--danger);">
            <p>${msg}</p>
        </div>
    `;
}

function renderResults(data) {
    const area = document.getElementById("resultsArea");
    let html = "";

    html += `<div class="result-header">
        <div class="count-badge">
            <span>Plaques Detected:</span>
            <span class="count-number">${data.plaque_count}</span>
        </div>
        <div class="result-actions">
            <button class="btn btn-secondary" onclick="exportCSV('${data.result_id}')">Export CSV</button>
            <button class="btn btn-secondary" onclick="exportJSON(${JSON.stringify(JSON.stringify(data))})">Export JSON</button>
        </div>
    </div>`;

    html += `<div class="result-images">`;
    for (const [name, b64] of Object.entries(data.images)) {
        html += `<div class="result-image-card">
            <h5>${name.replace(/_/g, " ")}</h5>
            <img src="data:image/png;base64,${b64}" alt="${name}">
        </div>`;
    }
    html += `</div>`;

    if (data.plaques && data.plaques.length > 0) {
        html += `<div class="plaque-table-container">
            <table class="plaque-table">
                <thead><tr>
                    <th>#</th><th>X</th><th>Y</th><th>Radius</th>
                    <th>Area</th><th>Circularity</th><th>Confidence</th>
                </tr></thead>
                <tbody>`;
        for (const p of data.plaques) {
            const confClass = p.confidence >= 0.7 ? "confidence-high"
                            : p.confidence >= 0.4 ? "confidence-med" : "confidence-low";
            html += `<tr>
                <td>${p.id}</td>
                <td>${p.x.toFixed(1)}</td>
                <td>${p.y.toFixed(1)}</td>
                <td>${p.radius.toFixed(1)}</td>
                <td>${p.area}</td>
                <td>${p.circularity.toFixed(3)}</td>
                <td class="${confClass}">${(p.confidence * 100).toFixed(0)}%</td>
            </tr>`;
        }
        html += `</tbody></table></div>`;
    }

    if (data.parameters) {
        html += `<details style="margin-top:1rem">
            <summary style="cursor:pointer;color:var(--text-muted);font-size:0.85rem">
                Parameters Used
            </summary>
            <pre style="background:var(--surface2);padding:0.75rem;border-radius:var(--radius-sm);
                        font-size:0.75rem;overflow-x:auto;margin-top:0.5rem;color:var(--text)">${
                JSON.stringify(data.parameters, null, 2)
            }</pre>
        </details>`;
    }

    area.innerHTML = html;
}

function exportCSV(resultId) {
    window.open(`/api/export/${resultId}`, "_blank");
}

function exportJSON(dataStr) {
    const data = JSON.parse(dataStr);
    const blob = new Blob([JSON.stringify(data, null, 2)], { type: "application/json" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = `plaques_${data.result_id}.json`;
    a.click();
    URL.revokeObjectURL(url);
}

/* ── Parameter Controls ─────────────────────────────────────── */

function initParamControls() {
    const autoMode = document.getElementById("autoMode");
    const manualParams = document.getElementById("manualParams");

    autoMode.addEventListener("change", () => {
        manualParams.classList.toggle("hidden", autoMode.checked);
    });

    const sliders = [
        ["paramBlur", "paramBlurVal", v => parseFloat(v).toFixed(1)],
        ["paramBlockSize", "paramBlockSizeVal", v => v],
        ["paramC", "paramCVal", v => v],
        ["paramMinArea", "paramMinAreaVal", v => v],
        ["paramMaxArea", "paramMaxAreaVal", v => v],
        ["paramCirc", "paramCircVal", v => parseFloat(v).toFixed(2)],
        ["paramAlpha", "paramAlphaVal", v => parseFloat(v).toFixed(1)],
        ["paramTrim", "paramTrimVal", v => v],
    ];

    for (const [sliderId, valId, fmt] of sliders) {
        const slider = document.getElementById(sliderId);
        const val = document.getElementById(valId);
        slider.addEventListener("input", () => { val.textContent = fmt(slider.value); });
    }
}

/* ── Titer Estimation ───────────────────────────────────────── */

function initTiterEstimation() {
    document.getElementById("addRowBtn").addEventListener("click", addDilutionRow);
    document.getElementById("estimateTiterBtn").addEventListener("click", estimateTiter);
    document.getElementById("loadSampleTiter").addEventListener("click", loadSampleTiterData);

    document.getElementById("dilutionRows").addEventListener("click", e => {
        if (e.target.classList.contains("remove-row")) {
            const row = e.target.closest(".dilution-row");
            if (document.querySelectorAll(".dilution-row").length > 2) {
                row.remove();
            }
        }
    });
}

function addDilutionRow() {
    const container = document.getElementById("dilutionRows");
    const row = document.createElement("div");
    row.className = "dilution-row";
    row.innerHTML = `
        <input type="number" placeholder="Dilution" class="dilution-input text-input">
        <input type="number" placeholder="Plaque Count" class="count-input text-input">
        <button class="btn-icon remove-row" title="Remove">&#x2715;</button>
    `;
    container.appendChild(row);
}

function loadSampleTiterData() {
    const container = document.getElementById("dilutionRows");
    container.innerHTML = "";

    const sampleData = [
        [10, 38], [20, 35], [40, 28], [80, 18], [160, 8], [320, 3], [640, 1]
    ];

    for (const [dil, count] of sampleData) {
        const row = document.createElement("div");
        row.className = "dilution-row";
        row.innerHTML = `
            <input type="number" placeholder="Dilution" value="${dil}" class="dilution-input text-input">
            <input type="number" placeholder="Plaque Count" value="${count}" class="count-input text-input">
            <button class="btn-icon remove-row" title="Remove">&#x2715;</button>
        `;
        container.appendChild(row);
    }

    document.getElementById("controlCount").value = 40;
}

async function estimateTiter() {
    const rows = document.querySelectorAll(".dilution-row");
    const dilutions = [];
    const plaqueCounts = [];

    rows.forEach(row => {
        const dil = parseFloat(row.querySelector(".dilution-input").value);
        const count = parseFloat(row.querySelector(".count-input").value);
        if (!isNaN(dil) && !isNaN(count)) {
            dilutions.push(dil);
            plaqueCounts.push(count);
        }
    });

    if (dilutions.length < 3) {
        alert("Please enter at least 3 dilution points.");
        return;
    }

    const controlCount = parseFloat(document.getElementById("controlCount").value);
    const model = document.getElementById("titerModel").value;
    const cutoffsStr = document.getElementById("cutoffs").value;
    const cutoffs = cutoffsStr.split(",").map(s => parseInt(s.trim())).filter(n => !isNaN(n));

    document.getElementById("titerLoading").classList.remove("hidden");

    try {
        const resp = await fetch("/api/estimate-titer", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ dilutions, plaque_counts: plaqueCounts, control_count: controlCount, model, cutoffs })
        });
        const data = await resp.json();
        renderTiterResults(data, dilutions, plaqueCounts, controlCount);
    } catch (err) {
        document.getElementById("titerResults").innerHTML = `
            <div class="placeholder" style="color:var(--danger)"><p>${err.message}</p></div>
        `;
    } finally {
        document.getElementById("titerLoading").classList.add("hidden");
    }
}

function renderTiterResults(data, dilutions, plaqueCounts, controlCount) {
    const area = document.getElementById("titerResults");

    if (!data.success) {
        area.innerHTML = `<div class="placeholder" style="color:var(--danger)">
            <p>Could not fit dose-response curve. Try different data or model.</p>
        </div>`;
        return;
    }

    let html = "";

    html += `<div class="titer-result-card">
        <h4>PRNT Titers (${data.model} model)</h4>
        <div class="titer-values">`;

    for (const [name, info] of Object.entries(data.titers)) {
        const titerVal = info.titer != null ? info.titer.toFixed(1) : "N/A";
        const ci = (info.ci_lower != null && info.ci_upper != null)
            ? `95% CI: ${info.ci_lower.toFixed(1)} – ${info.ci_upper.toFixed(1)}`
            : "";
        html += `<div class="titer-value">
            <span class="label">${name}</span>
            <span class="value">${titerVal}</span>
            <span class="ci">${ci}</span>
        </div>`;
    }
    html += `</div></div>`;

    html += `<div class="curve-container">
        <canvas id="curveCanvas"></canvas>
    </div>`;

    if (data.parameters) {
        html += `<div class="model-params">Model parameters: `;
        for (const [k, v] of Object.entries(data.parameters)) {
            html += `${k}=<span>${v}</span> `;
        }
        html += `</div>`;
    }

    area.innerHTML = html;

    if (data.curve) {
        drawCurve(data.curve, data.titers);
    }
}

function drawCurve(curveData, titers) {
    const canvas = document.getElementById("curveCanvas");
    if (!canvas) return;

    const ctx = canvas.getContext("2d");
    const dpr = window.devicePixelRatio || 1;
    const rect = canvas.parentElement.getBoundingClientRect();

    canvas.width = rect.width * dpr;
    canvas.height = 300 * dpr;
    canvas.style.width = rect.width + "px";
    canvas.style.height = "300px";
    ctx.scale(dpr, dpr);

    const W = rect.width;
    const H = 300;
    const pad = { top: 20, right: 30, bottom: 50, left: 60 };
    const plotW = W - pad.left - pad.right;
    const plotH = H - pad.top - pad.bottom;

    ctx.fillStyle = "#242836";
    ctx.fillRect(0, 0, W, H);

    const xMin = Math.log10(Math.min(...curveData.x));
    const xMax = Math.log10(Math.max(...curveData.x));
    const toX = v => pad.left + (Math.log10(v) - xMin) / (xMax - xMin) * plotW;
    const toY = v => pad.top + (1 - v) * plotH;

    ctx.strokeStyle = "#2e3345";
    ctx.lineWidth = 1;
    for (let i = 0; i <= 10; i++) {
        const y = pad.top + (i / 10) * plotH;
        ctx.beginPath();
        ctx.moveTo(pad.left, y);
        ctx.lineTo(pad.left + plotW, y);
        ctx.stroke();
    }

    ctx.strokeStyle = "#6366f1";
    ctx.lineWidth = 2;
    ctx.beginPath();
    for (let i = 0; i < curveData.x.length; i++) {
        const x = toX(curveData.x[i]);
        const y = toY(curveData.y[i]);
        if (i === 0) ctx.moveTo(x, y);
        else ctx.lineTo(x, y);
    }
    ctx.stroke();

    ctx.fillStyle = "#f59e0b";
    for (let i = 0; i < curveData.raw_x.length; i++) {
        const x = toX(curveData.raw_x[i]);
        const y = toY(curveData.raw_y[i]);
        ctx.beginPath();
        ctx.arc(x, y, 5, 0, Math.PI * 2);
        ctx.fill();
    }

    const cutoffColors = { "PRNT50": "#22c55e", "PRNT80": "#f59e0b", "PRNT90": "#ef4444" };
    for (const [name, info] of Object.entries(titers)) {
        if (info.titer == null) continue;
        const cutoffVal = parseInt(name.replace("PRNT", "")) / 100;
        const color = cutoffColors[name] || "#8b8fa3";
        ctx.setLineDash([5, 5]);
        ctx.strokeStyle = color;
        ctx.lineWidth = 1;
        const y = toY(cutoffVal);
        ctx.beginPath();
        ctx.moveTo(pad.left, y);
        ctx.lineTo(pad.left + plotW, y);
        ctx.stroke();
        ctx.setLineDash([]);

        ctx.fillStyle = color;
        ctx.font = "11px Inter, sans-serif";
        ctx.fillText(name, pad.left + 4, y - 4);
    }

    ctx.fillStyle = "#8b8fa3";
    ctx.font = "11px Inter, sans-serif";
    ctx.textAlign = "center";
    const xTicks = [10, 20, 40, 80, 160, 320, 640, 1280];
    for (const t of xTicks) {
        if (Math.log10(t) >= xMin && Math.log10(t) <= xMax) {
            const x = toX(t);
            ctx.fillText(t.toString(), x, H - pad.bottom + 20);
            ctx.strokeStyle = "#2e3345";
            ctx.beginPath();
            ctx.moveTo(x, pad.top);
            ctx.lineTo(x, pad.top + plotH);
            ctx.stroke();
        }
    }
    ctx.fillText("Dilution Factor", W / 2, H - 5);

    ctx.textAlign = "right";
    for (let i = 0; i <= 10; i += 2) {
        const y = pad.top + (1 - i / 10) * plotH;
        ctx.fillText((i * 10) + "%", pad.left - 8, y + 4);
    }

    ctx.save();
    ctx.translate(14, H / 2);
    ctx.rotate(-Math.PI / 2);
    ctx.textAlign = "center";
    ctx.fillText("Neutralization (%)", 0, 0);
    ctx.restore();
}
