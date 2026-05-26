"""
Neutralizing antibody titer estimation module.

Fits dose-response curves (2-parameter, 3-parameter, or 4-parameter logistic)
to plaque count data and estimates PRNT titers at user-specified cut-offs.
"""

import numpy as np
from scipy.optimize import curve_fit
from scipy.interpolate import interp1d


def logistic_2p(x, ec50, slope):
    """2-parameter logistic: top=1, bottom=0."""
    return 1.0 / (1.0 + (x / ec50) ** slope)


def logistic_3p(x, ec50, slope, top):
    """3-parameter logistic: bottom=0."""
    return top / (1.0 + (x / ec50) ** slope)


def logistic_4p(x, ec50, slope, top, bottom):
    """4-parameter logistic."""
    return bottom + (top - bottom) / (1.0 + (x / ec50) ** slope)


class TiterEstimator:
    MODELS = {
        "2PL": (logistic_2p, ["EC50", "Slope"], [100, 1]),
        "3PL": (logistic_3p, ["EC50", "Slope", "Top"], [100, 1, 1]),
        "4PL": (logistic_4p, ["EC50", "Slope", "Top", "Bottom"], [100, 1, 1, 0]),
    }

    def __init__(self, model="4PL"):
        if model not in self.MODELS:
            raise ValueError(f"Unknown model: {model}. Choose from {list(self.MODELS.keys())}")
        self.model_name = model
        self.func, self.param_names, self.p0 = self.MODELS[model]
        self.popt = None
        self.pcov = None

    def fit(self, dilutions, plaque_counts, control_count=None):
        """
        Fit the dose-response curve.

        dilutions: array of dilution factors (e.g. [10, 20, 40, 80, ...])
        plaque_counts: array of plaque counts at each dilution
        control_count: plaque count for virus-only control (no serum)
        """
        dilutions = np.array(dilutions, dtype=float)
        plaque_counts = np.array(plaque_counts, dtype=float)

        if control_count is None:
            control_count = np.max(plaque_counts)
        control_count = max(control_count, 1)

        self.neutralization = 1.0 - plaque_counts / control_count
        self.neutralization = np.clip(self.neutralization, 0, 1)
        self.dilutions = dilutions
        self.control_count = control_count

        try:
            self.popt, self.pcov = curve_fit(
                self.func, dilutions, self.neutralization,
                p0=self.p0, maxfev=10000,
                bounds=(0, np.inf)
            )
        except (RuntimeError, ValueError):
            self.popt = None
            self.pcov = None
            return False
        return True

    def estimate_titer(self, cutoff=50):
        """
        Estimate the PRNT titer at the given cutoff (e.g. PRNT50).
        Returns the dilution at which neutralization = cutoff%.
        """
        if self.popt is None:
            return None

        target = cutoff / 100.0
        dilution_range = np.logspace(
            np.log10(max(self.dilutions.min() / 10, 0.1)),
            np.log10(self.dilutions.max() * 10),
            10000
        )
        curve_values = self.func(dilution_range, *self.popt)

        if np.all(curve_values >= target) or np.all(curve_values <= target):
            return None

        try:
            interp = interp1d(curve_values[::-1], dilution_range[::-1],
                              kind='linear', bounds_error=False)
            titer = float(interp(target))
            return titer
        except Exception:
            return None

    def get_curve_data(self, n_points=200):
        """Generate smooth curve for plotting."""
        if self.popt is None:
            return None

        x_min = max(self.dilutions.min() / 2, 0.1)
        x_max = self.dilutions.max() * 2
        x_smooth = np.logspace(np.log10(x_min), np.log10(x_max), n_points)
        y_smooth = self.func(x_smooth, *self.popt)

        return {
            "x": x_smooth.tolist(),
            "y": y_smooth.tolist(),
            "raw_x": self.dilutions.tolist(),
            "raw_y": self.neutralization.tolist(),
        }

    def get_confidence_interval(self, cutoff=50, alpha=0.05):
        """Estimate confidence interval via parameter covariance."""
        if self.popt is None or self.pcov is None:
            return None, None

        from scipy.stats import norm
        z = norm.ppf(1 - alpha / 2)
        se = np.sqrt(np.diag(self.pcov))

        lower_params = self.popt - z * se
        upper_params = self.popt + z * se

        lower_params = np.maximum(lower_params, 1e-10)

        target = cutoff / 100.0
        dilution_range = np.logspace(
            np.log10(max(self.dilutions.min() / 10, 0.1)),
            np.log10(self.dilutions.max() * 10),
            10000
        )

        try:
            curve_lower = self.func(dilution_range, *lower_params)
            curve_upper = self.func(dilution_range, *upper_params)

            interp_l = interp1d(curve_lower[::-1], dilution_range[::-1],
                                kind='linear', bounds_error=False, fill_value="extrapolate")
            interp_u = interp1d(curve_upper[::-1], dilution_range[::-1],
                                kind='linear', bounds_error=False, fill_value="extrapolate")

            ci_lower = float(interp_l(target))
            ci_upper = float(interp_u(target))
            return min(ci_lower, ci_upper), max(ci_lower, ci_upper)
        except Exception:
            return None, None

    def get_results_summary(self, cutoffs=None):
        if cutoffs is None:
            cutoffs = [50, 80, 90]

        results = {
            "model": self.model_name,
            "parameters": {},
            "titers": {},
        }

        if self.popt is not None:
            for name, val in zip(self.param_names, self.popt):
                results["parameters"][name] = round(float(val), 4)

        for c in cutoffs:
            titer = self.estimate_titer(c)
            ci = self.get_confidence_interval(c)
            results["titers"][f"PRNT{c}"] = {
                "titer": round(titer, 2) if titer else None,
                "ci_lower": round(ci[0], 2) if ci[0] else None,
                "ci_upper": round(ci[1], 2) if ci[1] else None,
            }

        return results
