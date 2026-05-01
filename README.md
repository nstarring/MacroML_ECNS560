# Forecasting Economic Behavior via Persistent Homology

This project implements a novel approach to recession prediction by treating the macro-economy as a high-dimensional manifold. By applying **Persistent Homology (PH)** and measuring the **Wasserstein distance** (optimal transport) between consecutive temporal states, we identify structural "velocity" changes that signal upcoming economic contractions.

---

## Core Methodology

### 1. Topological Data Analysis (TDA)
Instead of traditional point-estimate forecasting, we model the "shape" of economic data using:
*   **Persistent Homology:** Constructing simplicial complexes across varying radii to identify persistent topological features (connectivity, loops, and voids).
*   **Wasserstein Distance:** Quantifying the "work" required to transform the topology of window $t$ into window $t+1$. This acts as a topological derivative, detecting regime shifts.

### 2. Feature Engineering
We utilize three primary variable specifications:
*   **Pre-Depression:** Long-run data including Gold, Industrial Production, and S&P 500.
*   **Real Economy:** Consumer Sentiment, U3 Unemployment, and Housing Starts.
*   **Monetary Movers:** Federal Funds Rate, Inflation, and Yield spreads.

Data is pre-processed using relative-scaled first-differencing and normalization to ensure the PH filtration is not biased by variable scale or autocorrelation.

---

## Key Results

*   **Best Specification:** The "Real Economy" models demonstrated the highest predictive power.
*   **Performance:** Achieved a peak **AUC of 0.8** in out-of-sample testing (post-2000 data) for 12-month recession forecasts.  This is likely due to chance and the number of model variants tested.
*   **Feature Importance:** Lasso selection consistently prioritized $D_0$ (connectivity) and $D_1$ (cycles), suggesting that the emergence of "holes" in the economic manifold is a more reliable predictor than higher-dimensional voids ($D_2$).  This may be due to the density of the data not permitting a strong identification of voids (or the lack thereof), moreso than the true relevance of the predictors.
