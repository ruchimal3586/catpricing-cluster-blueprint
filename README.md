


# Catastrophe Risk Pricing Blueprint

A compact, transparent *mini-cat* workflow that turns hazard rasters → clustered exposures → stochastic event losses → **AAL / EP / PML / TVaR** for pricing and portfolio steering.

**▶ Live demo (HTML):** [Open the report](https://ruchimal3586.github.io/catpricing-cluster-blueprint/)

> If the link 404s, knit `Rmd/catpricing_report.Rmd` and move the generated `catpricing_report.html` to the repo root.

---

## 🔍 Key Features
- **Open-source hazards** (CHIRPS precipitation rasters)
- **Asset-level exposure simulation** across California
- **Stochastic event loss simulation** with deductible/limit logic
- **Construction-specific vulnerability mapping**
- **K-means clustering** for accumulation cohorts
- **EP curves, PML(100/200), TVaR(99/99.5)** by cluster
- **Pricing vs CAT load** comparisons for technical underwriting

---

## 📦 Repository Structure

| Path | What it contains |
|---|---|
| `R/` | Script pipeline(s), e.g., `catpricing_pipeline.R` |
| `Rmd/` | Reports, e.g., `catpricing_report.Rmd` (knit to HTML) |
| `data/` | Input rasters: `chirps-v3.0.2014.tif`, `chirps-v3.0.2024.tif` |
| `outputs/` | Auto-generated CSVs/tables/figures (gitignored) |
| `catpricing_report.html` | Rendered HTML report (landing link above) |

> Legacy script: `Bleuprint_GeospatialClustering_Insurance.R` (kept for reference).

---

## 🏁 Quick Start


# one-time
install.packages(c("rmarkdown","dplyr","tidyr","purrr","scales","ggplot2",
                   "terra","sf","cluster","data.table","kableExtra","here"))

# knit the report
rmarkdown::render("Rmd/catpricing_report.Rmd")

# outputs created in /outputs; HTML report saved in Rmd/ by default.
# Move/rename it to: catpricing_report.html (repo root)


```



## 📈 Sample Outputs

- Average CAT Load vs Premium by Cluster  
- Loss Ratio Heatmap  
- EP Curve for Each Cluster  

![Cluster Loss Ratio Example](path/to/your/image.png)


## 🧠 Why This Matters

This mirrors the workflow insurers/reinsurers use to translate climate hazard intelligence into pricing-usable distributions rather than scores—supporting rate reviews, reinsurance structuring, and resilience ROI.

Questions you can answer quickly:

Which clusters drive tail risk (PML/TVaR)?

Where is pricing pressure vs modeled CAT load?

How would resilience capex shift AAL/PML/TVaR?

💬 Looking for Input

If you’re a CAT modeler, actuary, or underwriter, I’d love feedback on:

Calibration of hazard → vulnerability → loss

How you consume AAL/EP/PML/TVaR in pricing and capital

Assumptions you’d challenge or refine

## 👋 About Me

I’m Ruchi. I bridge climate modeling with financial risk translation—so climate data becomes decision-ready for pricing, capital, and strategy.


## 📫 Contact

📧 ruchi.malhotra1@outlook.com  
🌐 [LinkedIn](https://www.linkedin.com/in/ruchimalhotra/)  

License & Citation

MIT License.
Please cite: Malhotra, R. (2025). Cat Pricing Cluster Blueprint. GitHub.
