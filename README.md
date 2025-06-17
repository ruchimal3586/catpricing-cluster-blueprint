# Catastrophe Risk Pricing Blueprint

This project illustrates how to combine hazard raster data, exposure clustering, and stochastic event simulations to estimate catastrophe risk premiums in the insurance industry.


## 🔍 Key Features

- **Open-source hazard integration** using CHIRPS precipitation rasters
- **Asset-level exposure simulation** (randomized across California)
- **Stochastic event loss simulation** with deductible & limit logic
- **Construction-specific vulnerability mapping**
- **K-means clustering** to segment exposure regions
- **Exceedance Probability (EP) curves** by region
- **Pricing vs. Cat Load Comparison** for technical underwriting

---

## 📦 Project Structure

| File | Description |
|------|-------------|
| `Bleuprint_GeospatialClustering_Insurance.R` | Full simulation pipeline |
| `chirps-v3.0.2014.tif`, `chirps-v3.0.2024.tif` | CHIRPS precipitation rasters 

## 📈 Sample Outputs

- Average CAT Load vs Premium by Cluster  
- Loss Ratio Heatmap  
- EP Curve for Each Cluster  

![Cluster Loss Ratio Example](path/to/your/image.png)

---

## 🧠 Why This Matters

This project mimics the type of risk simulation and loss calibration used by insurers and reinsurers when pricing catastrophe-exposed risks like wildfire, hurricane, or earthquake.

It explores questions such as:
- Are certain regions consistently underpriced?
- How does asset clustering impact risk accumulation?
- What’s the modeled loss-to-premium ratio by region?

---

## 💬 Looking for Input

This is a prototype. If you're a CAT modeler, actuarial analyst, or underwriter, I’d love your feedback on:
- Better ways to calibrate CAT load
- How you incorporate model outputs into pricing decisions
- What modeling assumptions you'd challenge or refine

---

## 👋 About Me

Hi, I’m Ruchi. I’m transitioning into catastrophe pricing and insurance risk analytics from a background in climate risk modeling. I’m currently exploring opportunities in this space and would love to connect with professionals working in cat modeling, reinsurance strategy, or technical underwriting.

---

## 📫 Contact

📧 ruchi.malhotra1@outlook.com  
🌐 [LinkedIn](https://www.linkedin.com/in/ruchimalhotra/)  

## ⚠️ Disclaimer

This is an independent modeling blueprint for educational and demonstration purposes only. The hazard data used is publicly available and not proprietary.


Created in R using `terra`, `sf`, `ggplot2`, and `dplyr`.

Contact: Ruchi Malhotra
[LinkedIn](https://www.linkedin.com/in/ruchimalhotra/)