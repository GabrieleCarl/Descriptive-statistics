# Real Estate Market Analysis - Descriptive Statistics (R)

Descriptive statistical analysis of the Texas residential real estate market across 4 cities from 2010 to 2014, developed as the final project for the Descriptive Statistics course.

## Overview

The project applies a complete descriptive statistics toolkit to a real estate dataset, covering variable classification, position and variability indices, frequency distributions, probability calculations, and multi-dimensional data visualization.

## Dataset

240 observations across 4 Texas cities (Beaumont, Bryan-College Station, Tyler, Wichita Falls), covering monthly data from 2010 to 2014.

| Variable | Type | Scale |
|---|---|---|
| City | Qualitative | Nominal |
| year | Quantitative | Interval |
| month | Quantitative | Ordinal |
| sales | Quantitative discrete | Ratio |
| volume | Quantitative continuous | Ratio |
| median_price | Quantitative continuous | Ratio |
| listings | Quantitative discrete | Ratio |
| months_inventory | Quantitative continuous | Ratio |

## Analysis

**Indices computed** (position, variability, shape):
- Position: mean (arithmetic, weighted, geometric, harmonic), median, quartiles, percentiles, mode, min/max
- Variability: range, interquartile range, variance, standard deviation, coefficient of variation
- Shape: skewness (Fisher), kurtosis

**Key findings:**

- `Volume` has the highest variability (CV) and the strongest positive skewness (Fisher index = 0.88)
- `median_price` range: $73,800 – $180,000 - divided into 4 equal-width classes ($26,550 each); Gini index G' = 0.753
- `City` Gini index G' = 1 (perfectly uniform distribution across 4 cities)

**Probability:**
- P(Beaumont) = 25.0%
- P(July) = 8.33%
- P(December AND 2012) = 1.67%

**Engineered features:**
- `average_price` - derived from volume (converted from millions $) divided by sales
- `vendite_giornaliere` - estimated daily sales rate; Tyler leads all cities in listing effectiveness

## Visualizations (5 plots)

| Plot | Description |
|---|---|
| Plot 1 | Bar chart - frequency distribution of median_price by price class |
| Plot 2 | Boxplot - median price distribution by city (Bryan-College Station highest) |
| Plot 3 | Boxplot - total sales volume by year and city (Tyler leads; all cities growing 2010–2014) |
| Plot 4 | Stacked bar chart - monthly unit sales by city across years (Q2–Q3 seasonal peak) |
| Plot 5 | Normalized bar chart - proportional monthly sales by city |

## Key Insights

- **Bryan-College Station** has the highest median property price
- **Tyler** generates the highest total sales volume and has the most effective listings
- Sales consistently peak in **Q2–Q3** (spring/summer) across all cities and years
- Total sales volume **increased year-over-year** for all 4 cities from 2010 to 2014

## Tech Stack

R · ggplot2

## How to Run

Open `Progetto_Statistica_Descrittiva_Real_estate.R` in RStudio and run the script. The dataset is loaded at the beginning of the file.

## Project Structure

```
real-estate-descriptive-stats/
├── Progetto_Statistica_Descrittiva_Real_estate.R        # Full R analysis
└── Svolgimento_del_Progetto-Statistica_Descrittiva.pdf  # Project report with plots
```
