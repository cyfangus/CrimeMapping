🛡️ London Crime Severity & Hotspot Analysis

🔴 The Problem

Traditional policing relies on "crime counts" (volume). However, this is inefficient: one shoplifting incident is counted the same as one assault. This leads to resource misallocation where police focus on high-volume/low-harm areas rather than high-risk zones.

✅ The Solution

This project implements a Harm-Weighted Hotspot Analysis using the National Crime Harm Index (NCHI) methodology. Instead of simple counts, crimes are weighted by their judicial severity (average custodial days). This methodology ensures that hotspots represent areas of high victimization cost and true police priority, not just high foot traffic.

🛠️ Tech Stack

R: Data cleaning & transformation (dplyr, lubridate, tsibble)

Geospatial: Spatial joins and interactive mapping (sf, leaflet)

Machine Learning: Time Series Forecasting (fable, feasts, forecast)

Visualization: Time-series decomposition of crime trends (ggplot2, viridis)

📊 Key Insights

Volume vs. Harm: Anti-social behaviour often dominates raw volume counts, but Robbery and Violence and Sexual Offences generate the highest Harm Scores (average custodial days), shifting strategic focus away from simple noise complaints.

Predictive Policing: An ETS model is applied to the monthly aggregate harm score to forecast future trends. This provides an actionable 12-month outlook for resource pre-allocation.

Seasonality: Property crime spikes in Q4 (Winter), whereas violent crime peaks in Q3 (Summer).

🚀 How to Run

Clone the repo

Run renv::restore() to install dependencies.

Run HotSpotCrimeMapping.R
