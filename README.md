🛡️ London Crime Severity & Hotspot Analysis

🔴 The Problem

Traditional policing relies on "crime counts" (volume). However, this is inefficient: one shoplifting incident is counted the same as one assault. This leads to resource misallocation where police focus on high-volume/low-harm areas rather than high-risk zones.

✅ The Solution

This project implements a Harm-Weighted Hotspot Analysis (based on the Cambridge Crime Harm Index methodology). By weighing crimes by their sentencing severity, we identify areas of high victimization cost r\ather than just high activity.

🛠️ Tech Stack

R: Data cleaning & transformation (dplyr, lubridate)

Geospatial: Spatial joins and kernel density estimation (sf, tmap)

Visualization: Time-series decomposition of crime trends (ggplot2)

📊 Key Insights

Westminster has the highest volume of crime, but Lambeth shows a higher density of harm-weighted violent offenses during weekends.

Seasonality: Property crime spikes in Q4 (Winter), whereas violent crime peaks in Q3 (Summer).

🚀 How to Run

Clone the repo

Run renv::restore() to install dependencies.

Run HotSpotCrimeMapping.R
