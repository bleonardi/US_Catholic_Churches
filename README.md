# Canonical Rigidity & Sacred Space: A Spatial Econometric Analysis

## Overview
A spatial econometric study of the US Catholic Church, investigating the misalignment between legacy territorial boundaries ("Canonical Rigidity") and modern urban demand. The project quantifies "spatial leakage"—where parishioners bypass assigned parishes—and estimates the opportunity cost of underutilized surface parking in high-density urban cores.

## Key Data Science Skills
*   **Spatial Econometrics:** Modeling transport-cost sensitive demand functions in religious contexts.
*   **Computational Geometry:** Using **Voronoi Tessellations** to define "Natural Market Areas" vs. legal boundaries.
*   **External Data Integration:** Merging Church parochial data with **Overture Maps** AI-derived building footprints and US Census ACS 5-year estimates.
*   **Optimization:** Identifying "Shadow Value" of land for potential multi-family housing conversion.

## Tech Stack
*   **R (sf, tidyverse):** Spatial data manipulation and modeling.
*   **Python:** Large-scale data scraping and Overture Maps querying.
*   **Quarto:** Scientific publishing and interactive reports.

## Data Sources
*   **Overture Maps Foundation:** [Building and Infrastructure Layers](https://overturemaps.org/download/)
*   **US Census Bureau:** [ACS 5-Year Estimates (Tract Level)](https://data.census.gov/)
*   **Mass Times Data:** Sourced from public diocesan directories and parish websites.

## Methodology
The project identifies "Inefficient Assets" by finding parishes in the bottom quartile of service frequency that occupy top-quartile land-value locations (measured by pop density and surface parking area).
