# Kia and Hyundai Car Thefts: A Report for City Decision-Makers

An R visualization project showing how much of the 2019-2022 rise in car thefts came from Kia and Hyundai vehicles, and where cities should focus prevention.

## Data
Motherboard/VICE News Kia and Hyundai theft data:
- `carTheftsMap.csv`: yearly car thefts by reporting agency, 2019-2022
- `kiaHyundaiThefts.csv`: monthly Kia/Hyundai and other thefts for a set of cities, January 2019 to December 2022
- `KiaHyundaiMilwaukeeData.csv`: monthly Kia/Hyundai thefts in Milwaukee
- `Motherboard VICE News Kia Hyundai Theft Data Upd.xlsx`: theft totals by city (67 cities with more than 100 thefts)

## Key results
- Car thefts across all reporting agencies rose **47.2%**, from 318,467 in 2019 to 468,821 in 2022.
- In the monthly multi-city data, Kia and Hyundai went from **5.6%** of thefts in January 2019 to **44.0%** in December 2022.
- Across the 67 cities, Kia and Hyundai account for **16.2%** of thefts (122,085 of 755,298).
- Five cities (Chicago, Milwaukee, Los Angeles, Denver and Minneapolis) account for **48.6%** of all Kia/Hyundai thefts in the data. In Milwaukee, **53%** of all car thefts were Kias or Hyundais.
- Milwaukee's Kia/Hyundai thefts went from 22 in January 2019 to a peak of 832 in August 2021.

**Recommendation (from the original report):** focus police and community resources on the highest-volume cities and agencies, and offer steering wheel locks or immobilizer kits to owners of affected Kia and Hyundai models in those areas.

## Charts (saved to `charts/`)
| Chart | Shows |
|---|---|
| `pie_chart.png` | Kia/Hyundai share of all thefts |
| `donut_chart.png` | Top 5 cities by Kia/Hyundai thefts |
| `stacked_bar.png` | Yearly thefts at the top 10 reporting agencies |
| `treemap.png` | Relative theft volume of the top 15 agencies |
| `area_chart.png` | Monthly Kia/Hyundai thefts in Milwaukee |
| `stacked_area.png` | Kia/Hyundai vs. other thefts over time |

## Files
| File | What it is |
|---|---|
| `Kia_Hyundai_Thefts.R` | The script: cleans the data, prints the key numbers, saves the charts |
| Data files listed above | Inputs (`Motherboard VICE News Kia Hyundai Theft Data.xlsx` is the original download; the script uses the updated version) |

## Tools
R (tidyverse, readxl, treemapify, lubridate, scales), ggplot2 with a color-blind friendly palette (Okabe-Ito)

## How to run
From this folder: `Rscript Kia_Hyundai_Thefts.R` (install the packages listed at the top of the script first).

## Context
Master's coursework (DSC640, Weeks 5 and 6), cleaned up for this portfolio.

## Portfolio cleanup (November 2026)
This cleanup was done with help from Claude (Anthropic).

- Added a header explaining the question, data and how to run it
- The script now prints the key numbers behind the charts and saves each chart as a PNG
- Cleaned city names (for example "MilwaukeeWI" became "Milwaukee") and fixed chart typos
- Replaced a chart subtitle that referred to an event not in the data with one based on the data
- Every number in this README comes from the script's output
