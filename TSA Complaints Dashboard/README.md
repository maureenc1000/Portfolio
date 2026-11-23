# TSA Complaints: Tableau Dashboard for Airport Operations

An interactive Tableau dashboard that shows airport operations managers where and why travelers complain to the TSA, with a Python ETL step that prepares the data.

## Audience and purpose
Airport operations managers who can act on the results. The dashboard lets them filter by year, airport, category and subcategory, and its story points carry the calls to action.

## Data
TSA complaint counts by airport, month, category and subcategory (January 2015 to January 2024, seven report editions) and an airport code lookup with names and coordinates. The ETL notebook combines them into `complaints_data_For_Visuals.csv` (480,383 rows, 583,206 complaints, 443 airports).

## Key results
- **One category drives the recent jump.** Complaints rose from 47,857 in 2021 to 128,315 in 2023, almost entirely from the Expedited Passenger Screening Program (37.1% of all complaints), which had none recorded from 2016 to 2019. Without it, complaints are still below 2019 levels.
- Every Expedited Passenger Screening Program complaint has a blank subcategory, so the reason behind them is unknown.
- Most complaints by airport: Los Angeles (24,794), JFK (24,677), Newark (24,405), Atlanta (23,591), Orlando (22,308).
- 43% of complaints (437,786) have no airport and can't be mapped, so airport totals understate the true volume.

**Calls to action (from the original report):** find out why the Expedited Passenger Screening Program generates so many complaints, review calls or shadow employees to improve customer interactions, and look into month-to-month swings for seasonal patterns.

## Dashboard
`DSC640_Week9and10.twbx` (open in Tableau): choropleth map of complaints by airport, yearly trend line, stacked bars of categories by year, top 25 subcategories, a slope chart of the top 5 categories, and a box plot of monthly variability. `Airport Map.twb` is a separate map sheet.

## Files
| File | What it is |
|---|---|
| `TSA_Complaints_ETL.ipynb` | Python ETL and quick checks of the dashboard's numbers |
| `complaints-by-subcategory.zip`, `iata-icao.csv` | Inputs used by the ETL (unzip the complaints file into this folder first) |
| `complaints-by-airport.csv`, `complaints-by-category.csv` | The same counts at higher levels (not needed by the ETL) |
| `complaints_data_For_Visuals.zip` | Output file used by Tableau (zipped; the ETL notebook also rebuilds it) |
| `DSC640_Week9and10.twbx`, `Airport Map.twb` | Tableau workbooks |

## Tools
Python (pandas, matplotlib), Tableau

## Context
Master's coursework (DSC640, Weeks 9 and 10), cleaned up for this portfolio.

## Portfolio cleanup (November 2026)
This cleanup was done with help from Claude (Anthropic).

- Added a summary, quick checks and written findings to the ETL notebook
- Confirmed the seven report editions do not overlap (no double counting) and reported the 5 airport codes that had no match
- Explained the 2022-2023 jump in complaints, which comes from a single category
- The output file Tableau uses is identical to the original (480,383 rows, 583,206 complaints)
- Every number was checked against a fresh run
