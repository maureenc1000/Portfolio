# U.S. Retail Sales Forecast

Forecasts a full year of U.S. monthly retail sales with Holt-Winters exponential smoothing and checks how well it held up through the COVID-19 period.

## Data
U.S. monthly retail sales in millions of dollars, January 1992 to June 2021 (`us_retail_sales.csv`, 354 months).

## Approach
- Reshaped the year-by-month table into a monthly time series
- Trained Holt-Winters exponential smoothing (additive trend, 12-month seasonality) on January 1992 to June 2020
- Forecast July 2020 to June 2021 and compared it with actual sales and with a naive baseline (repeat the last known month)

## Key results
| Forecast | RMSE (millions $) | MAPE |
|---|---|---|
| Holt-Winters | 45,157 | 6.5% |
| Naive (repeat June 2020) | 48,985 | 7.1% |

- The model under-forecast every month: by 1.2% to 3.5% through December 2020, then by 11.7% to 14.4% from March to June 2021, when sales rose well above their historical trend.
- The data shows the two sharpest drops at the 2008 financial crisis (down 13.6% from June 2008 to March 2009) and the start of COVID-19 (down 17.3% from February to April 2020).

## Files
| File | What it is |
|---|---|
| `Retail_Sales_Forecast.ipynb` | The analysis |
| `us_retail_sales.csv` | The data |

## Tools
Python (pandas, statsmodels, scikit-learn, matplotlib)

## How to run
Keep `us_retail_sales.csv` in this folder and run the notebook from top to bottom.

## Context
Master's coursework (DSC630, Week 8), cleaned up for this portfolio.

## Portfolio cleanup (November 2026)
This cleanup was done with help from Claude (Anthropic).

- Combined two copies of the same notebook into one, with a summary and findings
- Added a naive baseline so the RMSE can be judged against something, plus MAPE and a month-by-month comparison table
- Checked the observations against the data: the 2008-2009 and early-2020 drops are confirmed with exact figures, and the note now says 2020 as a whole still finished 3.1% above 2019
- Removed a duplicate date conversion and cleaned up chart titles and labels
- The RMSE matches the original notebook (45,157); every other number was checked against a fresh run
