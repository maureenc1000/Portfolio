# Connecting to an API: Census Population through the Kaggle API

Downloads 2010 U.S. census population by ZIP code through the Kaggle API, converts it to JSON, and keeps one total per ZIP code.

This is step 3 of a larger project. The full four-step pipeline (flat file, web scraping, API, SQL merge) is in the **Income and Census Pipeline** folder of this repository.

## Result
The census file lists every ZIP code three ways (by gender and age band, by gender, and in total). Using only the total rows gives 312,444,427 people in 33,119 ZIP codes (U.S. and Puerto Rico, 2010), 50.9% female.

## Files
| File | What it is |
|---|---|
| `3_API_Census_Population.ipynb` | The notebook |
| `Data/Census_API.csv` | Output: one population total per ZIP code |

## How to run
The source file (`population_by_zip_2010.csv`, 57 MB) is too large to include here unzipped. A zipped copy is in the **Income and Census Pipeline** folder (`Data/population_by_zip_2010.zip`): unzip it into a `Data` folder next to this notebook, or download the Kaggle dataset `census/us-population-by-zip-code`. The API cells only run when a `Kaggle.json` key file is present; the key is never included here.

## Tools
Python (pandas, Kaggle API, json)

## Context
Master's coursework (DSC540), cleaned up and corrected for this portfolio. This cleanup was done with help from Claude (Anthropic).
