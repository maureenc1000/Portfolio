# Original data notes

**Census population by ZIP code (Kaggle dataset `census/us-population-by-zip-code`)**

`us-population-by-zip-code` is the Kaggle dataset name, not a file name. The download arrives as `Data\population_by_zip_2010.csv` (included here zipped, as `Data\population_by_zip_2010.zip`). `3_API_Census_Population.ipynb` converts it to `Data\population_by_zip_2010.json` (178 MB), rebuilding it locally when it is missing. The JSON should be left out of GitHub because it is over GitHub's 100 MB file limit.

| File | Size | Rows | Columns |
|---|---|---|---|
| population_by_zip_2010.csv (included, zipped) | 56.8 MB | 1,622,831 | 6 |
| population_by_zip_2010.json (rebuilt by notebook 3) | 178.0 MB | 1,622,831 | 6 |

Columns: population, minimum_age, maximum_age, gender, zipcode, geo_id. Each ZIP code appears three ways (by gender and age band, a total per gender, and an overall total), so only the overall total rows are used for population: 312,444,427 people in 33,119 ZIP codes.

The Kaggle key file is not included. The API cells only run when `Kaggle.json` is in this folder; without it they are skipped and the notebook uses the saved CSV.

**IRS Statistics of Income, 2010 (`Data\2010 Income Tax.csv`)**: 93 MB, 167,022 rows, 73 columns; dollar amounts are in thousands. Included here zipped, as `Data\2010 Income Tax.zip`.

Counts measured directly from the files on 2026-11-14.
