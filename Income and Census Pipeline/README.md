# Income and Census Data Pipeline (Flat File, Web Scraping, API, SQL)

A four-step data preparation pipeline that pulls income and population data from three kinds of sources, cleans each one to one row per ZIP code, merges them in SQLite, and looks at the highest-income ZIP codes in the U.S.

## Pipeline
| Notebook | Source type | What it does | Output |
|---|---|---|---|
| `1_Flat_File_IRS_Income.ipynb` | Flat file | IRS 2010 tax returns by ZIP code: renames coded columns, removes state-total rows, combines six income brackets | `Data/Income_flat.csv` (27,786 ZIP codes) |
| `2_Web_Scrape_Top_Income_ZIPs.ipynb` | Website | Scrapes Wikipedia's 100 highest per capita income ZIP codes with requests and BeautifulSoup | `Data/TopIncome_wiki_current.csv` |
| `3_API_Census_Population.ipynb` | API | Downloads 2010 census population through the Kaggle API, converts it to JSON, keeps one total per ZIP code | `Data/Census_API.csv` (33,119 ZIP codes) |
| `4_Merge_SQLite_and_Visualize.ipynb` | All three | Loads the sources into SQLite, joins them on ZIP code, charts the results | `Data/merged_file.csv` |

## Key results
- IRS data: 137.1 million returns and $5.68 trillion in salaries and wages across 27,786 ZIP codes
- Census data: 312,444,427 people in 33,119 ZIP codes (U.S. and Puerto Rico, 2010), 50.9% female
- Highest per capita income: Montchanin, Delaware ($654,485), a ZIP code with 68 residents; 28 of the top 100 ZIP codes have fewer than 1,000 people
- New York and California each have 18 of the top 100
- Per capita income (Wikipedia) and average wages per return (IRS) barely agree (correlation 0.05), because they measure different things in different years

## Files
| File | What it is |
|---|---|
| Notebooks `1` to `4` | The pipeline |
| `Data/` | Source files and the cleaned outputs (`TopIncome_wiki.csv` is the July 2024 web scrape used in the merge). The two largest source files are zipped: `2010 Income Tax.zip` and `population_by_zip_2010.zip` |
| `ORIGINAL_DATA_NOTE.md` | Sizes of the large source files |

## Tools
Python (pandas, requests, BeautifulSoup, Kaggle API, json, sqlite3, matplotlib)

## How to run
Unzip the two zip files in `Data/`, then run the notebooks in order from this folder. Notebook 2 needs internet access; notebook 3 only calls the Kaggle API if a `Kaggle.json` key file is present and otherwise uses the included CSV.

## Context
Master's coursework (DSC540 term project), cleaned up and corrected for this portfolio.

## Portfolio cleanup and corrections (November 2026)
This cleanup was done with help from Claude (Anthropic).

- **Corrected the merge.** The original joined raw tables with several rows per ZIP code (six income brackets; many age and gender rows) and grouped without totals, so each ZIP code got one arbitrary row's wages and population. Each source is now reduced to one row per ZIP code before the merge.
- **Corrected the census totals.** The census file lists each ZIP code three ways. The original kept every row, so population by gender added up to 625 million, about twice the U.S. population. Only the total rows are used now (312.4 million).
- **Corrected the IRS data.** Column N1 is the number of returns, not exemptions; state-total rows (ZIP code 00000) were mixed in with real ZIP codes; and dollar amounts are in thousands. All three are now handled.
- **Fixed the web scrape.** Wikipedia now blocks requests without a User-Agent and has changed the table layout, so the original scrape failed. The scraper now identifies itself and finds the table by its headers. The merge uses the July 2024 scrape, which still has place names.
- **Corrected the top-10 chart.** The original added per capita incomes across ZIP codes in the same city (for example "Houston $368,259"); ZIP codes are now charted individually.
- The Kaggle API steps only run when the key file is present; all API and JSON code is still shown
- Every number was checked against a fresh run
