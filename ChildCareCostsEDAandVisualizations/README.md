# The Cost of Childcare in the U.S.

Visualizes center-based childcare prices across U.S. states and over time, and compares them with household income.

## Data
National Database of Childcare Prices (U.S. Department of Labor): 34,567 county-year rows, 3,144 counties in all 50 states and DC, 2008 to 2018 (`nationaldatabaseofchildcareprices_raw.xlsx`).

## Key results
- Weekly infant care at a center averaged **$304 in DC** and **$302 in Massachusetts**, compared with **$79 in Mississippi** (2008 to 2018 average).
- From 2008 to 2018, average weekly prices rose **21.7% to 25.5%** across age groups, while median household income rose **3.8%** (prices are not inflation-adjusted; income is in 2018 dollars).
- In 2018, a year of infant care cost on average **16.2%** of median household income, and **23.6%** in Massachusetts.

## Visuals
1. Interactive state map of average weekly price (Plotly; interactive when the notebook is opened in Jupyter)
2. Top 10 states by infant care price, with household income
3. National price trend by age group
4. Overall price vs. household income over time

A Tableau version is in `Milestone5.twbx`.

## Files
| File | What it is |
|---|---|
| `Child_Care_Costs_Analysis.ipynb` | The analysis and charts |
| `nationaldatabaseofchildcareprices_raw.xlsx` | The data the notebook uses (not included, too large; see How to run) |
| `nationaldatabaseofchildcareprices_milestone.xlsx` | Data source for the Tableau workbooks |
| `Milestone5.twbx`, `Milestone3.twb`, `Milestone5.twb` | Tableau workbooks (the `.twb` files point to the data file's original location, so open the packaged `.twbx`) |

## Tools
Python (pandas, matplotlib, seaborn, Plotly), Tableau

## How to run
The data file is too large for this repository. Download "National Database of Childcare Prices: 2008-2018 (Data File)" from the U.S. Department of Labor (https://www.dol.gov/agencies/wb/topics/featured-childcare), save its `nationaldatabaseofchildcare` sheet in this folder as `nationaldatabaseofchildcareprices_raw.xlsx`, and run the notebook from top to bottom (loading the workbook takes about a minute).

## Context
Master's coursework (DSC640 term project), cleaned up and corrected for this portfolio.

## Portfolio cleanup and corrections (November 2026)
This cleanup was done with help from Claude (Anthropic).

- Combined four draft notebooks into one that runs from top to bottom (two of the drafts failed because they used a variable before defining it)
- Added a summary, printed key numbers and written findings (the drafts had none)
- **Corrected the income comparison:** prices are in each year's dollars and income is in 2018 dollars, so share-of-income figures now use 2018 only, and the findings note that part of the price increase is inflation
- Removed duplicated setup cells, a step that opened the Excel file twice, and assignment instructions
- Every number was checked against a fresh run
