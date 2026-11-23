# Telecom Customer Churn: SQL Segmentation and Predictive Modeling

This project identifies which customers are likely to cancel their service, explains what drives churn, and produces a prioritized list of at-risk customers for a retention team.

## Business questions
1. **What is happening?** Which customer segments churn the most, and how much monthly revenue does that cost? (SQL)
2. **Can we see it coming?** Can we predict which current customers are likely to churn? (Python)
3. **What should we do?** Which customers should a retention team contact first? (risk score x revenue)

## Data
IBM Telco Customer Churn sample dataset: 7,043 customers, 21 columns, one row per customer. 11 brand new customers (0 months tenure, not yet billed) were removed, leaving 7,032.

## Tools
Python (pandas, NumPy, scikit-learn, matplotlib, seaborn), SQLite via Python's built-in `sqlite3`, SQL Server, Oracle, Power BI, Jupyter Notebook

## Approach
**SQL segmentation** (notebook section 2)
- Loaded the cleaned data into SQLite and created a feature view (tenure bands, add-on service count)
- Analyzed churn rate and lost revenue by contract type, tenure, payment method and engagement
- Used CTEs and window functions (`SUM() OVER ()`, `RANK()`) to find each segment's share of churn and its revenue-lost rank

**Predictive modeling** (section 3)
- 80/20 stratified train/test split
- Compared a baseline (always predicts "no churn"), Logistic Regression, a Decision Tree and Random Forest
- Handled class imbalance (about 27% churn) with `class_weight='balanced'`
- Tuned the Random Forest with GridSearchCV (5-fold cross validation, ROC AUC)
- Evaluated with accuracy, precision, recall, F1, ROC AUC, confusion matrix and a threshold analysis
- Explained churn drivers with logistic regression odds ratios and random forest feature importance

**Risk scoring** (section 4)
- Scored every customer with 5-fold cross-validated probabilities
- Calculated expected revenue at risk (churn probability x monthly charges)
- Wrote the scores back to SQL to build risk tiers and a prioritized call list

## Key results
- 26.6% of customers churned, about $139K in monthly charges (roughly $1.67M a year)
- Month-to-month customers churn at 42.7% and make up 88.6% of all churners
- Churn is 53.3% in the first 6 months
- Chosen model: Logistic Regression. It catches 78.6% of churners (ROC AUC 0.833), close to the tuned Random Forest (79.1%, 0.837), and it is easier to explain and deploy.
- Risk tiers line up with actual outcomes: 62.6% of High-risk customers churned, compared with 7.2% of Low-risk
- 655 active customers are High risk, paying $53.1K a month

## Files
| File | What it is |
|---|---|
| `Telecom Churn SQL and Predictive Model.ipynb` | Main notebook: SQL segmentation, modeling, risk scoring, recommendations |
| `Telecom Churn SQL Server.sql` | The SQL queries rewritten for SQL Server (T-SQL) |
| `Telecom Churn SQL Server - import checks.sql` | Row and value counts to confirm the SQL Server import |
| `Telecom Churn Oracle.sql` | The SQL queries rewritten for Oracle |
| `WA_Fn-UseC_-Telco-Customer-Churn.csv` | The dataset |
| `churn_risk_scores.csv` | Customer-level risk scores exported by the notebook (data source for Power BI) |
| `Retail Churn Risk Report.pbix` / `.pdf` | Power BI dashboard and a PDF export of it |
| `requirements.txt` | Python package versions used |

## How to run
Install the packages in `requirements.txt`, keep the CSV in the same folder as the notebook, and run all cells. The notebook rewrites `churn_risk_scores.csv`.

## Updates from the original coursework version
- Added the SQL segmentation and risk-scoring sections
- Added a baseline model, Logistic Regression, class balancing and GridSearchCV tuning
- Removed overlapping features (TotalCharges, MonthlyCharges) from the model so the drivers are easier to interpret
- Corrected the contract-type interpretation: month-to-month contracts are the strongest churn signal

## Portfolio cleanup (November 2026)
This cleanup was done with help from Claude (Anthropic).

- Expanded the notebook's opening summary with approach, key results, context and how to run
- Reran the notebook top to bottom; every number in the write-up was checked against the fresh output
- Replaced an older 26-column `churn_risk_scores.csv` with the current 28-column export (identical to the file the Power BI report uses)
- Added a header to the SQL Server import check script and gave it a clearer name
- Added `requirements.txt`
- Updated the tuned random forest results (79.1% of churners caught, ROC AUC 0.837) and the call list values to match a fresh rerun. The earlier figures came from a run in a different Python version; the chosen logistic regression results did not change.
