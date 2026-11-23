/*
Telecom Customer Churn: Oracle version
--------------------------------------
The same segmentation queries as section 2 of "Telecom Churn SQL and Predictive Model.ipynb",
written in Oracle SQL, plus the risk score queries from section 4.
Requires Oracle 12.2 or later (FETCH FIRST and TO_NUMBER ... DEFAULT NULL ON CONVERSION ERROR).

Setup (Oracle SQL Developer):
  1. Right-click Tables > Import Data
       - WA_Fn-UseC_-Telco-Customer-Churn.csv  -> table telco_customers
       - churn_risk_scores.csv (exported by the notebook) -> table churn_risk_scores
     Set text columns (and TotalCharges) to VARCHAR2(50) so values like
     "No internet service" are not cut off.
  2. Run this script (F5 runs the whole script).

Differences from the SQL Server version:
  - CREATE OR REPLACE VIEW instead of DROP VIEW IF EXISTS + GO
  - FETCH FIRST 10 ROWS ONLY instead of TOP 10
  - TO_NUMBER(... DEFAULT NULL ON CONVERSION ERROR) instead of TRY_CAST
  - Oracle's AVG already returns decimals, so no 1.0 * is needed
  - SELECT *, other_column needs a table alias in Oracle (s.*)
  - No dbo. schema prefix
*/


/* ---------- 1. Feature view: cleans the data and adds tenure_band, num_addons, churn_flag ---------- */
CREATE OR REPLACE VIEW customer_features AS
SELECT
    customerID,
    gender,
    SeniorCitizen,
    Partner,
    Dependents,
    tenure,
    PhoneService,
    MultipleLines,
    InternetService,
    OnlineSecurity,
    OnlineBackup,
    DeviceProtection,
    TechSupport,
    StreamingTV,
    StreamingMovies,
    Contract,
    PaperlessBilling,
    PaymentMethod,
    CAST(MonthlyCharges AS NUMBER(10, 2))                           AS MonthlyCharges,
    TO_NUMBER(TotalCharges DEFAULT NULL ON CONVERSION ERROR)        AS TotalCharges,
    Churn,
    CASE
        WHEN tenure <= 6  THEN '01: 0-6 months'
        WHEN tenure <= 12 THEN '02: 7-12 months'
        WHEN tenure <= 24 THEN '03: 13-24 months'
        WHEN tenure <= 48 THEN '04: 25-48 months'
        ELSE                   '05: 49-72 months'
    END AS tenure_band,
    (CASE WHEN OnlineSecurity   = 'Yes' THEN 1 ELSE 0 END
   + CASE WHEN OnlineBackup     = 'Yes' THEN 1 ELSE 0 END
   + CASE WHEN DeviceProtection = 'Yes' THEN 1 ELSE 0 END
   + CASE WHEN TechSupport      = 'Yes' THEN 1 ELSE 0 END
   + CASE WHEN StreamingTV      = 'Yes' THEN 1 ELSE 0 END
   + CASE WHEN StreamingMovies  = 'Yes' THEN 1 ELSE 0 END) AS num_addons,
    CASE WHEN Churn = 'Yes' THEN 1 ELSE 0 END AS churn_flag
FROM telco_customers
WHERE tenure > 0;   -- removes the 11 brand new customers who have not been billed yet


/* ---------- 2. Overall churn and monthly revenue lost ---------- */
SELECT
    COUNT(*)                                            AS customers,
    SUM(churn_flag)                                     AS churned,
    ROUND(100 * AVG(churn_flag), 1)                     AS churn_rate_pct,
    ROUND(SUM(MonthlyCharges * churn_flag), 0)          AS monthly_revenue_lost,
    ROUND(SUM(MonthlyCharges * churn_flag) * 12, 0)     AS annualized_revenue_lost
FROM customer_features;


/* ---------- 3. Churn by contract type ---------- */
-- SUM(SUM(...)) OVER () gives each contract type's share of all churners
SELECT
    Contract,
    COUNT(*)                                                             AS customers,
    SUM(churn_flag)                                                      AS churned,
    ROUND(100 * AVG(churn_flag), 1)                                      AS churn_rate_pct,
    ROUND(100 * SUM(churn_flag) / SUM(SUM(churn_flag)) OVER (), 1)       AS pct_of_all_churn,
    ROUND(SUM(MonthlyCharges * churn_flag), 0)                           AS monthly_revenue_lost
FROM customer_features
GROUP BY Contract
ORDER BY churn_rate_pct DESC;


/* ---------- 4. Churn by tenure band ---------- */
SELECT
    tenure_band,
    COUNT(*)                                                             AS customers,
    SUM(churn_flag)                                                      AS churned,
    ROUND(100 * AVG(churn_flag), 1)                                      AS churn_rate_pct,
    ROUND(100 * SUM(churn_flag) / SUM(SUM(churn_flag)) OVER (), 1)       AS pct_of_all_churn
FROM customer_features
GROUP BY tenure_band
ORDER BY tenure_band;


/* ---------- 5. Churn by payment method ---------- */
SELECT
    PaymentMethod,
    COUNT(*)                            AS customers,
    ROUND(100 * AVG(churn_flag), 1)     AS churn_rate_pct
FROM customer_features
GROUP BY PaymentMethod
ORDER BY churn_rate_pct DESC;


/* ---------- 6. Churn by engagement (number of add-on services), internet customers only ---------- */
SELECT
    num_addons,
    COUNT(*)                            AS customers,
    ROUND(100 * AVG(churn_flag), 1)     AS churn_rate_pct,
    ROUND(AVG(MonthlyCharges), 2)       AS avg_monthly_charges
FROM customer_features
WHERE InternetService <> 'No'
GROUP BY num_addons
ORDER BY num_addons;


/* ---------- 7. Highest-cost segments: contract x tenure, ranked by monthly revenue lost ---------- */
WITH segments AS (
    SELECT
        Contract,
        tenure_band,
        COUNT(*)                                    AS customers,
        ROUND(100 * AVG(churn_flag), 1)             AS churn_rate_pct,
        ROUND(SUM(MonthlyCharges * churn_flag), 0)  AS monthly_revenue_lost
    FROM customer_features
    GROUP BY Contract, tenure_band
    HAVING COUNT(*) >= 100
)
SELECT
    s.*,
    ROUND(100 * s.monthly_revenue_lost / SUM(s.monthly_revenue_lost) OVER (), 1)  AS pct_of_revenue_lost,
    RANK() OVER (ORDER BY s.monthly_revenue_lost DESC)                            AS revenue_lost_rank
FROM segments s
ORDER BY revenue_lost_rank
FETCH FIRST 10 ROWS ONLY;


/* ---------- 8. Risk scores (from churn_risk_scores.csv exported by the notebook) ---------- */

-- do the risk tiers line up with what actually happened?
SELECT
    risk_tier,
    COUNT(*)                                                        AS customers,
    ROUND(100 * AVG(CASE WHEN Churn = 'Yes' THEN 1 ELSE 0 END), 1)  AS actual_churn_rate_pct
FROM churn_risk_scores
GROUP BY risk_tier
ORDER BY risk_tier;

-- active customers only: how much revenue is at risk in each tier?
SELECT
    risk_tier,
    COUNT(*)                                    AS active_customers,
    ROUND(AVG(churn_prob) * 100, 1)             AS avg_churn_prob_pct,
    ROUND(SUM(MonthlyCharges), 0)               AS monthly_revenue,
    ROUND(SUM(expected_revenue_at_risk), 0)     AS expected_monthly_revenue_at_risk
FROM churn_risk_scores
WHERE Churn = 'No'
GROUP BY risk_tier
ORDER BY risk_tier;

-- the retention team's call list: top 10 active customers by expected revenue at risk
SELECT
    customerID,
    Contract,
    tenure,
    MonthlyCharges,
    ROUND(churn_prob * 100, 1)              AS churn_prob_pct,
    ROUND(expected_revenue_at_risk, 2)      AS expected_revenue_at_risk
FROM churn_risk_scores
WHERE Churn = 'No'
ORDER BY expected_revenue_at_risk DESC
FETCH FIRST 10 ROWS ONLY;
