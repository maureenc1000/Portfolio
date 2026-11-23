/*
Telecom Customer Churn: SQL Server version
------------------------------------------
The same segmentation queries as section 2 of "Telecom Churn SQL and Predictive Model.ipynb",
written in T-SQL, plus the risk score queries from section 4.

Setup (SQL Server Management Studio):
  1. Right-click your database > Tasks > Import Flat File
       - WA_Fn-UseC_-Telco-Customer-Churn.csv  -> table dbo.telco_customers
       - churn_risk_scores.csv (exported by the notebook) -> table dbo.churn_risk_scores
  2. Run this script.

Differences from the SQLite version:
  - TOP instead of LIMIT
  - AVG of a whole number returns a whole number in SQL Server, so churn rates use AVG(1.0 * churn_flag)
  - TRY_CAST handles the blank TotalCharges values
  - CREATE VIEW has to be the only statement in its batch, so it is wrapped in GO
*/

-- run everything in the churnDB database
USE churnDB;
GO


/* ---------- 1. Feature view: cleans the data and adds tenure_band, num_addons, churn_flag ---------- */
DROP VIEW IF EXISTS dbo.customer_features;
GO

-- Import Flat File may load Yes/No columns as bit (1/0) instead of text.
-- CAST(... AS NVARCHAR(10)) IN ('Yes', '1') works either way, and the view returns them as Yes/No.
CREATE VIEW dbo.customer_features AS
SELECT
    customerID,
    gender,
    SeniorCitizen,
    CASE WHEN CAST(Partner AS NVARCHAR(10)) IN ('Yes', '1') THEN 'Yes' ELSE 'No' END AS Partner,
    CASE WHEN CAST(Dependents AS NVARCHAR(10)) IN ('Yes', '1') THEN 'Yes' ELSE 'No' END AS Dependents,
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
    CASE WHEN CAST(PaperlessBilling AS NVARCHAR(10)) IN ('Yes', '1') THEN 'Yes' ELSE 'No' END AS PaperlessBilling,
    PaymentMethod,
    CAST(MonthlyCharges AS DECIMAL(10, 2))          AS MonthlyCharges,
    TRY_CAST(TotalCharges AS DECIMAL(10, 2))        AS TotalCharges,
    CASE WHEN CAST(Churn AS NVARCHAR(10)) IN ('Yes', '1') THEN 'Yes' ELSE 'No' END AS Churn,
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
    CASE WHEN CAST(Churn AS NVARCHAR(10)) IN ('Yes', '1') THEN 1 ELSE 0 END AS churn_flag
FROM dbo.telco_customers
WHERE tenure > 0;   -- removes the 11 brand new customers who have not been billed yet
GO


/* ---------- 2. Overall churn and monthly revenue lost ---------- */
SELECT
    COUNT(*)                                            AS customers,
    SUM(churn_flag)                                     AS churned,
    ROUND(100.0 * AVG(1.0 * churn_flag), 1)             AS churn_rate_pct,
    ROUND(SUM(MonthlyCharges * churn_flag), 0)          AS monthly_revenue_lost,
    ROUND(SUM(MonthlyCharges * churn_flag) * 12, 0)     AS annualized_revenue_lost
FROM dbo.customer_features;


/* ---------- 3. Churn by contract type ---------- */
-- SUM(SUM(...)) OVER () gives each contract type's share of all churners
SELECT
    Contract,
    COUNT(*)                                                             AS customers,
    SUM(churn_flag)                                                      AS churned,
    ROUND(100.0 * AVG(1.0 * churn_flag), 1)                              AS churn_rate_pct,
    ROUND(100.0 * SUM(churn_flag) / SUM(SUM(churn_flag)) OVER (), 1)     AS pct_of_all_churn,
    ROUND(SUM(MonthlyCharges * churn_flag), 0)                           AS monthly_revenue_lost
FROM dbo.customer_features
GROUP BY Contract
ORDER BY churn_rate_pct DESC;


/* ---------- 4. Churn by tenure band ---------- */
SELECT
    tenure_band,
    COUNT(*)                                                             AS customers,
    SUM(churn_flag)                                                      AS churned,
    ROUND(100.0 * AVG(1.0 * churn_flag), 1)                              AS churn_rate_pct,
    ROUND(100.0 * SUM(churn_flag) / SUM(SUM(churn_flag)) OVER (), 1)     AS pct_of_all_churn
FROM dbo.customer_features
GROUP BY tenure_band
ORDER BY tenure_band;


/* ---------- 5. Churn by payment method ---------- */
SELECT
    PaymentMethod,
    COUNT(*)                                    AS customers,
    ROUND(100.0 * AVG(1.0 * churn_flag), 1)     AS churn_rate_pct
FROM dbo.customer_features
GROUP BY PaymentMethod
ORDER BY churn_rate_pct DESC;


/* ---------- 6. Churn by engagement (number of add-on services), internet customers only ---------- */
SELECT
    num_addons,
    COUNT(*)                                    AS customers,
    ROUND(100.0 * AVG(1.0 * churn_flag), 1)     AS churn_rate_pct,
    ROUND(AVG(MonthlyCharges), 2)               AS avg_monthly_charges
FROM dbo.customer_features
WHERE InternetService <> 'No'
GROUP BY num_addons
ORDER BY num_addons;


/* ---------- 7. Highest-cost segments: contract x tenure, ranked by monthly revenue lost ---------- */
WITH segments AS (
    SELECT
        Contract,
        tenure_band,
        COUNT(*)                                    AS customers,
        ROUND(100.0 * AVG(1.0 * churn_flag), 1)     AS churn_rate_pct,
        ROUND(SUM(MonthlyCharges * churn_flag), 0)  AS monthly_revenue_lost
    FROM dbo.customer_features
    GROUP BY Contract, tenure_band
    HAVING COUNT(*) >= 100
)
SELECT TOP 10
    *,
    ROUND(100.0 * monthly_revenue_lost / SUM(monthly_revenue_lost) OVER (), 1)  AS pct_of_revenue_lost,
    RANK() OVER (ORDER BY monthly_revenue_lost DESC)                            AS revenue_lost_rank
FROM segments
ORDER BY revenue_lost_rank;


/* ---------- 8. Risk scores (from churn_risk_scores.csv exported by the notebook) ---------- */
-- joined to customer_features on customerID so churn status comes from the cleaned view

-- do the risk tiers line up with what actually happened?
SELECT
    r.risk_tier,
    COUNT(*)                                    AS customers,
    ROUND(100.0 * AVG(1.0 * c.churn_flag), 1)   AS actual_churn_rate_pct
FROM dbo.churn_risk_scores AS r
JOIN dbo.customer_features AS c ON r.customerID = c.customerID
GROUP BY r.risk_tier
ORDER BY r.risk_tier;

-- active customers only: how much revenue is at risk in each tier?
SELECT
    r.risk_tier,
    COUNT(*)                                        AS active_customers,
    ROUND(AVG(r.churn_prob) * 100, 1)               AS avg_churn_prob_pct,
    ROUND(SUM(c.MonthlyCharges), 0)                 AS monthly_revenue,
    ROUND(SUM(r.expected_revenue_at_risk), 0)       AS expected_monthly_revenue_at_risk
FROM dbo.churn_risk_scores AS r
JOIN dbo.customer_features AS c ON r.customerID = c.customerID
WHERE c.churn_flag = 0
GROUP BY r.risk_tier
ORDER BY r.risk_tier;

-- the retention team's call list: top 10 active customers by expected revenue at risk
SELECT TOP 10
    c.customerID,
    c.Contract,
    c.tenure,
    c.MonthlyCharges,
    ROUND(r.churn_prob * 100, 1)            AS churn_prob_pct,
    ROUND(r.expected_revenue_at_risk, 2)    AS expected_revenue_at_risk
FROM dbo.churn_risk_scores AS r
JOIN dbo.customer_features AS c ON r.customerID = c.customerID
WHERE c.churn_flag = 0
ORDER BY r.expected_revenue_at_risk DESC;
