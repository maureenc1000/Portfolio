/*
Telecom Customer Churn: SQL Server import checks
-------------------------------------------------
Run after importing WA_Fn-UseC_-Telco-Customer-Churn.csv into dbo.telco_customers.
Confirms the import wizard loaded every row and kept the text values intact.
The import wizard can turn Yes/No into bits (1/0), so the Churn check accepts either form.
*/
USE churnDB;
SELECT COUNT(*) AS total_rows FROM dbo.telco_customers;                    -- expect 7043
SELECT Churn, COUNT(*) AS customers FROM dbo.telco_customers GROUP BY Churn; -- expect 1 = 1869, 0 = 5174
SELECT OnlineSecurity, COUNT(*) AS customers FROM dbo.telco_customers GROUP BY OnlineSecurity;
-- expect No 3498, Yes 2019, No internet service 1526