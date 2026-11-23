/*
Bob's Fixit: business questions
Run after 01 and 02.
*/

-- 1. List the names of the customers who provided bids last month.
--    "Last month" is the calendar month before today, so the result depends on the date the query runs.
--    Using a date range (rather than MONTH(GETDATE()) - 1) also works in January.
DECLARE @first_of_this_month DATE = DATEFROMPARTS(YEAR(GETDATE()), MONTH(GETDATE()), 1);
SELECT DISTINCT C.CUSTOMER_NAME
FROM CUSTOMERS AS C
JOIN BIDS AS B ON C.CUSTOMER_ID = B.CUSTOMER_ID
WHERE B.BID_DATE >= DATEADD(MONTH, -1, @first_of_this_month)
  AND B.BID_DATE <  @first_of_this_month;

--    The same question for a fixed month, January 2025, when the sample bids were made:
SELECT DISTINCT C.CUSTOMER_NAME
FROM CUSTOMERS AS C
JOIN BIDS AS B ON C.CUSTOMER_ID = B.CUSTOMER_ID
WHERE B.BID_DATE >= '2025-01-01' AND B.BID_DATE < '2025-02-01'
ORDER BY C.CUSTOMER_NAME;

-- 2. List all the customers that have an outstanding balance (amount due to Bob) greater than $500.
SELECT CUSTOMER_NAME, TOTAL_BILLED, TOTAL_CUSTOMER_PAYMENTS, BALANCE_DUE
FROM CUSTOMER_BALANCES
WHERE BALANCE_DUE > 500;

--    All balances, for context:
SELECT CUSTOMER_NAME, BALANCE_DUE FROM CUSTOMER_BALANCES ORDER BY BALANCE_DUE DESC;

-- 3. List all the suppliers where Bob has an outstanding balance greater than $1,000.
--    Bob's purchases from each supplier come from MATERIALS (quantity x unit cost).
SELECT SUPPLIER_NAME, TOTAL_PURCHASED, TOTAL_PAID, BALANCE_OWED
FROM SUPPLIER_BALANCES
WHERE BALANCE_OWED > 1000;

--    All supplier balances, for context:
SELECT SUPPLIER_NAME, TOTAL_PURCHASED, TOTAL_PAID, BALANCE_OWED FROM SUPPLIER_BALANCES ORDER BY BALANCE_OWED DESC;

-- 4. List all the material used for bids where the estimated hours were greater than 40.
SELECT M.MATERIAL_NAME, M.MATERIAL_QTY, M.MATERIAL_UNIT_COST, R.REPAIR_HOURS
FROM MATERIALS AS M
JOIN REPAIRS AS R ON M.REPAIR_ID = R.REPAIR_ID
WHERE R.REPAIR_HOURS > 40;

-- 5. List all the material Bob purchased from one supplier (Home Depot).
SELECT M.MATERIAL_NAME, M.MATERIAL_QTY, M.MATERIAL_UNIT_COST, S.SUPPLIER_NAME
FROM MATERIALS AS M
JOIN SUPPLIERS AS S ON M.SUPPLIER_ID = S.SUPPLIER_ID
WHERE S.SUPPLIER_NAME = 'Home Depot';
