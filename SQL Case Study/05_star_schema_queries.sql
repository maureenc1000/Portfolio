/*
Bob's Fixit: reporting queries on the star schema
Run after 04_star_schema.sql.
*/

-- Total bid amount by customer zip code
SELECT dcl.CustomerZipCode, SUM(fb.Bid_Amount) AS Total_Bid_Amount
FROM FactBid AS fb
JOIN DimCustomerLocation AS dcl ON fb.DimCustomerID = dcl.DimCustomerID
GROUP BY dcl.CustomerZipCode
ORDER BY Total_Bid_Amount DESC;

-- Average bid amount by customer state
SELECT dcl.CustomerState, AVG(fb.Bid_Amount) AS Avg_Bid_Amount
FROM FactBid AS fb
JOIN DimCustomerLocation AS dcl ON fb.DimCustomerID = dcl.DimCustomerID
GROUP BY dcl.CustomerState
ORDER BY Avg_Bid_Amount DESC;

-- Total bid amount by month (year included so months from different years are not combined)
SELECT dd.BidYear, dd.BidMonth, SUM(fb.Bid_Amount) AS Total_Bid_Amount
FROM FactBid AS fb
JOIN DimDate AS dd ON fb.DimDateID = dd.DimDateID
GROUP BY dd.BidYear, dd.BidMonth
ORDER BY dd.BidYear, dd.BidMonth;

-- Zip codes with total bids over $100 (HAVING filters after grouping)
SELECT dcl.CustomerZipCode, SUM(fb.Bid_Amount) AS Total_Bid_Amount
FROM FactBid AS fb
JOIN DimCustomerLocation AS dcl ON fb.DimCustomerID = dcl.DimCustomerID
GROUP BY dcl.CustomerZipCode
HAVING SUM(fb.Bid_Amount) > 100
ORDER BY Total_Bid_Amount DESC;

-- ROLLUP: totals by year and state, with a subtotal for each year and a grand total (NULL rows)
SELECT d.BidYear, c.CustomerState, SUM(f.Bid_Amount) AS Total_Bid_Amount
FROM FactBid AS f
JOIN DimDate AS d ON f.DimDateID = d.DimDateID
JOIN DimCustomerLocation AS c ON f.DimCustomerID = c.DimCustomerID
GROUP BY ROLLUP (d.BidYear, c.CustomerState)
ORDER BY GROUPING(d.BidYear), d.BidYear, GROUPING(c.CustomerState), c.CustomerState;

-- ROLLUP in the other order: subtotals by state instead of by year
SELECT c.CustomerState, d.BidYear, SUM(f.Bid_Amount) AS Total_Bid_Amount
FROM FactBid AS f
JOIN DimDate AS d ON f.DimDateID = d.DimDateID
JOIN DimCustomerLocation AS c ON f.DimCustomerID = c.DimCustomerID
GROUP BY ROLLUP (c.CustomerState, d.BidYear)
ORDER BY GROUPING(c.CustomerState), c.CustomerState, GROUPING(d.BidYear), d.BidYear;
