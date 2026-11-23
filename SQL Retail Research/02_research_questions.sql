/*
Retail research project: business questions (SQL Server)
---------------------------------------------------------
Answers seven questions from the sales department about a bike retailer's customers,
orders and order items. Run 01_load_retail_data.sql first to create and load the tables.
Customer names, emails and addresses in the data are fictional sample data.
*/
-- quick look at the three tables
SELECT TOP 5 * FROM retail_customers;
SELECT TOP 5 * FROM retail_orders;
SELECT TOP 5 * FROM retail_order_items;

--1. The sales department wants to start a new ad campaign but they are not sure what state they want to target for their new campaign. Based on the data what state would you recommend and why? Make sure you include the SQL statements you used to support your research. Your recommendation should be a minimum of one detailed paragraph.
SELECT 
cust."state"
,SUM(items.quantity * items.list_price * (1 - items.discount)) AS Total_Sales_Revenue
,COUNT(items.quantity) AS Total_Sales

FROM retail_customers AS cust

LEFT JOIN retail_orders AS orders
    ON cust.customer_id = orders.customer_id

LEFT JOIN retail_order_items AS items 
    ON orders.order_id = items.order_id

GROUP BY cust.state
ORDER BY Total_Sales_Revenue DESC;


--2. The sales department wants to start a new ad campaign based on the phone number area code for their customers. They need you to return the area codes for all the customers in desc order excluding customers with null area codes. Make sure you include your SQL statement(s).
SELECT DISTINCT 
SUBSTRING(phone, 2, 3) AS AreaCode

FROM retail_customers

WHERE phone IS NOT NULL

ORDER BY AreaCode DESC;


--3. The sales department is not sure if their new ad campaign should be based on phone number area code or state. What approach would you recommend and why? Your recommendation should be a minimum of one detailed paragraph.

SELECT COUNT(CASE WHEN state IS NULL THEN 1 END) AS NullCount
FROM retail_customers;

SELECT COUNT(CASE WHEN phone IS NOT NULL THEN 1 END) AS NotNullCount
FROM retail_customers;

--4. The sales department wants to start a new loyalty program but they are not sure on the cost. To help them understand the cost you need to determine the number of customers that have more than two orders. Hint: Use the having clause. Make sure you include your SQL statement(s).
--to get list of all customers and their order counts if over 2
SELECT 
customer_id
,COUNT(*) AS Order_Count

FROM retail_orders

GROUP BY customer_id
HAVING COUNT(*) > 2;

--to get total count of all customers if their total orders are over 2
SELECT 
COUNT(*) AS "Cust_With>2_Orders"
FROM (
    SELECT customer_id
    FROM retail_orders
    GROUP BY customer_id
    HAVING COUNT(*) > 2
) AS subquery;

--5. The sales department wants to encourage sales during the months when the company has the least number of sales. To help them you need to return the three months (use month names not month numbers) when sales are at their lowest. Make sure you include your SQL statement(s).

SELECT TOP 3 
DATENAME(month, order_date) AS Month_Name,
COUNT(*) AS Order_Count

FROM retail_orders

GROUP BY MONTH(order_date), DATENAME(month, order_date)
ORDER BY Order_Count;

--6. The sales department is concerned with the discount that is getting used for all items sold. The business rule is any list price above $500 should have a minimum discount of .20. Is this business rule getting followed with all order items? If not, how many order items are not following this business rule? What is the overall % of order items not following this business rule? Make sure you include your SQL statement(s).

SELECT COUNT(*) AS NotCompliant_Items
FROM retail_order_items

WHERE 
list_price > 500
AND discount < 0.20;


SELECT 
COUNT(*) AS Total_Items
,SUM(CASE WHEN list_price > 500 AND discount < 0.20 THEN 1 ELSE 0 END) AS NotCompliant_Items
,100.0 * SUM(CASE WHEN list_price > 500 AND discount < 0.20 THEN 1 ELSE 0 END) / COUNT(*) AS "% NotCompliant"
FROM retail_order_items;



--7. The sales department is concerned with the discount that is getting used. They want to know the number of order items that have a discount greater than the average discount. They also want to see if there is any pattern to the discounts that are greater than the average discount. Make sure your answer is a minimum of one detailed paragraph. You also need to include any SQL statements you used to answer this question and any sources you used in your research.

-- overall average discount, and how many order items are above it
SELECT
(SELECT AVG(discount) FROM retail_order_items) * 100 AS Overall_Average_Discount_Pct
,COUNT(*) AS Items_Above_Average_Discount
,AVG(discount) * 100 AS Average_Discount_Of_Those_Items_Pct

FROM retail_order_items

WHERE discount > (SELECT AVG(discount) FROM retail_order_items);


--Additionally, to observe any patterns among these discounts, you might group the discounts to see their distribution:
SELECT 
discount
,COUNT(*) AS Items_Over_Discount_Avg

FROM retail_order_items

WHERE discount > (SELECT AVG(discount) FROM retail_order_items)

GROUP BY discount
ORDER BY discount;

SELECT 
state
,COUNT(quantity) AS Items_Over_Discount_Avg
,SUM(items.quantity * items.list_price * items.discount) AS Total_Discount_Amount
,SUM(items.quantity * items.list_price * (1 - items.discount)) AS Total_Sales_Revenue
,MAX(discount) AS Max_Discount

FROM retail_customers cust
LEFT JOIN retail_orders AS orders
    ON cust.customer_id = orders.customer_id

LEFT JOIN retail_order_items AS items 
    ON orders.order_id = items.order_id

WHERE discount > (SELECT AVG(discount) FROM retail_order_items)

GROUP BY state
ORDER BY Items_Over_Discount_Avg DESC;

