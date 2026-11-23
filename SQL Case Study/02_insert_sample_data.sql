/*
Bob's Fixit: sample data (fictional customers and suppliers; 555 phone numbers)
Run after 01_create_tables.sql.
*/

INSERT INTO CUSTOMERS (CUSTOMER_NAME, CUSTOMER_EMAIL, CUSTOMER_ADDRESS, CUSTOMER_PHONE) VALUES
 ('Bob Smith',     'bobsmith@gmail.com',   '100 Main St, Miami, FL, 12345',    '2125551212')
,('Jane Carter',   'janecarter@gmail.com', '2500 Lee Ln, Miami, FL, 54321',    '2105551212')
,('Sally Adams',   'sadams@gmail.com',     '300 Spruce Blvd, Miami, FL, 32145','2125551412')
,('Robert Jones',  'robbie@gmail.com',     '6500 Elm St, Miami, FL, 65421',    '2105551512')
,('Taylor Porter', 'tporter@gmail.com',    '410 Beech Blvd, Miami, FL, 43215', '2125551812');

INSERT INTO BIDS (CUSTOMER_ID, BID_DATE, RATE_PER_HOUR, MATERIAL_COST) VALUES
 (1, '2025-01-02', 50, 100)
,(2, '2025-01-03', 50, 500)
,(3, '2025-01-04', 50, 250)
,(4, '2025-01-05', 50, 50)
,(5, '2025-01-10', 50, 10);

INSERT INTO CUSTOMER_PAYMENTS (CUSTOMER_ID, PAYMENT_DATE, PAYMENT_AMOUNT) VALUES
 (1, '2025-01-22', 20)
,(2, '2025-01-23', 50)
,(3, '2025-01-24', 100)
,(4, '2025-01-25', 10)
,(5, '2025-01-30', 10);

INSERT INTO INVOICES (CUSTOMER_ID, INVOICE_DATE, TOTAL_AMOUNT_DUE) VALUES
 (1, '2025-01-12', 150)
,(2, '2025-01-13', 550)
,(3, '2025-01-14', 300)
,(4, '2025-01-15', 100)
,(5, '2025-01-20', 60);

INSERT INTO SUPPLIERS (SUPPLIER_NAME, SUPPLIER_EMAIL, SUPPLIER_ADDRESS, SUPPLIER_PHONE) VALUES
 ('Home Depot',             'sales@homedepot.com',  '1000 Mountain St, Miami, FL, 12345', '2125552020')
,('Sherwin Williams Paint', 'sales@sherwin.com',    '25 Elm Ln, Miami, FL, 54321',        '2105553030')
,('Plumbing Supply',        'sales@plumbing.com',   '3000 Hickory Blvd, Miami, FL, 32145','2125554040')
,('Electrical Works',       'sales@electrical.com', '650 James St, Miami, FL, 65421',     '2105555050')
,('Windows R Us',           'sales@windows.com',    '4100 First Blvd, Miami, FL, 43215',  '2125556060');

INSERT INTO REPAIRS (BID_ID, REPAIR_DESCRIPTION, REPAIR_HOURS, REPAIR_COST) VALUES
 (1, 'House paint, one room', 3, 250)
,(2, 'Fix wall hole',         1, 100)
,(3, 'Paint two rooms',       7, 450)
,(4, 'Repair stairs',         2, 200)
,(5, 'Repair stairs',         2, 200);

INSERT INTO MATERIALS (REPAIR_ID, SUPPLIER_ID, MATERIAL_NAME, MATERIAL_QTY, MATERIAL_UNIT_COST) VALUES
 (1, 1, 'Nails',                 100, 0.01)
,(2, 1, 'Lumber 2x4',             25, 5)
,(3, 2, 'Paint Primer 1 gallon',   4, 35)
,(4, 2, 'Paint 1 gallon',          8, 25)
,(5, 2, 'Paint Brush 2 inch',     10, 2);

-- each material linked to the supplier it was bought from
INSERT INTO MATERIAL_SUPPLIER (SUPPLIER_ID, MATERIAL_ID) VALUES
 (1, 1)
,(1, 2)
,(2, 3)
,(2, 4)
,(2, 5);

INSERT INTO SUPPLIER_PAYMENTS (SUPPLIER_ID, PAYMENT_DATE, PAYMENT_AMOUNT) VALUES
 (1, '2025-01-22', 20)
,(1, '2025-01-23', 50)
,(2, '2025-01-24', 100)
,(2, '2025-01-25', 10)
,(2, '2025-01-30', 10);

SELECT 'CUSTOMERS' AS table_name, COUNT(*) AS row_count FROM CUSTOMERS
UNION ALL SELECT 'BIDS', COUNT(*) FROM BIDS
UNION ALL SELECT 'INVOICES', COUNT(*) FROM INVOICES
UNION ALL SELECT 'CUSTOMER_PAYMENTS', COUNT(*) FROM CUSTOMER_PAYMENTS
UNION ALL SELECT 'SUPPLIERS', COUNT(*) FROM SUPPLIERS
UNION ALL SELECT 'SUPPLIER_PAYMENTS', COUNT(*) FROM SUPPLIER_PAYMENTS
UNION ALL SELECT 'REPAIRS', COUNT(*) FROM REPAIRS
UNION ALL SELECT 'MATERIALS', COUNT(*) FROM MATERIALS
UNION ALL SELECT 'MATERIAL_SUPPLIER', COUNT(*) FROM MATERIAL_SUPPLIER;
