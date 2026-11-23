# Bob's Fixit: Database Design Case Study

Designs a SQL Server database for a small home repair business, answers business questions with SQL, and adds a star schema for reporting on bids.

## What it covers
- **Transactional database (9 tables, 2 views):** customers, bids, repairs, materials, suppliers, invoices, customer and supplier payments, and a many-to-many link between materials and suppliers
- **Design features:** primary and foreign keys, a composite key, a persisted calculated column, a default value, and views that total what each customer owes Bob and what Bob owes each supplier
- **Business questions:** recent bids, customer and supplier balances, materials for long jobs, materials bought from one supplier
- **Star schema:** a `FactBid` fact table with date and customer location dimensions, queried with GROUP BY, HAVING and ROLLUP subtotals

## Scripts (run in order)
| Script | What it does |
|---|---|
| `01_create_tables.sql` | Creates the tables and the two balance views (drops them first, so it can be rerun) |
| `02_insert_sample_data.sql` | Adds fictional sample data and prints the row counts |
| `03_business_questions.sql` | Answers the five business questions |
| `04_star_schema.sql` | Creates and loads the star schema |
| `05_star_schema_queries.sql` | Reporting queries, including two ROLLUP examples |

## Results on the sample data
- Every customer submitted a bid in January 2025. Balances due range from $50 to $500 (Jane Carter), so no customer is over the $500 threshold.
- Bob owes Sherwin Williams Paint $240 and Home Depot $56, so no supplier is over the $1,000 threshold.
- No repair is over 40 hours; materials bought from Home Depot are nails and 2x4 lumber.
- In the star schema, total bids are $2,551.50: Georgia $1,500.50, Texas $1,001.00 and California $50.00.

The sample data has only five rows per table, so several questions correctly return no rows. The queries are written to work on a full data set.

## Files
| File | What it is |
|---|---|
| `01` to `05` `.sql` | The scripts above |
| `Case Study SQL.xlsx` | Supporting design document |

## Tools
Microsoft SQL Server (T-SQL), SQL Server Management Studio

## How to run
In SSMS (or `sqlcmd -I`), create an empty database and run the five scripts in order. All five were tested on SQL Server Express.

## Context
Master's coursework (CIS535), cleaned up and corrected for this portfolio.

## Portfolio cleanup and corrections (November 2026)
This cleanup was done with help from Claude (Anthropic).

- **Reorganized six working scripts into five that run in order.** The original create script referenced the REPAIRS table before creating it, dropped every table partway through, and inserted rows that did not match the columns.
- **Fixed the material-supplier links.** The sample rows pointed to materials 6 and 7, which do not exist; they now match the supplier each material was bought from.
- **Fixed the supplier balance question.** The original joined supplier IDs to customer invoices. A new `SUPPLIER_BALANCES` view uses materials purchased minus supplier payments.
- **Made the customer balance view safe for real data.** Invoices and payments are totaled separately before joining, so a customer with several of each is not double counted.
- **Fixed "last month."** `MONTH(GETDATE()) - 1` fails in January; the query now uses a date range, plus a fixed January 2025 version for the sample data.
- Removed duplicate CREATE statements, scratch SELECTs and a typo in a sample email address; added comments and the year to the monthly totals
- Every result above comes from running the scripts on SQL Server Express
