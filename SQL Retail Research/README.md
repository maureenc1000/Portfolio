# Retail Research Project: SQL for Sales Decisions

Uses SQL Server to answer seven questions from a sales department about a bike retailer's customers, orders and discounts.

## Data
Three tables loaded by `01_load_retail_data.sql`: 1,445 customers, 1,615 orders and 4,722 order items (fictional sample data).

## Questions and answers
| # | Question | Answer |
|---|---|---|
| 1 | Which state should a new ad campaign target? | **New York**: $5.22 million in revenue from 3,195 order items, far ahead of California ($1.61 million) and Texas ($0.87 million). Texas would be the choice if the goal were growing the weakest market. |
| 2 | Customer area codes, in descending order | 26 distinct area codes, from 916 down to 210 |
| 3 | Area code or state for targeting? | **State.** Every customer has a state, but only 178 of 1,445 have a phone number (1,267 are blank), and phone numbers move with people. |
| 4 | How many customers have more than two orders? (loyalty program cost) | **39** customers, each with 3 orders |
| 5 | Three months with the fewest orders | **November (100), December (103), July (106)** |
| 6 | Is the rule "list price over $500 needs at least a 20% discount" being followed? | **No.** 2,067 of 4,722 order items (43.8%) break it. |
| 7 | How many items have an above-average discount, and is there a pattern? | The average discount is 10.54%; **1,203** items are above it, and all of them are at exactly 20%, the maximum. New York has the most (844), in line with its share of sales. |

## Files
| File | What it is |
|---|---|
| `01_load_retail_data.sql` | Creates and loads the three tables |
| `02_research_questions.sql` | The SQL for all seven questions |

## Tools
Microsoft SQL Server (T-SQL): joins, GROUP BY and HAVING, subqueries, CASE, string and date functions

## How to run
In SSMS (or `sqlcmd -I`), create an empty database, run `01_load_retail_data.sql`, then `02_research_questions.sql`. Both were tested on SQL Server Express.

## Context
Master's coursework (CIS535, Week 9 research project), cleaned up for this portfolio.

## Portfolio cleanup (November 2026)
This cleanup was done with help from Claude (Anthropic).

- Removed a final query that referenced a table that does not exist (`items`), so the script now runs from start to finish
- Fixed the question 7 query: it was labeled as the average discount but calculated the average of only the above-average items (20%). It now shows the overall average (10.54%), the count above it (1,203) and their average.
- Replaced three full-table SELECT statements with five-row previews and added a header
- Every answer above matches both the script output and the original write-up
