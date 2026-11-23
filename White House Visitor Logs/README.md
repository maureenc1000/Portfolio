# White House Visitor Logs: Access Patterns for Security Analysts

Combines five public White House visitor access files and charts when and where visitors enter the complex, to guide security staffing.

## Audience and purpose
Security analysts reviewing access control and facility use, deciding where and when to add staff and what needs a closer look.

## Data
Five public White House visitor access record (WAVES) files: all of 2021, then June and December of 2022 and 2023 (362,847 rows). The 2021 file uses different column names than the later files, and the files use two date formats.

## Key results
- **255,092** access events with an arrival time, January 2021 to December 2023
- The **East Wing tour** is the busiest destination (124,154 events); the White House itself accounts for 82% of events
- **December is the peak and volume is rising:** 10,856 events in December 2021 vs. 93,757 in December 2023; busiest day December 23, 2023 (6,846)
- **91.5%** of arrivals are between 8 a.m. and 8 p.m.; **83.3%** of visitors came only once
- **Pseudonymous visitor IDs** let analysts follow frequent visitors over time without showing names; a **flag table** narrows 195,121 visitors to **2,147** for review, and **295** meet three rules at once (frequent, spread over 4+ months, 10+ different days)

**Recommendations:** staff most heavily along the East Wing tour route, add coverage for December events, and review the three-flag visitors first.

## Charts (saved to `wh_visit_visualizations/`)
Ten most frequent visitors (by pseudonymous ID), top 10 meeting rooms, locations by month (stacked bars), access events per month, time of day over time (scatter), cumulative access events (step chart) with a daily box plot, and a month-by-month heatmap of the 15 most frequent visitors.

## Files
| File | What it is |
|---|---|
| `White_House_Visitor_Logs.ipynb` | The analysis and charts |
| `wh_visitor_flags.csv` | Flag table: every flagged visitor ID with counts and which rules it meets (no names) |

## Tools
Python (pandas, matplotlib, seaborn)

## How to run
The five source files are not included here because they list every visitor by name. They are public records released by the White House; to rerun the analysis, place them in this folder and run the notebook from top to bottom.

## Context
Master's coursework (DSC640, Weeks 3 and 4), cleaned up and corrected for this portfolio.

## Portfolio cleanup and corrections (November 2026)
This cleanup was done with help from Claude (Anthropic).

- **Corrected the date parsing.** The original read arrival times in one format only. The 2022 and 2023 files use another ("Jun 16 2022  2:13PM"), so 218,443 of their access events were silently dropped, leaving 36,649 events, mostly from 2021. That is why the original found a December 2021 peak and thought data was missing. Both formats are now read, and the true peak is December 2023.
- **Corrected the column merge.** The original joined the old and new column layouts by gluing values together, which split the same location into two categories (for example "WH | " and " | WH"). The layouts are now mapped to one set of column names.
- **Replaced visitor names with pseudonymous IDs** in the charts, as the original write-up recommended for public release, and added a month-by-month view of frequent visitors and a flag table for review
- Months with no data are no longer charted as if they were zero-visit periods
- Every number was checked against a fresh run
