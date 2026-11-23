# Case Load Balancing Map

Maps caseload by state across three regional teams and suggests which states to reassign so each team carries a fair share of the work.

## Data
`SampleAnon.xlsx`: an anonymized list of 4,144 cases with each case's state and the region the state belongs to (Central, East, West). 17 cases with no state or region are left out.

## Key results
| Region | Cases now | After suggested moves | Even share |
|---|---|---|---|
| Central | 1,224 | 1,341 | 1,376 |
| East | 1,778 | 1,367 | 1,376 |
| West | 1,125 | 1,419 | 1,376 |

- The East region carries 402 more cases than an even share.
- Moving 12 states (411 cases) out of the East brings every region within 43 cases of an even share.
- The method balances counts only; geography and case complexity would need a practical review.

## Files
| File | What it is |
|---|---|
| `Case_Load_Balancing_Map.ipynb` | The analysis, balancing logic and map |
| `case_load_dashboard.py` | Interactive Dash app with a region filter (`python case_load_dashboard.py`, then open http://127.0.0.1:8050) |
| `SampleAnon.xlsx` | Anonymized case data |

## Tools
Python (pandas, Plotly, Dash)

## Context
A work project, built on anonymized sample data.

## Portfolio cleanup (November 2026)
This cleanup was done with help from Claude (Anthropic).

- Combined three working versions into one notebook and a separate Dash app
- Added a before-and-after balance table and written findings
- States are now moved to the most underloaded region first
- Cases with no state or region (17) are reported instead of dropped silently
- Every number was checked against a fresh run
