# Dodgers Attendance: What Fills the Stadium?

Uses one season of Dodgers home games to find what drives attendance and recommend how to raise it.

## Data
81 home games (`dodgers-2022.csv`): attendance, day of the week, opponent, temperature, sky conditions, day or night, and four promotions (bobblehead, shirt, fireworks, cap).

## Key results
- Bobblehead games averaged **53,145** fans, compared with 38,201 for games with no promotion.
- The effect holds on the same day of the week: bobblehead games drew **9,565 to 19,010 more fans** on each of the four days that had both.
- Monday is the weakest day (34,966 on average) and never had a bobblehead night.
- Fireworks nights can't be judged on their own: 13 of the 14 were Fridays.

**Recommendation:** test bobblehead promotions on Mondays and compare against other Monday games.

## Files
| File | What it is |
|---|---|
| `Dodgers_Attendance_Analysis.ipynb` | The analysis |
| `dodgers-2022.csv` | The data |

## Tools
Python (pandas, matplotlib, seaborn)

## How to run
Keep `dodgers-2022.csv` in this folder and run the notebook from top to bottom.

## Context
Master's coursework (DSC630, Week 3), cleaned up and corrected for this portfolio.

## Portfolio cleanup and corrections (November 2026)
This cleanup was done with help from Claude (Anthropic).

- **Corrected the promotion ranking.** The original said fireworks ranked second. By average attendance, shirts were second (46,644, from only 3 games) and fireworks third (41,078).
- Added a same-day comparison (promotion vs. no promotion on the same day of the week), which shows the bobblehead lift is real and that fireworks are tied to Fridays
- Added game counts to the promotion chart and a no-promotion reference line
- Added a summary, findings and limitations; removed an empty chart cell and an unnecessary cleaning step
- Every number was checked against a fresh run
