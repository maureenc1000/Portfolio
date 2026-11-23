# Original data (not copied here because of size)

The full Medicaid State Drug Utilization Data file is too large for this folder and for GitHub (over 100 MB). It is kept outside the portfolio, in the original course folder (DSC680 Assignments). To rebuild the two small files, copy it into this folder and rerun the notebook.

| File | Size | Rows | Columns |
|---|---|---|---|
| sdud-2025-updated-dec2025.csv | 238.6 MB | 2,637,009 | 15 |
| zepbound_wegovy_filtered.csv (copied) | 0.1 MB | 838 | 15 |

- The full file covers 2025, quarters 1 and 2, with 14,192 distinct product names. The 53 values in the State column are the 50 states, DC, Puerto Rico, and "XX" (the national total).
- About half of the rows (50.2%) are suppressed for privacy and have no amounts.
- The analysis narrows 2.6 million rows down to 838 Zepbound and Wegovy rows. `state_reimbursement_all_drugs.csv` (52 rows, all drugs by state, excluding the national total) is also built from the full file so the notebook can run without it.

Counts measured directly from the files on 2026-11-14.
