# Medicaid Spending on GLP-1 Weight-Loss Drugs (Zepbound and Wegovy)

Analyzes how much state Medicaid programs spent on Zepbound and Wegovy in the first half of 2025, where the money went, and how cost per prescription differs by state.

## Data
CMS State Drug Utilization Data, Q1 and Q2 2025: 2,637,009 rows covering every drug Medicaid paid for (239 MB, not included; see `ORIGINAL_DATA_NOTE.md`). The State column holds the 50 states, DC, Puerto Rico and a national total ("XX"). The notebook uses two small files built from it:
- `zepbound_wegovy_filtered.csv`: 838 unsuppressed Zepbound and Wegovy rows (49 states and territories)
- `state_reimbursement_all_drugs.csv`: total reimbursement for all drugs by state, for context

## Key results
- **$1.68 billion** for **1.49 million** Zepbound and Wegovy prescriptions in the first half of 2025, **3.20%** of all Medicaid drug spending ($52.4 billion)
- Wegovy: 64% of dollars, $1,175 per prescription. Zepbound: 36%, $1,043 per prescription, with prescriptions up **41.7%** from Q1 to Q2 (Wegovy up 6.7%)
- Five states account for **83.0%** of the dollars, led by California ($780 million)
- Per-prescription cost differs significantly by state (Kruskal-Wallis p < 0.001), median $1,221; Delaware ($28) and Michigan ($454) are outliers worth confirming
- New York, North Dakota and South Dakota have no unsuppressed rows for either drug

## Files
| File | What it is |
|---|---|
| `Medicaid_GLP1_Spending_Analysis.ipynb` | The analysis |
| `zepbound_wegovy_filtered.csv`, `state_reimbursement_all_drugs.csv` | Data the notebook uses |
| `ORIGINAL_DATA_NOTE.md` | Size and location of the full CMS file |

## Tools
Python (pandas, SciPy, matplotlib, seaborn)

## How to run
Run the notebook from top to bottom. If the full `sdud-2025-updated-dec2025.csv` is placed in this folder, it rebuilds both small files from it first.

## Context
Master's coursework (DSC680, Project 2), cleaned up and corrected for this portfolio.

## Portfolio cleanup and corrections (November 2026)
This cleanup was done with help from Claude (Anthropic).

- **Separated GLP-1 results from all-drug results.** The original switched to all Medicaid drugs partway through without labeling it, so its state totals and its Kruskal-Wallis test described all drug spending. All-drug figures are now labeled as context, and the test runs on the GLP-1 data.
- **Weighted the per-prescription costs.** The original averaged row-level costs, so a 15-prescription row counted as much as a 5,000-prescription row. Costs are now total dollars divided by total prescriptions.
- Added a summary, findings, the Q1-to-Q2 trend, each state's GLP-1 share of drug spending, and limitations
- The notebook now runs without the 239 MB raw file
- Every number was checked against a fresh run
