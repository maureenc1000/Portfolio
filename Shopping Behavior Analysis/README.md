# Shopping Behavior: Who Subscribes?

Explores 3,900 retail purchases and tests whether customer behavior can predict who becomes a subscriber, including a check on whether the model's accuracy is real.

## Data
`shopping_behavior_updated.csv` (public Kaggle dataset): 3,900 purchases with customer age, gender, location, item, category, amount, season, review rating, shipping type, payment method, purchase frequency, discount and promo code use, and subscription status.

## Key results
| Model | Test accuracy | Subscribers caught |
|---|---|---|
| Baseline: always "not a subscriber" | 71.5% | 0% |
| Random forest, all features | 81.0% | 89.6% |
| Random forest, without discount, promo code and gender | 71.3% | 0% |

- Every one of the 1,053 subscribers used a discount and a promo code, and every one is male, so those columns give the answer away. Discount and promo code use carry 45% of the model's feature importance.
- Without them, the model finds no subscribers at all: nothing else in the data predicts subscription.
- Shopping patterns are very even across age, gender, category and channel (for example 16.8 to 17.9 purchases a year across age groups).
- **Takeaway:** check how a model gets its accuracy before trusting it. This dataset looks synthetic and can't support real conclusions about what drives subscriptions.

## Files
| File | What it is |
|---|---|
| `Shopping_Behavior_Subscription_Analysis.ipynb` | The analysis |
| `shopping_behavior_updated.csv` | The data |

## Tools
Python (pandas, scikit-learn, matplotlib, seaborn)

## How to run
Keep the CSV in this folder and run the notebook from top to bottom.

## Context
Master's coursework (DSC680, Project 1), cleaned up and corrected for this portfolio.

## Portfolio cleanup and corrections (November 2026)
This cleanup was done with help from Claude (Anthropic).

- **Added a leakage check.** The original reported 81% accuracy and a feature importance chart without noting that discount and promo code use perfectly identify subscribers. The new section shows the cross-tabs and a model without those columns.
- Combined the useful charts into one overview figure and added the printed numbers behind them
- Removed an unrun duplicate of the whole pipeline at the end of the notebook and a correlation heatmap of ID-like columns
- Every number was checked against a fresh run
