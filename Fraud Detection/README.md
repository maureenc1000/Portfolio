# Transaction Fraud Detection

Compares five classification models for flagging fraudulent card transactions when fraud is only 3.5% of the data, and shows the trade-off between catching fraud and flagging legitimate customers.

## Data
IEEE-CIS fraud detection data: 590,540 labeled training transactions (20,663 fraud, 3.5%) and 506,691 unlabeled test transactions. The raw files total about 1.3 GB and are not included (see `ORIGINAL_DATA_NOTE.md`); the notebook runs from the included cleaned files (zipped; unzip them into this folder first).

## Key results (validation set: 10,000 transactions, 350 fraud)
| Model | AUC | Accuracy | Fraud caught | Flags that are fraud |
|---|---|---|---|---|
| Logistic regression | 0.683 | 96.5% | 0.0% | n/a |
| Random forest | 0.758 | 96.2% | 16.9% | 40.4% |
| RF, balanced weights | 0.760 | 95.4% | 19.1% | 27.5% |
| RF, undersampling | **0.772** | 72.3% | **66.0%** | 8.0% |
| RF, oversampling | 0.764 | 94.5% | 24.3% | 22.9% |

- Never flagging anything is 96.5% accurate, so accuracy is meaningless here; logistic regression matches it by catching nothing.
- The best model depends on the cost of each mistake: undersampling catches the most fraud, the plain random forest gives the most reliable flags.
- Strongest signals: transaction amount and card details. Fraud rates are highest for product code C (11.7%) and mobile devices (10.2%).

## Files
| File | What it is |
|---|---|
| `Fraud_Detection_Models.ipynb` | The analysis |
| `df_fraud_train_clean.zip`, `df_fraud_test_clean.zip` | Cleaned data the notebook uses (unzip into this folder before running) |
| `train_identity.csv`, `test_identity.csv` | Device data, used only when rebuilding from the raw files (not included: too large for this repository) |
| `fraud_predictions_test.csv` | Fraud probabilities for the unlabeled test transactions |
| `ORIGINAL_DATA_NOTE.md` | Size and location of the raw transaction files |

## Tools
Python (pandas, scikit-learn, matplotlib, seaborn)

## How to run
Unzip the two cleaned data files into this folder, then run the notebook from top to bottom. If the raw `train_transaction.csv` and `test_transaction.csv` are placed in this folder, it rebuilds the cleaned files from them first.

## Context
Master's coursework (DSC680, Project 3), cleaned up for this portfolio.

## Portfolio cleanup (November 2026)
This cleanup was done with help from Claude (Anthropic).

- Added a summary, findings and limitations (the original had no written analysis)
- Fixed the random seed for undersampling and oversampling, so results repeat (the original's results shifted slightly each run; for example the undersampled AUC was 0.770 in its saved output and 0.772 now)
- Added recall, precision and number of transactions flagged, since accuracy is misleading with rare fraud
- Added fraud-rate charts, feature importance, and the 3-fold cross-validation score the original computed but never displayed
- The notebook now runs without the 1.3 GB raw files
- Every number was checked against a fresh run
