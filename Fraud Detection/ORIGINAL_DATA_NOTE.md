# Original data (not copied here because of size)

The raw transaction files are too large for this folder and for GitHub (over 100 MB each). They are kept outside the portfolio, in the original course folder (DSC680 Assignments). To rebuild the cleaned files, copy them into this folder and rerun the notebook.

| File | Size | Rows | Columns |
|---|---|---|---|
| train_transaction.csv | 651.7 MB | 590,540 | 394 |
| test_transaction.csv | 584.8 MB | 506,691 | 393 |
| train_identity.csv (not included, too large) | 25.3 MB | 144,233 | 41 |
| test_identity.csv (not included, too large) | 24.6 MB | 141,907 | 41 |

- Training set: 20,663 of 590,540 transactions are labeled fraud (3.5%).
- In total, about 1.1 million transactions and 1.3 GB of raw transaction data.
- The cleaned files the notebook builds from these (`df_fraud_train_clean.csv`, `df_fraud_test_clean.csv`) are included here as zip files.

Counts measured directly from the files on 2026-11-14.
