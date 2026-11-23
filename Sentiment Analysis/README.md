# Movie Review Sentiment Analysis

Can a computer tell a positive movie review from a negative one? This project compares two prebuilt sentiment analyzers with two classifiers trained on labeled reviews.

## Data
25,000 labeled IMDB movie reviews (`labeledTrainData.tsv`, included zipped as `labeledTrainData.zip`), exactly half positive and half negative.

## Notebooks
| Notebook | What it covers |
|---|---|
| `1_Sentiment_TextBlob_vs_VADER.ipynb` | Scores every review with TextBlob and VADER (no training) and checks them against the true labels |
| `2_Sentiment_Classifiers_TFIDF.ipynb` | Cleans the text (lowercase, punctuation, stop words, stemming), builds TF-IDF features, and trains logistic regression and Naive Bayes on an 80/20 split |

## Key results
| Method | Trained on reviews? | Accuracy | Evaluated on |
|---|---|---|---|
| TextBlob | No | 68.5% | all 25,000 reviews |
| VADER | No | 69.4% | all 25,000 reviews |
| Naive Bayes (TF-IDF) | Yes | 87.5% | 5,000 held-out reviews |
| Logistic regression (TF-IDF) | Yes | 89.1% (ROC AUC 0.958) | 5,000 held-out reviews |

- Both prebuilt analyzers lean positive: TextBlob labels 7,193 negative reviews as positive.
- Training on labeled reviews adds 18 to 21 percentage points of accuracy.
- The model's strongest signals make sense: "worst", "waste" and "bad" push toward negative; "great", "excellent" and "best" push toward positive.

## Tools
Python (pandas, NLTK, TextBlob, vaderSentiment, scikit-learn, matplotlib)

## How to run
Install the packages in `requirements.txt`, unzip `labeledTrainData.zip` into this folder, and run each notebook from top to bottom. Notebook 2 downloads NLTK's stop word list the first time it runs.

## Context
Master's coursework (DSC550, Weeks 3 and 5), cleaned up and corrected for this portfolio.

## Portfolio cleanup and corrections (November 2026)
This cleanup was done with help from Claude (Anthropic).

- **Corrected a labeling bug.** The original notebooks overwrote the true review labels with TextBlob or VADER predictions. Week 3 then compared predictions with themselves, and Week 5 trained its models to copy TextBlob. Both notebooks now keep the true labels, so every result is measured against the actual sentiment.
- **Corrected the reported accuracy.** The original Week 3 figures (65.7% and 75.2%) were 12,500 divided by the number of predicted positives, not accuracy. The measured accuracies are 68.5% (TextBlob) and 69.4% (VADER).
- **Corrected the Naive Bayes evaluation.** The original reused the logistic regression predictions for Naive Bayes, so both models showed identical results. Each model is now scored on its own predictions.
- Reorganized two weekly assignments into two notebooks: prebuilt analyzers, then trained classifiers
- Added a summary and findings to each notebook, plus comparison charts, ROC curves and a top-words chart
- Removed empty cells, duplicate imports, install comments and a cell that printed 12,500 labels
- Replaced a step that converted the 25,000 x 92,226 word matrix into a full table (very slow and memory heavy) with a summary of its size
- Every number in the notebooks and this README was checked against a fresh run
