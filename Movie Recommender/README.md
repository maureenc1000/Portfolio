# Movie Recommender (SVD Matrix Factorization)

Recommends movies from past ratings, either for a specific user or based on a movie someone already likes.

## Data
MovieLens "latest small" dataset: 100,836 ratings (0.5 to 5 stars) of 9,742 movies by 610 users (`ratings.csv`, `movies.csv`; `links.csv` and `tags.csv` are included but not used).

## Approach
- Trained an SVD matrix factorization model with the Surprise library on an 80/20 split
- Compared it with a baseline that always predicts the average rating
- Tuned its settings with 5-fold cross-validation (grid search)
- Used it two ways: predicted ratings for a user's unseen movies, and similar movies based on the profile the model learns for each movie

## Key results
| Model | RMSE (stars) |
|---|---|
| Always predict the average | 1.045 |
| SVD, default settings (test set) | 0.881 |
| SVD, default settings (5-fold CV) | 0.873 |
| SVD, tuned (5-fold CV) | 0.861 |

- Movies similar to *Toy Story*: *Wallace & Gromit: The Wrong Trousers*, *Aladdin*, *Back to the Future*, *Toy Story 2*, ...
- Movies similar to *The Matrix*: *Die Hard*, *The Empire Strikes Back*, *Return of the Jedi*, *Seven*, *Inception*, ...
- The model was never given genres; it learned these groupings from rating patterns alone.

## Files
| File | What it is |
|---|---|
| `Movie_Recommender_SVD.ipynb` | The analysis |
| `movies.csv`, `ratings.csv`, `links.csv`, `tags.csv` | MovieLens data |

## Tools
Python (pandas, NumPy, scikit-surprise)

## How to run
Install `scikit-surprise` (on Windows it needs Microsoft C++ Build Tools), keep the CSV files in this folder, and run the notebook from top to bottom. The grid search takes a few minutes.

## Context
Master's coursework (DSC630, Week 10), cleaned up and corrected for this portfolio.

## Portfolio cleanup and corrections (November 2026)
This cleanup was done with help from Claude (Anthropic).

- **Corrected a data cleaning bug.** The original removed duplicates by movie ID, which kept only one rating per movie and discarded 91,112 of the 100,836 ratings, so its recommendations were close to random. This version keeps every rating.
- **Corrected the reported RMSE.** The original's comments said 0.86 to 0.88, but its own output showed 0.94 because of the bug above. The fixed model scores 0.881 on the test set and 0.861 after tuning.
- Added a baseline and a summary chart, fixed random seeds (including the cross-validation folds) so results repeat, and limited recommendations to movies with at least 20 ratings
- Replaced the typed `input()` prompts with a function call so the notebook runs from start to finish
- Added movie-to-movie recommendations (the assignment asked for recommendations based on a movie the user likes)
- An alternate coursework attempt (`...MaureenCasey no.ipynb`) was not included
- Every number was checked against a fresh run

