# Meeting Comment Classifier

Tools that turn a meeting transcript into a searchable record of who suggested what. They pull out comments and suggestions (opinions, concerns, proposed actions, recommendations), tag each one with the speaker and a topic, give it a unique ID, and append it to an Excel repository. A notebook compares two free approaches and picks one based on measured results.

**All examples use a fictional meeting** (`sample_transcript_otter.txt`). Every person, company and detail in it is made up. The tools were first built on a real work meeting, which is not published.

## What it does
| Capability | Where |
|---|---|
| Parse Otter.ai transcripts (`Speaker N  MM:SS` blocks) | `extract_categories.py`, notebook, `run_demo.py` |
| Parse Teams/Zoom transcripts (`Name: text` lines) | `extract_meeting_comments.py` |
| Detect comments and suggestions with a rules engine (regex patterns) | `extract_meeting_comments.py --engine rules` |
| Detect them with a free local LLM through Ollama | `extract_meeting_comments.py --engine ollama` |
| Assign a topic (Budget, Timeline, Process, Client Communication, Staffing, Quality / Risk, Tools / Technology, Scope) | `extract_meeting_comments.py` |
| Append to an Excel repository with unique IDs, skipping duplicates | `extract_meeting_comments.py` |
| Tag keyword categories by speaker, with the word that triggered each match | `extract_categories.py` |
| Compare the rules engine with a scikit-learn model on hand-labeled sentences | `comment_classification_model_comparison.ipynb` |

## Results on the sample meeting
- 41 speaker turns, 35 keyword category matches, 17 comments and suggestions found by the rules engine, covering all 8 topics
- Running the same meeting twice adds nothing new: all 17 rows are recognized as duplicates
- On 40 hand-labeled sentences, the rules engine scored F1 0.74 (precision 0.87) and the scikit-learn model scored F1 0.47. With this little training data, the hand-written rules win; the notebook explains when that should change.

## How to run
```
pip install -r requirements.txt
python run_demo.py
```
`run_demo.py` runs every tool in order and writes `demo_category_matches.csv`, `sample_transcript_teams.txt` and `demo_comments_repository_rules.xlsx`. The Ollama step runs only if Ollama is installed and running; otherwise it is skipped with setup instructions.

Each script also runs on its own, for example:
```
python extract_meeting_comments.py --transcript sample_transcript_teams.txt --meeting-name "Weekly Sync" --meeting-date 2026-01-15 --engine rules
python extract_categories.py --transcript sample_transcript_otter.txt --out category_matches.csv
```

## Tools
Python (re, argparse, pandas, scikit-learn), openpyxl, Ollama (optional)

## Context
Built during an AI Builder course to automate meeting notes.

## Portfolio cleanup (November 2026)
This cleanup was done with help from Claude (Anthropic).

- Replaced the real meeting transcript and labeled sentences with a fictional sample meeting, and reran the notebook on it
- Removed the real client and people names from the keyword lists in `extract_categories.py`
- Added command-line options to `extract_categories.py` (`--transcript`, `--out`)
- Added `run_demo.py` to show every capability end to end
- Every number in this README and the notebook was checked against the fresh output
