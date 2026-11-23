"""
run_demo.py

End-to-end demo of the meeting comment tools on a FICTIONAL sample meeting
(sample_transcript_otter.txt). Every person, company and detail in the sample
is made up, so the outputs are safe to share.

What it shows, in order:
  1. Reading an Otter.ai-style transcript ("Speaker N  MM:SS" headers)
  2. Converting it to the "Name: text" format that Teams/Zoom exports use
  3. extract_categories.py: keyword categories attributed to each speaker -> CSV
  4. extract_meeting_comments.py --engine rules: comments and suggestions,
     each with a speaker, topic and unique ID -> Excel repository
  5. Running step 4 again on the same meeting to show duplicate rows are skipped
  6. extract_meeting_comments.py --engine ollama, if a local Ollama server is
     running (skipped with a message otherwise)
  7. A short summary of what was found

Usage:
  python run_demo.py
"""

import re
import subprocess
import sys
import urllib.request
from pathlib import Path

HERE = Path(__file__).resolve().parent
OTTER = HERE / "sample_transcript_otter.txt"
TEAMS = HERE / "sample_transcript_teams.txt"
CATEGORY_CSV = HERE / "demo_category_matches.csv"
RULES_REPO = HERE / "demo_comments_repository_rules.xlsx"
OLLAMA_REPO = HERE / "demo_comments_repository_ollama.xlsx"
MEETING_NAME = "Loyalty App Pilot Planning Sync"
MEETING_DATE = "2026-01-15"

# fictional names for the Otter speaker numbers
SPEAKER_NAMES = {"Speaker 1": "Alex", "Speaker 2": "Jordan", "Speaker 3": "Sam", "Speaker 4": "Taylor"}


def banner(text):
    print("\n" + "=" * 70 + f"\n{text}\n" + "=" * 70)


def run(args):
    print("> python " + " ".join(str(a) for a in args))
    result = subprocess.run([sys.executable, *map(str, args)], cwd=HERE, capture_output=True, text=True)
    print(result.stdout.strip())
    if result.returncode != 0:
        print(result.stderr.strip())
        raise SystemExit(f"Step failed: {args[0]}")


def otter_to_teams(src: Path, dst: Path):
    """Convert 'Speaker N  MM:SS' blocks into one 'Name: text' line per turn."""
    header = re.compile(r"^(Speaker \d+)\s+\d{1,2}:\d{2}\s*$")
    turns, speaker, buf = [], None, []
    for line in src.read_text(encoding="utf-8").splitlines():
        s = line.strip()
        m = header.match(s)
        if m:
            if speaker and buf:
                turns.append((speaker, " ".join(buf)))
            speaker, buf = m.group(1), []
        elif s and speaker and not s.lower().startswith("transcribed by"):
            buf.append(s)
    if speaker and buf:
        turns.append((speaker, " ".join(buf)))
    dst.write_text("\n".join(f"{SPEAKER_NAMES.get(sp, sp)}: {text}" for sp, text in turns) + "\n", encoding="utf-8")
    return len(turns)


def ollama_running(url="http://localhost:11434"):
    try:
        with urllib.request.urlopen(url + "/api/tags", timeout=3) as r:
            return r.status == 200
    except Exception:
        return False


def summarize(repo: Path):
    from collections import Counter
    from openpyxl import load_workbook
    rows = list(load_workbook(repo).active.iter_rows(min_row=2, values_only=True))
    print(f"{len(rows)} comment(s)/suggestion(s) in {repo.name}")
    print("By speaker:", dict(Counter(r[3] for r in rows).most_common()))
    print("By topic:  ", dict(Counter(r[4] for r in rows).most_common()))
    print("First three rows:")
    for r in rows[:3]:
        print(f"  {r[0]} | {r[3]} | {r[4]} | {r[5]}")


if __name__ == "__main__":
    banner("1-2. Read the Otter-style sample and convert it to 'Name: text' format")
    n = otter_to_teams(OTTER, TEAMS)
    print(f"{n} speaker turns written to {TEAMS.name}")

    banner("3. Keyword categories by speaker (extract_categories.py)")
    run(["extract_categories.py", "--transcript", OTTER.name, "--out", CATEGORY_CSV.name])

    banner("4. Comments and suggestions, rules engine (extract_meeting_comments.py)")
    for f in (RULES_REPO, OLLAMA_REPO):
        if f.exists():
            f.unlink()  # start each demo with a fresh repository file
    repo_args = ["extract_meeting_comments.py", "--transcript", TEAMS.name, "--meeting-name", MEETING_NAME,
                 "--meeting-date", MEETING_DATE, "--repository", RULES_REPO.name, "--engine", "rules"]
    run(repo_args)

    banner("5. Same meeting again: duplicates are skipped, nothing is added twice")
    run(repo_args)

    banner("6. Local LLM engine (Ollama)")
    if ollama_running():
        run(["extract_meeting_comments.py", "--transcript", TEAMS.name, "--meeting-name", MEETING_NAME,
             "--meeting-date", MEETING_DATE, "--repository", OLLAMA_REPO.name, "--engine", "ollama"])
    else:
        print("Ollama is not running on this computer, so this step is skipped.")
        print("To try it: install Ollama (https://ollama.com/download), run `ollama pull llama3.1`, then rerun this demo.")

    banner("7. Summary")
    summarize(RULES_REPO)
    if OLLAMA_REPO.exists():
        print()
        summarize(OLLAMA_REPO)
