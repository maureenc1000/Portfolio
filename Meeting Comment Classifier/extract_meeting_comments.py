"""
extract_meeting_comments.py

Pulls comments/suggestions out of a meeting transcript, attributes each one
to a speaker, assigns a topic and a unique ID, and appends the results to
an Excel repository (one row per comment).

NO PAID API REQUIRED. Two free engines are available, pick with --engine:

  --engine rules   (default)
    Pure Python. No LLM, no install beyond openpyxl, runs instantly, $0.
    Detects comments/suggestions with keyword/phrase patterns (e.g. "I think
    we should", "what if we", "I'm concerned about") and assigns topics from
    a keyword dictionary. TRADEOFF: it will miss suggestions phrased in ways
    that don't match the patterns, and will occasionally flag a sentence that
    isn't really a suggestion. Good for a quick first pass or if you want
    zero setup.

  --engine ollama
    Uses a free LOCAL LLM through Ollama (https://ollama.com) running on your
    own machine. No API key, no per-token cost. Reads meaning/context the way
    the rules engine can't, so it's noticeably more accurate. TRADEOFF: you
    have to install Ollama and pull a model first (one-time, free), and it
    uses your computer's CPU/GPU so it's slower than a hosted API.
    Setup:
      1. Install Ollama: https://ollama.com/download
      2. Pull a model, e.g.:  ollama pull llama3.1
      3. Make sure Ollama is running (it starts a local server automatically
         on http://localhost:11434 after install)

SETUP (either engine):
  pip install openpyxl

TRANSCRIPT FORMAT EXPECTED:
  Plain text file, one turn per line (or block), like:
    Alex: I think we should push the deadline back a week.
    Jordan: That works for me, but let's flag it to the client first.
  This matches typical Teams/Zoom transcript exports. A turn can also
  span multiple lines as long as it doesn't start with a new "Speaker:" label.

USAGE:
  python extract_meeting_comments.py \
      --transcript path/to/transcript.txt \
      --meeting-name "Weekly Sync" \
      --meeting-date 2026-08-28 \
      --repository path/to/meeting_comments_repository.xlsx \
      --engine rules

  (add --engine ollama --ollama-model llama3.1 to use the local-LLM engine)

  If --meeting-date is omitted, today's date is used.
  If --repository is omitted, it defaults to meeting_comments_repository.xlsx
  in the current folder. If the file already exists, new rows are appended
  to it (duplicates are skipped).
"""

import argparse
import json
import re
import sys
import urllib.request
import urllib.error
from datetime import date, datetime
from pathlib import Path

try:
    from openpyxl import Workbook, load_workbook
    from openpyxl.styles import Font
    from openpyxl.utils import get_column_letter
except ImportError:
    print("Missing dependency. Run: pip install openpyxl")
    sys.exit(1)


MAX_CHARS_PER_CHUNK = 6000               # rough chunk size for the ollama engine so local context windows aren't overloaded
OLLAMA_DEFAULT_URL = "http://localhost:11434"
OLLAMA_DEFAULT_MODEL = "llama3.1"

REPOSITORY_COLUMNS = [
    "ID",
    "Meeting Date",
    "Meeting Name",
    "Speaker",
    "Topic",
    "Comment / Suggestion",
    "Extracted On",
]

EXTRACTION_PROMPT = """You are analyzing a meeting transcript. Extract every COMMENT or SUGGESTION
made by a speaker: an opinion, concern, proposed action, or recommendation.

Do NOT extract:
- Status updates or factual reporting ("we shipped the report Tuesday")
- Small talk or greetings
- Questions that are just asking for information (not proposing anything)

For each comment or suggestion found, return an object with:
- "speaker": the exact speaker label as it appears in the transcript
- "quote": the relevant text, lightly cleaned up into a complete sentence (don't paraphrase away the meaning)
- "topic": a short topic label, 2-4 words, written consistently (e.g. "Budget", "Timeline", "Client Communication")

Return ONLY valid JSON: a list of objects with those three keys. No other text, no markdown code fences.
If there are no comments or suggestions in this transcript, return an empty list: []

TRANSCRIPT:
---
{transcript_chunk}
---
"""

# ---------------------------------------------------------------------------
# Rules-engine patterns (used by --engine rules)
# ---------------------------------------------------------------------------

SUGGESTION_PATTERNS = [
    r"\bi (?:think|suggest|recommend|propose)\b",
    r"\bwe should\b",
    r"\byou should\b",
    r"\blet'?s\b",
    r"\bwhat if we\b",
    r"\bhow about\b",
    r"\bit (?:would|might) be (?:good|better|worth|nice)\b",
    r"\bmaybe we\b",
    r"\bconsider(?:ing)?\b",
    r"\bworth (?:considering|trying|exploring)\b",
    r"\bi'?m (?:concerned|worried)\b",
    r"\b(?:concern|issue|problem) (?:is|with)\b",
    r"\bwe need to\b",
    r"\brecommend(?:ation|ing)?\b",
    r"\bpropos(?:e|al|ing)\b",
    r"\bcould we\b",
    r"\bshould we\b",
    r"\bi'?d (?:suggest|recommend|propose)\b",
]
SUGGESTION_REGEX = re.compile("|".join(SUGGESTION_PATTERNS), re.IGNORECASE)

TOPIC_KEYWORDS = {
    "Budget": ["budget", "cost", "costs", "spend", "spending", "funding", "price", "expense", "expenses"],
    "Timeline": ["deadline", "timeline", "schedule", "late", "delay", "delayed", "date", "dates", "sooner", "later"],
    "Process": ["process", "workflow", "procedure", "approach", "steps"],
    "Client Communication": ["client", "customer", "stakeholder", "communicate", "communication", "email", "call"],
    "Staffing": ["hire", "hiring", "staff", "team", "resource", "resources", "headcount", "capacity"],
    "Quality / Risk": ["risk", "quality", "bug", "bugs", "issue", "issues", "error", "errors", "concern", "concerns"],
    "Tools / Technology": ["tool", "tools", "software", "system", "platform", "tech", "technology"],
    "Scope": ["scope", "feature", "features", "requirement", "requirements", "deliverable", "deliverables"],
}

SENTENCE_SPLIT_REGEX = re.compile(r"(?<=[.!?])\s+")


# ---------------------------------------------------------------------------
# Shared: transcript parsing
# ---------------------------------------------------------------------------

def parse_transcript_turns(raw_text: str):
    """Split a 'Speaker: text' transcript into (speaker, text) turns."""
    turn_pattern = re.compile(r"^([A-Za-z][A-Za-z0-9 ._'-]{0,49}):\s?(.*)$")
    turns = []
    current_speaker = None
    current_lines = []

    for line in raw_text.splitlines():
        match = turn_pattern.match(line.strip())
        if match:
            if current_speaker is not None:
                turns.append((current_speaker, " ".join(current_lines).strip()))
            current_speaker = match.group(1).strip()
            current_lines = [match.group(2).strip()]
        else:
            if line.strip():
                current_lines.append(line.strip())

    if current_speaker is not None:
        turns.append((current_speaker, " ".join(current_lines).strip()))

    return turns


def chunk_transcript(turns, max_chars=MAX_CHARS_PER_CHUNK):
    """Group turns into chunks that stay under max_chars, without splitting a turn."""
    chunks = []
    current_chunk_lines = []
    current_len = 0

    for speaker, text in turns:
        line = f"{speaker}: {text}"
        if current_len + len(line) > max_chars and current_chunk_lines:
            chunks.append("\n".join(current_chunk_lines))
            current_chunk_lines = []
            current_len = 0
        current_chunk_lines.append(line)
        current_len += len(line)

    if current_chunk_lines:
        chunks.append("\n".join(current_chunk_lines))

    return chunks


def parse_json_response(raw_output: str):
    raw_output = raw_output.strip()
    if raw_output.startswith("```"):
        raw_output = re.sub(r"^```[a-zA-Z]*\n?", "", raw_output)
        raw_output = re.sub(r"\n?```$", "", raw_output)
    try:
        items = json.loads(raw_output)
    except json.JSONDecodeError:
        print("  Warning: could not parse a JSON response for one chunk. Skipping it.")
        print(f"  Raw response was: {raw_output[:300]}")
        return []
    return items if isinstance(items, list) else []


# ---------------------------------------------------------------------------
# Engine: rules (pure Python, no LLM)
# ---------------------------------------------------------------------------

def guess_topic(sentence: str) -> str:
    lowered = sentence.lower()
    best_topic = "Uncategorized"
    best_score = 0
    for topic, keywords in TOPIC_KEYWORDS.items():
        score = sum(1 for kw in keywords if kw in lowered)
        if score > best_score:
            best_score = score
            best_topic = topic
    return best_topic


def extract_with_rules(turns):
    items = []
    for speaker, text in turns:
        if not text:
            continue
        sentences = SENTENCE_SPLIT_REGEX.split(text)
        for sentence in sentences:
            sentence = sentence.strip()
            if sentence and SUGGESTION_REGEX.search(sentence):
                items.append({
                    "speaker": speaker,
                    "quote": sentence,
                    "topic": guess_topic(sentence),
                })
    return items


# ---------------------------------------------------------------------------
# Engine: ollama (free local LLM)
# ---------------------------------------------------------------------------

def call_ollama(prompt: str, model: str, base_url: str) -> str:
    url = f"{base_url.rstrip('/')}/api/generate"
    payload = json.dumps({
        "model": model,
        "prompt": prompt,
        "stream": False,
    }).encode("utf-8")

    req = urllib.request.Request(url, data=payload, headers={"Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=180) as resp:
            body = json.loads(resp.read().decode("utf-8"))
    except urllib.error.URLError as e:
        raise RuntimeError(
            f"Could not reach Ollama at {base_url}. Is it installed and running? "
            f"(Download: https://ollama.com/download, then `ollama pull {model}`). Details: {e}"
        )
    return body.get("response", "")


def extract_with_ollama(turns, model: str, base_url: str):
    chunks = chunk_transcript(turns)
    print(f"  Sending {len(chunks)} chunk(s) to local Ollama model '{model}'...")

    all_items = []
    for i, chunk in enumerate(chunks, start=1):
        print(f"    Chunk {i}/{len(chunks)}...")
        prompt = EXTRACTION_PROMPT.format(transcript_chunk=chunk)
        raw_output = call_ollama(prompt, model, base_url)
        all_items.extend(parse_json_response(raw_output))
    return all_items


# ---------------------------------------------------------------------------
# Extraction dispatch
# ---------------------------------------------------------------------------

def extract_all_comments(transcript_path: Path, engine: str, ollama_model: str, ollama_url: str):
    raw_text = transcript_path.read_text(encoding="utf-8")
    turns = parse_transcript_turns(raw_text)

    if not turns:
        print("Warning: no 'Speaker: text' lines were found. Check the transcript format.")
        return []

    print(f"Parsed {len(turns)} speaker turns.")

    if engine == "rules":
        return extract_with_rules(turns)
    elif engine == "ollama":
        return extract_with_ollama(turns, ollama_model, ollama_url)
    else:
        raise ValueError(f"Unknown engine: {engine}")


# ---------------------------------------------------------------------------
# Repository (xlsx)
# ---------------------------------------------------------------------------

def build_id(meeting_date: str, meeting_name: str, sequence: int) -> str:
    slug = re.sub(r"[^a-z0-9]+", "-", meeting_name.lower()).strip("-")
    return f"{meeting_date}-{slug}-{sequence:03d}"


def load_or_create_repository(repo_path: Path):
    if repo_path.exists():
        wb = load_workbook(repo_path)
        ws = wb.active
        return wb, ws

    wb = Workbook()
    ws = wb.active
    ws.title = "Comments"
    ws.append(REPOSITORY_COLUMNS)
    for col_num, header in enumerate(REPOSITORY_COLUMNS, start=1):
        cell = ws.cell(row=1, column=col_num)
        cell.font = Font(bold=True)
        ws.column_dimensions[get_column_letter(col_num)].width = 22
    ws.freeze_panes = "A2"
    return wb, ws


def existing_keys(ws):
    """Set of (meeting_date, meeting_name, speaker, quote) already in the sheet, for dedup."""
    keys = set()
    for row in ws.iter_rows(min_row=2, values_only=True):
        if row and len(row) >= 6:
            _, meeting_date, meeting_name, speaker, _topic, quote, *_ = row
            keys.add((str(meeting_date), str(meeting_name), str(speaker), str(quote)))
    return keys


def append_to_repository(items, meeting_name: str, meeting_date: str, repo_path: Path):
    wb, ws = load_or_create_repository(repo_path)
    seen = existing_keys(ws)

    # Continue numbering from however many rows already exist for this meeting.
    existing_count = sum(
        1 for row in ws.iter_rows(min_row=2, values_only=True)
        if row and str(row[1]) == meeting_date and str(row[2]) == meeting_name
    )

    added = 0
    skipped = 0
    sequence = existing_count + 1

    for item in items:
        speaker = str(item.get("speaker", "Unknown")).strip()
        quote = str(item.get("quote", "")).strip()
        topic = str(item.get("topic", "Uncategorized")).strip()

        if not quote:
            continue

        key = (meeting_date, meeting_name, speaker, quote)
        if key in seen:
            skipped += 1
            continue

        comment_id = build_id(meeting_date, meeting_name, sequence)
        ws.append([
            comment_id,
            meeting_date,
            meeting_name,
            speaker,
            topic,
            quote,
            datetime.now().isoformat(timespec="seconds"),
        ])
        seen.add(key)
        sequence += 1
        added += 1

    wb.save(repo_path)
    return added, skipped


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Extract comments/suggestions from a meeting transcript into a repository. Free engines only.")
    parser.add_argument("--transcript", required=True, help="Path to the transcript text file.")
    parser.add_argument("--meeting-name", required=True, help="Name of the meeting, e.g. 'Weekly Sync'.")
    parser.add_argument("--meeting-date", default=str(date.today()), help="Meeting date, YYYY-MM-DD. Defaults to today.")
    parser.add_argument("--repository", default="meeting_comments_repository.xlsx", help="Path to the xlsx repository file.")
    parser.add_argument("--engine", choices=["rules", "ollama"], default="rules", help="Extraction engine: 'rules' (pure Python, no LLM) or 'ollama' (free local LLM). Default: rules.")
    parser.add_argument("--ollama-model", default=OLLAMA_DEFAULT_MODEL, help=f"Ollama model name (only used with --engine ollama). Default: {OLLAMA_DEFAULT_MODEL}")
    parser.add_argument("--ollama-url", default=OLLAMA_DEFAULT_URL, help=f"Ollama server URL (only used with --engine ollama). Default: {OLLAMA_DEFAULT_URL}")
    args = parser.parse_args()

    transcript_path = Path(args.transcript)
    if not transcript_path.exists():
        print(f"Transcript file not found: {transcript_path}")
        sys.exit(1)

    print(f"Processing '{args.meeting_name}' ({args.meeting_date}) with engine='{args.engine}'...")
    items = extract_all_comments(transcript_path, args.engine, args.ollama_model, args.ollama_url)
    print(f"Found {len(items)} comment(s)/suggestion(s).")

    repo_path = Path(args.repository)
    added, skipped = append_to_repository(items, args.meeting_name, args.meeting_date, repo_path)

    print(f"Added {added} new row(s) to {repo_path}.")
    if skipped:
        print(f"Skipped {skipped} duplicate row(s) already in the repository.")


if __name__ == "__main__":
    main()
