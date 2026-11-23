"""
Extract category/keyword matches from a meeting transcript and attribute
each match back to the speaker who said it.

Input:  a transcript .txt file where each speaker turn looks like:

    Speaker 1  00:01
    Some line of text they said, possibly spanning multiple lines...

    Speaker 2  00:55
    Their reply...

Output: a CSV with columns: speaker, timestamp, category, matched_keyword, line_text

Usage:
    python extract_categories.py --transcript sample_transcript_otter.txt --out category_matches.csv
"""

import argparse
import re
import csv

# ---------------------------------------------------------------------------
# 1. EDIT THIS: category name -> list of keywords/phrases to search for.
#    Matching is case-insensitive and looks for whole words/phrases
#    (so "host" won't match "hosting" unless you add "hosting" too).
#    The client category below is set up for the fictional sample meeting;
#    swap in your own client's names and terms for a real meeting.
# ---------------------------------------------------------------------------
CATEGORIES = {
    "Infrastructure/Access": [
        "host", "ssh", "aws", "vpc", "linux", "powershell",
        "firewall", "ip address", "ip addresses", "allow list",
    ],
    "Business Intelligence/AI": [
        "business intelligence", "matching", "insights", "algorithms",
        "machine learning", "crm", "scoring model",
    ],
    "Requirements/MVP": [
        "requirements", "mvp", "poc", "minimum viable", "priority",
        "scope", "architecture", "feature",
    ],
    "Client Background (Maple and Main)": [
        "maple and main", "store managers", "stores", "loyalty members", "consulting partner",
    ],
    "Costs/Budget": [
        "cost", "budget", "budget cap", "by the hour", "charges",
    ],
    "Technical Skills": [
        "python", "sql", "linux",
    ],
}


def parse_transcript(filepath):
    """Parse a transcript file into a list of (speaker, timestamp, text) tuples."""
    speaker_line_re = re.compile(r"^(Speaker \d+)\s+([\d:]+)\s*$")

    entries = []
    current_speaker = None
    current_ts = None
    buffer = []

    with open(filepath, encoding="utf-8") as f:
        for line in f:
            stripped = line.strip()
            m = speaker_line_re.match(stripped)
            if m:
                # save the previous turn before starting a new one
                if current_speaker and buffer:
                    entries.append((current_speaker, current_ts, " ".join(buffer)))
                current_speaker = m.group(1)
                current_ts = m.group(2)
                buffer = []
            elif stripped and "Transcribed by" not in stripped:
                buffer.append(stripped)

    if current_speaker and buffer:
        entries.append((current_speaker, current_ts, " ".join(buffer)))

    return entries


def find_matches(entries, categories):
    """For each speaker turn, check every category's keywords.
    Returns one row per (turn, matched keyword) so you can see exactly
    which word triggered which category."""
    rows = []
    for speaker, ts, text in entries:
        text_lower = text.lower()
        for category, keywords in categories.items():
            for kw in keywords:
                # \b word boundaries so "host" doesn't match inside "hosting"
                pattern = r"\b" + re.escape(kw.lower()) + r"\b"
                if re.search(pattern, text_lower):
                    rows.append({
                        "speaker": speaker,
                        "timestamp": ts,
                        "category": category,
                        "matched_keyword": kw,
                        "line_text": text,
                    })
    return rows


def write_csv(rows, out_path):
    with open(out_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["speaker", "timestamp", "category", "matched_keyword", "line_text"])
        writer.writeheader()
        writer.writerows(rows)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Find category keywords in a transcript and attribute them to speakers.")
    parser.add_argument("--transcript", default="transcript.txt", help="Path to the transcript .txt file.")
    parser.add_argument("--out", default="category_matches.csv", help="Path for the output CSV.")
    args = parser.parse_args()

    entries = parse_transcript(args.transcript)
    rows = find_matches(entries, CATEGORIES)
    write_csv(rows, args.out)
    print(f"Parsed {len(entries)} speaker turns, found {len(rows)} keyword matches.")
    print(f"Wrote {args.out}")
