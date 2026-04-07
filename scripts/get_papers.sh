#!/bin/bash

set -euo pipefail

MANIFEST="resources/papers.txt"
OUTPUT_DIR="resources"
failures=0

if [[ ! -f "$MANIFEST" ]]; then
  echo "paper manifest not found: $MANIFEST" >&2
  exit 1
fi

while IFS='|' read -r raw_filename raw_title raw_url; do
  filename="$(printf '%s' "${raw_filename:-}" | xargs)"
  title="$(printf '%s' "${raw_title:-}" | xargs)"
  url="$(printf '%s' "${raw_url:-}" | xargs)"

  if [[ -z "$filename" || "${filename:0:1}" == "#" ]]; then
    continue
  fi

  if [[ -z "$title" || -z "$url" ]]; then
    echo "skipping malformed manifest entry for '$filename'" >&2
    continue
  fi

  echo "Downloading: $title"
  if ! wget -O "$OUTPUT_DIR/$filename" "$url"; then
    echo "wget failed for: $title; retrying with curl" >&2
    if ! curl -L -f -o "$OUTPUT_DIR/$filename" "$url"; then
      echo "download failed: $title" >&2
      failures=$((failures + 1))
    fi
  fi
done < "$MANIFEST"

if [[ "$failures" -ne 0 ]]; then
  echo "$failures download(s) failed" >&2
  exit 1
fi
