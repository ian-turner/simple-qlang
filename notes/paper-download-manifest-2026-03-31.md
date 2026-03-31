# Paper Download Manifest

`scripts/get_papers.sh` now reads `resources/papers.txt` instead of hardcoding a
single URL.

Manifest format:

```text
filename | title | url
```

Rules:

- one paper per line
- blank lines are ignored
- lines beginning with `#` are ignored
- downloaded PDFs are written to `resources/<filename>`

The script logs the human-readable title while downloading, but uses the
manifest filename as the actual output path so the repository can keep stable,
predictable local names even if paper titles change.
