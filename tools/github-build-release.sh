#!/bin/sh
# Trigger the GitHub "Release candidate" workflow on main and download
# all artifacts.  Needs the gh CLI, authenticated (gh auth login).
set -eu
repo=jgm/pandoc
workflow=release-candidate.yml
ref=main

start=$(date -u +%Y-%m-%dT%H:%M:%SZ)
gh workflow run "$workflow" -R "$repo" --ref "$ref"

# The dispatch doesn't return a run id; wait for the new run to show up.
while :; do
  sleep 10
  run=$(gh run list -R "$repo" --workflow "$workflow" --branch "$ref" \
          --event workflow_dispatch -L 1 --json databaseId,createdAt \
          --jq ".[] | select(.createdAt >= \"$start\") | .databaseId")
  [ -n "$run" ] && break
done
echo "Run $run started: https://github.com/$repo/actions/runs/$run"

# Wait for the run to finish; download artifacts even if a job failed.
status=0
gh run watch "$run" -R "$repo" -i 60 --exit-status >/dev/null || status=$?
gh run view "$run" -R "$repo" --json jobs \
  --jq '.jobs[] | "\(.name): \(.conclusion)"'

mkdir -p artifacts
gh run download "$run" -R "$repo" -D artifacts || status=$?
exit "$status"
