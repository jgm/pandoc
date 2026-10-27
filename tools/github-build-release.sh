#!/bin/sh
# Trigger the GitHub "Release candidate" workflow on main and download
# all artifacts.  Needs the gh CLI, authenticated (gh auth login).
# If a parameter is provided, it will be used as the download path,
# by default, 'artifacts'.
set -eu
repo=jgm/pandoc
workflow=release-candidate.yml
ref=main
artifacts="${1:-artifacts}"

# Cancel any release candidate runs that are still queued or in progress.
for r in $(gh run list -R "$repo" --workflow "$workflow" -L 50 \
             --json databaseId,status \
             --jq '.[] | select(.status != "completed") | .databaseId'); do
  echo "Canceling run $r"
  gh run cancel "$r" -R "$repo" || true
done

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

# Once the unsigned Windows artifact appears, signing is waiting on
# approval in SignPath; send a macOS notification.
(
  while :; do
    sleep 60
    if gh api "repos/$repo/actions/runs/$run/artifacts" --paginate \
         --jq '.artifacts[].name' 2>/dev/null |
         grep -qx 'windows-release-candidate-unsigned'; then
      osascript -e 'display notification "SignPath is awaiting approval of the Windows package signing." with title "pandoc release candidate"'
      break
    fi
  done
) &
notifier=$!
# Make sure the notifier is killed however the script exits.  Background
# jobs in a non-interactive shell ignore SIGINT, so Ctrl-C won't reach it.
trap 'kill "$notifier" 2>/dev/null || true' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
trap 'exit 129' HUP

# Wait for the run to finish; download artifacts even if a job failed.
status=0
gh run watch "$run" -R "$repo" -i 60 --exit-status >/dev/null || status=$?
kill "$notifier" 2>/dev/null || true
gh run view "$run" -R "$repo" --json jobs \
  --jq '.jobs[] | "\(.name): \(.conclusion)"'

# Skip the intermediate Windows artifacts; only the signed one is wanted.
mkdir -p "$artifacts"
for name in $(gh api "repos/$repo/actions/runs/$run/artifacts" --paginate \
                --jq '.artifacts[].name |
                      select(test("^windows-release-candidate-(unsigned|to-sign)$") | not)'); do
  rm -f "$artifacts/$name"
  gh run download "$run" -R "$repo" -n "$name" -D "$artifacts" || status=$?
done
exit "$status"
