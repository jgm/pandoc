#!/bin/sh
# Trigger the CircleCI release pipeline on main and download all artifacts.
# Needs CIRCLECI_API_TOKEN (personal API token), curl, and jq.
# If a parameter is provided, it will be used as the download path,
# by default, 'artifacts'.
set -eu
api=https://circleci.com/api/v2
slug=gh/jgm/pandoc
auth="Circle-Token: $CIRCLECI_API_TOKEN"
artifacts="${1:-artifacts}"

pid=$(curl -sf -X POST -H "$auth" -H 'Content-Type: application/json' \
        -d '{"branch":"main"}' "$api/project/$slug/pipeline" | jq -r .id)
echo "Pipeline $pid started"

# Wait until no workflow is still running.
while :; do
  sleep 60
  wf=$(curl -sf -H "$auth" "$api/pipeline/$pid/workflow")
  echo "$wf" | jq -r '.items[] | "\(.name): \(.status)"'
  echo "$wf" | jq -e '.items | length > 0 and
      all(.status | IN("running","failing","on_hold") | not)' >/dev/null && break
done

mkdir -p "$artifacts"
for w in $(echo "$wf" | jq -r '.items[].id'); do
  for n in $(curl -sf -H "$auth" "$api/workflow/$w/job" |
               jq -r '.items[] | select(.job_number) | .job_number'); do
    curl -sf -H "$auth" "$api/project/$slug/$n/artifacts" |
      jq -r '.items[].url' |
      while read -r url; do
        echo "Downloading $url"
        curl -sfL -H "$auth" -o "$artifacts/$(basename "$url")" "$url"
      done
  done
done
