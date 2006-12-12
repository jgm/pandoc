# Check if input files exist.
for f; do
    if [ -n "$f" ] && ! [ -f "$f" ]; then
        err "File '$f' not found."
        exit 1
    fi
done
