# Ensure to work with a single argument.
if [ $# -gt 1 ]; then
    first_arg="$1"
    shift
    err "Warning:  extra arguments '$@' will be ignored."
    set -- $first_arg
fi
