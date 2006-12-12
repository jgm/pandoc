if [ -z "$SYNOPSIS" ]; then
    SYNOPSIS="[-h] [input_file]"
    [ -n "$THIS_NARG" ] || SYNOPSIS="${SYNOPSIS}..."
fi

while getopts h opt; do
    case $opt in
    h|?) usage "$SYNOPSIS"; exit 2 ;;
    esac
done

shift $(($OPTIND - 1))
