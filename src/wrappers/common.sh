THIS=${0##*/}

NEWLINE='
'
WRAPPER_ARGS=
WRAPPEE_ARGS=

err ()  { echo "$*"   | fold -s -w ${COLUMNS:-110} >&2; }
errn () { printf "$*" | fold -s -w ${COLUMNS:-110} >&2; }

usage () {
    synopsis="$@"
    err "Usage:  $THIS $synopsis"
    err "See $THIS(1) man file for details."
}

runpandoc () {
    if [ -n "$WRAPPEE_ARGS" ]; then
        # Unpack arguments that will be passed to pandoc.
        oldifs="$IFS"; IFS="$NEWLINE"; set -- $WRAPPEE_ARGS "$@"; IFS="$oldifs"
        case "$1" in --) shift;; esac # tolerate the existence of a leading '--'
    fi

    pandoc "$@"
}

# Portable which(1).
pathfind () {
    oldifs="$IFS"; IFS=':'
    for _p in $PATH; do
        if [ -x "$_p/$*" ] && [ -f "$_p/$*" ]; then
            IFS="$oldifs"
            return 0
        fi
    done
    IFS="$oldifs"
    return 1
}

HAVE_ICONV=
if pathfind iconv; then
    HAVE_ICONV=1
    alias to_utf8='iconv -t utf-8'
    alias from_utf8='iconv -f utf-8'
else
    err "Warning:  iconv not present.  Assuming UTF-8 character encoding."
    alias to_utf8='cat'
    alias from_utf8='cat'
fi

for p in pandoc $REQUIRED; do
    pathfind $p || {
        err "You need '$p' to use this program!"
        exit 1
    }
done
