THIS=${0##*/}

NEWLINE='
'

err ()  { echo "$*"   | fold -s -w ${COLUMNS:-110} >&2; }
errn () { printf "$*" | fold -s -w ${COLUMNS:-110} >&2; }

usage () {
    err "$1 - $2" # short description
    err "See the $1(1) man page for usage."
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

for p in pandoc $REQUIRED; do
    pathfind $p || {
        err "You need '$p' to use this program!"
        exit 1
    }
done

CONF=$(pandoc --dump-args "$@" 2>&1) || {
    errcode=$?
    echo "$CONF" | sed -e '/^pandoc \[OPTIONS\] \[FILES\]/,$d' >&2
    [ $errcode -eq 2 ] && usage "$THIS" "$SYNOPSIS"
    exit $errcode
}

OUTPUT=$(echo "$CONF" | sed -ne '1p')
ARGS=$(echo "$CONF" | sed -e '1d')

