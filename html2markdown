#!/bin/sh -e
# converts HTML from a URL, file, or stdin to markdown
# uses an available program to fetch URL and tidy to normalize it first

REQUIRED="tidy"
SYNOPSIS="converts HTML from a URL, file, or STDIN to markdown-formatted text."

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


grab_url_with () {
    url="${1:?internal error: grab_url_with: url required}"

    shift
    cmdline="$@"

    prog=
    prog_opts=
    if [ -n "$cmdline" ]; then
	eval "set -- $cmdline"
	prog=$1
	shift
	prog_opts="$@"
    fi

    if [ -z "$prog" ]; then
	# Locate a sensible web grabber (note the order).
	for p in wget lynx w3m curl links w3c; do
		if pathfind $p; then
		    prog=$p
		    break
		fi
	done

	[ -n "$prog" ] || {
            errn "$THIS:  Couldn't find a program to fetch the file from URL "
	    err "(e.g. wget, w3m, lynx, w3c, or curl)."
	    return 1
	}
    else
	pathfind "$prog" || {
	    err "$THIS:  No such web grabber '$prog' found; aborting."
	    return 1
	}
    fi

    # Setup proper base options for known grabbers.
    base_opts=
    case "$prog" in
    wget)  base_opts="-O-" ;;
    lynx)  base_opts="-source" ;;
    w3m)   base_opts="-dump_source" ;;
    curl)  base_opts="" ;;
    links) base_opts="-source" ;;
    w3c)   base_opts="-n -get" ;;
    *)     err "$THIS:  unhandled web grabber '$prog'; hope it succeeds."
    esac

    err "$THIS: invoking '$prog $base_opts $prog_opts $url'..."
    eval "set -- $base_opts $prog_opts"
    $prog "$@" "$url"
}

# Parse command-line arguments
parse_arguments () {
    while [ $# -gt 0 ]; do
        case "$1" in
            --encoding=*)
                wholeopt="$1"
                # extract encoding from after =
                encoding="${wholeopt#*=}" ;;
            -e|--encoding|-encoding)
                shift
                encoding="$1" ;; 
            --grabber=*)
                wholeopt="$1"
                # extract encoding from after =
                grabber="\"${wholeopt#*=}\"" ;;
            -g|--grabber|-grabber)
                shift
                grabber="$1" ;; 
            *)
                if [ -z "$argument" ]; then
                    argument="$1"
                else
                    err "Warning:  extra argument '$1' will be ignored."
                fi ;;
            esac
        shift
    done
}

argument=
encoding=
grabber=

oldifs="$IFS"
IFS=$NEWLINE
parse_arguments $ARGS
IFS="$oldifs"

inurl=
if [ -n "$argument" ] && ! [ -f "$argument" ]; then
    # Treat given argument as an URL.
    inurl="$argument"
fi

# As a security measure refuse to proceed if mktemp is not available.
pathfind mktemp || { err "Couldn't find 'mktemp'; aborting."; exit 1;  }

# Avoid issues with /tmp directory on Windows/Cygwin 
cygwin=
cygwin=$(uname | sed -ne '/^CYGWIN/p')
if [ -n "$cygwin" ]; then
    TMPDIR=.
    export TMPDIR
fi

THIS_TEMPDIR=
THIS_TEMPDIR="$(mktemp -d -t $THIS.XXXXXXXX)" || exit 1
readonly THIS_TEMPDIR

trap 'exitcode=$?
      [ -z "$THIS_TEMPDIR" ] || rm -rf "$THIS_TEMPDIR"
      exit $exitcode' 0 1 2 3 13 15

if [ -n "$inurl" ]; then
    err "Attempting to fetch file from '$inurl'..."

    grabber_out=$THIS_TEMPDIR/grabber.out
    grabber_log=$THIS_TEMPDIR/grabber.log
    if ! grab_url_with "$inurl" "$grabber" 1>$grabber_out 2>$grabber_log; then
        errn "grab_url_with failed"
        if [ -f $grabber_log ]; then
            err " with the following error log."
            err
            cat >&2 $grabber_log
        else
            err .
        fi
        exit 1
    fi

    argument="$grabber_out"
fi

if [ -z "$encoding" ] && [ "x$argument" != "x" ]; then
    # Try to determine character encoding if not specified
    # and input is not STDIN.
    encoding=$(
        head "$argument" |
        LC_ALL=C tr 'A-Z' 'a-z' |
        sed -ne '/<meta .*content-type.*charset=/ {
            s/.*charset=["'\'']*\([-a-zA-Z0-9]*\).*["'\'']*/\1/p
        }'
    )
fi

if [ -n "$encoding" ] && pathfind iconv; then
    alias to_utf8='iconv -f "$encoding" -t utf-8'
else # assume UTF-8
    alias to_utf8='cat'
fi 

htmlinput=$THIS_TEMPDIR/htmlinput

if [ -z "$argument" ]; then
    to_utf8 > $htmlinput                # read from STDIN
elif [ -f "$argument" ]; then
    to_utf8 "$argument" > $htmlinput    # read from file
else
    err "File '$argument' not found."
    exit 1
fi

if ! cat $htmlinput | pandoc --ignore-args -r html -w markdown "$@" ; then
     err "Failed to parse HTML.  Trying again with tidy..."
     tidy -q -asxhtml -utf8 $htmlinput | \
        pandoc --ignore-args -r html -w markdown "$@"
fi
