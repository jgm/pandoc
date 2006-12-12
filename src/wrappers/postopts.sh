# Parse wrapper and wrappee (pandoc) arguments by taking
# into account that they may have space or tab characters.
pick="WRAPPER_ARGS"
while [ $# -gt 0 ]; do
    if [ "$pick" = "WRAPPER_ARGS" ]; then
        case "$1" in
        -*) pick="WRAPPEE_ARGS" ;;
        esac
    fi
    # Pack args with NEWLINE to preserve spaces,
    # and put them into the picked variable.
    eval "$pick=\"\$${pick}${NEWLINE}${1}\""
    shift
done

# Unpack filename arguments.  Now "$@" will hold the filenames.
oldifs="$IFS"; IFS="$NEWLINE"; set -- $WRAPPER_ARGS; IFS="$oldifs"
