# As a security measure refuse to proceed if mktemp is not available.
pathfind mktemp || { err "Couldn't find 'mktemp'; aborting."; exit 1;  }

THIS_TEMPDIR=
THIS_TEMPDIR="$(mktemp -d -t $THIS.XXXXXXXX)" || exit 1
readonly THIS_TEMPDIR

trap 'exitcode=$?
      [ -z "$THIS_TEMPDIR" ] || rm -rf "$THIS_TEMPDIR"
      exit $exitcode' 0 1 2 3 13 15
