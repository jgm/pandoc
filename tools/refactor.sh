cppdir=`stack path --dist-dir`/build/autogen
for f in $*
do
    echo "Applying hlint --refactor to $f"
    hlint --refactor -i --cpp-file=$cppdir/cabal_macros.h $f >/dev/null
    echo "Applying stylish-haskell to $f"
    stylish-haskell -i $f | perl -ne 'if (/./) { print "\n" x $n, $_; $n = 0 } else { $n++ }' >/dev/null
done
