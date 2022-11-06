# pandoc-server

`pandoc-server` is a Haskell library providing access to
pandoc's document conversions as an HTTP server.

For a description of the API, see
[pandoc-server.md](https://github.com/jgm/pandoc/blob/master/doc/pandoc-server.md)
in the pandoc source repository.

Example of use:

``` hs
module Main where
import Text.Pandoc.Server (app)
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.run 3000 app
```

## License

© 2006-2022 John MacFarlane (jgm@berkeley.edu). Released under the
[GPL](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html "GNU General Public License"),
version 2 or greater. This software carries no warranty of any kind.
(See COPYRIGHT for full copyright and warranty notices.)
