-- | Default headers for Pandoc writers.
module Text.Pandoc.Writers.DefaultHeaders  (
                                      defaultLaTeXHeader,
                                      defaultConTeXtHeader,
                                      defaultDocbookHeader,
                                      defaultS5Header,
                                      defaultRTFHeader
                                      ) where
import Text.Pandoc.Writers.S5

defaultLaTeXHeader :: String
defaultLaTeXHeader = "@LaTeXHeader@"

defaultConTeXtHeader :: String
defaultConTeXtHeader = "@ConTeXtHeader@"

defaultDocbookHeader :: String
defaultDocbookHeader = "@DocbookHeader@"

defaultS5Header :: String
defaultS5Header = s5Meta ++ s5CSS ++ s5Javascript

defaultRTFHeader :: String
defaultRTFHeader = "@RTFHeader@"
