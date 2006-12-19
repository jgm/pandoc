-- | Default headers for Pandoc writers.
module Text.Pandoc.Writers.DefaultHeaders  (
                                      defaultLaTeXHeader,
                                      defaultHtmlHeader,
                                      defaultS5Header,
                                      defaultRTFHeader
                                      ) where
import Text.Pandoc.Writers.S5

defaultLaTeXHeader :: String
defaultLaTeXHeader = "@LaTeXHeader@"

defaultHtmlHeader :: String
defaultHtmlHeader = "@HtmlHeader@"

defaultS5Header :: String
defaultS5Header = "@S5Header@" ++ s5CSS ++ s5Javascript

defaultRTFHeader :: String
defaultRTFHeader = "@RTFHeader@"
