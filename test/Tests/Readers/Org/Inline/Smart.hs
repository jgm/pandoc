{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Org.Inline.Smart
   Copyright   : © 2014-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Test smart parsing of quotes, apostrophe, etc.
-}
module Tests.Readers.Org.Inline.Smart (tests) where

import Prelude
import Data.Text (Text)
import Test.Tasty (TestTree)
import Tests.Helpers ((=?>), purely, test)
import Text.Pandoc (ReaderOptions (readerExtensions),
                    Extension (Ext_smart), def, enableExtension,
                    getDefaultExtensions, readOrg)
import Text.Pandoc.Builder

orgSmart :: Text -> Pandoc
orgSmart = purely $
  let extensionsSmart = enableExtension Ext_smart (getDefaultExtensions "org")
  in readOrg def{ readerExtensions = extensionsSmart }

tests :: [TestTree]
tests =
  [ test orgSmart "quote before ellipses"
    ("'...hi'"
     =?> para (singleQuoted "…hi"))

  , test orgSmart "apostrophe before emph"
    ("D'oh! A l'/aide/!"
     =?> para ("D’oh! A l’" <> emph "aide" <> "!"))

  , test orgSmart "apostrophe in French"
    ("À l'arrivée de la guerre, le thème de l'«impossibilité du socialisme»"
     =?> para "À l’arrivée de la guerre, le thème de l’«impossibilité du socialisme»")

  , test orgSmart "Quotes cannot occur at the end of emphasized text"
    ("/say \"yes\"/" =?>
     para ("/say" <> space <> doubleQuoted "yes" <> "/"))

  , test orgSmart "Dashes are allowed at the borders of emphasis'"
    ("/foo---/" =?>
     para (emph "foo—"))

  , test orgSmart "Single quotes can be followed by emphasized text"
    ("Singles on the '/meat market/'" =?>
     para ("Singles on the " <> singleQuoted (emph "meat market")))

  , test orgSmart "Double quotes can be followed by emphasized text"
    ("Double income, no kids: \"/DINK/\"" =?>
     para ("Double income, no kids: " <> doubleQuoted (emph "DINK")))
  ]
