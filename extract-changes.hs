-- Extract changes from latest version in changelog.
import Text.Pandoc.JSON

main = toJSONFilter extractFirst

extractFirst :: Pandoc -> Pandoc
extractFirst (Pandoc meta (Para{} : BulletList bs : _)) =
  Pandoc meta [BulletList bs]
extractFirst x = x
