module Text.Pandoc.Error (PandocError(..), handleError,hush, mapLeft) where

import Text.Parsec.Error
import Text.Parsec.Pos hiding (Line)
import Text.Pandoc.Compat.Except

type Input = String

data PandocError = ParseFailure String
                 | ParsecError Input ParseError
                 deriving (Show)


instance Error PandocError where
  strMsg = ParseFailure


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

hush :: Either a b -> Maybe b
hush (Left _) = Nothing
hush (Right x) = Just x

handleError :: Either PandocError a -> a
handleError (Right r) = r
handleError (Left err) =
  case err of
    ParseFailure string -> error string
    ParsecError input err' ->
        let errPos = errorPos err'
            errLine = sourceLine errPos
            errColumn = sourceColumn errPos
            theline = (lines input ++ [""]) !! (errLine - 1)
        in  error $ "\nError at " ++ show  err' ++ "\n" ++
                theline ++ "\n" ++ replicate (errColumn - 1) ' ' ++
                "^"

