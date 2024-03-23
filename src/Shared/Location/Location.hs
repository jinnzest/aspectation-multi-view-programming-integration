module Shared.Location.Location
  ( anchored,
    spaces,
    keyword,
    ocAnchored,
    bracketsAnchored,
    Indent (Indented, NotIndented),
  )
where

import Control.Monad (return, void)
import Data.Char (Char)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor (($>), (<$>))
import Data.Int (Int)
import Data.Kind (Type)
import Data.Ord (Ordering (EQ, GT))
import Data.Text (Text, pack)
import GHC.Show (Show)
import Shared.Conditional.Debug (dbg)
import Shared.Lexer.Data (Lexer)
import Shared.Location.Data
  ( Anchored (Anchored, adItem, adRange, adSpaces),
    BracketsKind (CurlyBrackets, RoundBrackets, SquareBrackets),
    OcAnchored
      ( OcAnchored,
        cAdSpaces,
        oAdSpaces,
        ocAdItem,
        ocAdRange
      ),
    Position (Position, column, line),
    Range (Range, from, to),
  )
import Text.Megaparsec (SourcePos (sourceColumn), getSourcePos, many, (<?>), (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer as TL (indentGuard)
import Text.Megaparsec.Pos (SourcePos (sourceLine), pos1, unPos)

type Indent :: Type
data Indent = Indented | NotIndented deriving stock (Eq)

position :: Lexer Position
position = do
  pos <- getSourcePos
  return
    Position
      { line = unPos $ sourceLine pos,
        column = unPos $ sourceColumn pos
      }

spaces :: Lexer Text
spaces =
  dbg "spaces" $
    pack
      <$> many
        (char '\n' <|> char '\r' <|> char '\t' <|> char ' ')

indentGuard :: Indent -> Lexer ()
indentGuard Indented = TL.indentGuard (return ()) GT pos1 $> ()
indentGuard NotIndented = TL.indentGuard (return ()) EQ pos1 $> ()

anchored :: Indent -> Lexer a -> Lexer (Anchored a)
anchored indented parser = do
  adSpaces <- spaces
  Shared.Location.Location.indentGuard indented
  from <- position
  adItem <- parser
  to <- position
  let adRange = Range {from, to}
  return $ Anchored {adSpaces, adItem, adRange}

keyword :: Indent -> Text -> Lexer (Anchored Text)
keyword indented word = anchored indented $ string word

ocAnchored :: (Show a) => Lexer (Anchored ()) -> Lexer (Anchored ()) -> (Int -> Lexer a) -> Lexer (OcAnchored a)
ocAnchored openAdRangeParser closeAdRangeParse parser = dbg "ocAnchored" $ do
  oAnchored <- openAdRangeParser
  let bracketLine = line $ from $ adRange oAnchored
  ocAdItem <- parser bracketLine
  cAnchored <- closeAdRangeParse
  let ocAdRange =
        Range
          { from = from $ adRange oAnchored,
            to = to $ adRange cAnchored
          }
  return $
    OcAnchored
      { oAdSpaces = adSpaces oAnchored,
        cAdSpaces = adSpaces cAnchored,
        ocAdItem,
        ocAdRange
      }

keychar :: Indent -> Char -> Lexer (Anchored ())
keychar indented kc = anchored indented (void (char kc))

openBracket :: BracketsKind -> Indent -> Lexer (Anchored ())
openBracket RoundBrackets ind = keychar ind '(' <?> "open bracket"
openBracket SquareBrackets ind = keychar ind '[' <?> "open bracket"
openBracket CurlyBrackets ind = keychar ind '{' <?> "open bracket"

closeBracket :: BracketsKind -> Lexer (Anchored ())
closeBracket RoundBrackets = keychar Indented ')' <?> "close bracket"
closeBracket SquareBrackets = keychar Indented ']' <?> "close bracket"
closeBracket CurlyBrackets = keychar Indented '}' <?> "close bracket"

bracketsAnchored :: (Show a) => BracketsKind -> Indent -> (Int -> Lexer a) -> Lexer (OcAnchored a)
bracketsAnchored bracketsKind ind = ocAnchored (openBracket bracketsKind ind) (closeBracket bracketsKind)
