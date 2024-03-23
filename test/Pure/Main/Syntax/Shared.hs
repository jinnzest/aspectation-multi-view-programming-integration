module Pure.Main.Syntax.Shared
  ( runLexer,
    mkRange,
    mkAnchored,
    mkOcAnchored,
    mkOcAnchoredWS,
    mkAnchoredWS,
  )
where

import Control.Monad.Except (runExceptT)
import Data.Either (Either)
import Data.Function (($))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Int (Int)
import Data.Text (Text)
import Main.Syntax.Lexer.Parsing.LexerIO (lexingText)
import Shared.Errors (Errors)
import Shared.Lexer.Data (Lexer)
import Shared.Location.Data (Anchored (Anchored), OcAnchored (OcAnchored), Position (Position), Range (Range), adItem, adRange, adSpaces, cAdSpaces, column, from, line, oAdSpaces, ocAdItem, ocAdRange, to)

runLexer :: Text -> Lexer (a, Text) -> Either Errors (a, Text)
runLexer body lexer = runIdentity $ runExceptT $ lexingText "" body lexer

mkAnchoredWS :: Int -> Int -> Int -> Int -> Text -> a -> Anchored a
mkAnchoredWS lineFrom columnFrom lineTo columnTo adSpaces adItem =
  Anchored
    { adItem,
      adRange = mkRange lineFrom columnFrom lineTo columnTo,
      adSpaces
    }

mkRange :: Int -> Int -> Int -> Int -> Range
mkRange lineFrom columnFrom lineTo columnTo =
  Range
    { from = Position {line = lineFrom, column = columnFrom},
      to = Position {line = lineTo, column = columnTo}
    }

mkAnchored :: Int -> Int -> Int -> Int -> a -> Anchored a
mkAnchored lineFrom columnFrom lineTo columnTo = mkAnchoredWS lineFrom columnFrom lineTo columnTo ""

mkOcAnchoredWS :: Int -> Int -> Int -> Int -> Text -> Text -> a -> OcAnchored a
mkOcAnchoredWS lineFrom columnFrom lineTo columnTo oAdSpaces cAdSpaces ocAdItem =
  OcAnchored
    { ocAdItem,
      ocAdRange = mkRange lineFrom columnFrom lineTo columnTo,
      oAdSpaces,
      cAdSpaces
    }

mkOcAnchored :: Int -> Int -> Int -> Int -> a -> OcAnchored a
mkOcAnchored lineFrom columnFrom lineTo columnTo = mkOcAnchoredWS lineFrom columnFrom lineTo columnTo "" ""
