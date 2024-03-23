module Main.Syntax.Lexer.Parsing.Shared (getBodyExpSpaces, getBodyExprRange, getParamSigExprRange, getNameSigExprRange, getBodyExprsRange, getBodyBlockExprsadRange) where

import Data.Function (($))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), head, last)
import Data.Text (Text)
import Main.Syntax.Lexer.LexerModel
  ( BodyExpr (AlphaNumBodyExpr, HigherPriorityExpr, NumberBodyExpr, SpecBodyExpr, TextBodyExpr, UnderscoreBodyExpr),
    ExprsBlock (ExprsBlock, ebExprs),
    NameSigExpr (AlphaNumNameSigExpr, SpecNameSigExpr),
    Nested (EmptyNested, InBracketsNested, IndentedNested, ibExprs),
    ParamSigExpr (AlphaNumParamSigExpr, HpParamSigExpr, NumberParamSigExpr, TextParamSigExpr),
  )
import Shared.Location.Data (Anchored (adRange, adSpaces), OcAnchored (oAdSpaces, ocAdRange), Range (Range, from, to))

getBodyExpSpaces :: BodyExpr -> Text
getBodyExpSpaces (AlphaNumBodyExpr i) = adSpaces i
getBodyExpSpaces (SpecBodyExpr i) = adSpaces i
getBodyExpSpaces (UnderscoreBodyExpr i) = adSpaces i
getBodyExpSpaces (NumberBodyExpr i) = adSpaces i
getBodyExpSpaces (TextBodyExpr i) = adSpaces i
getBodyExpSpaces (HigherPriorityExpr (IndentedNested expressions)) = getBodyExpSpaces $ NE.head $ ebExprs $ NE.head expressions
getBodyExpSpaces (HigherPriorityExpr (InBracketsNested {ibExprs = e})) = oAdSpaces e
getBodyExpSpaces (HigherPriorityExpr (EmptyNested _)) = ""

getBodyExprRange :: BodyExpr -> Range
getBodyExprRange (AlphaNumBodyExpr i) = adRange i
getBodyExprRange (SpecBodyExpr i) = adRange i
getBodyExprRange (UnderscoreBodyExpr i) = adRange i
getBodyExprRange (NumberBodyExpr i) = adRange i
getBodyExprRange (TextBodyExpr i) = adRange i
getBodyExprRange (HigherPriorityExpr (IndentedNested expressions)) =
  Range
    { from = from $ getBodyBlockExprRange $ NE.head expressions,
      to = to $ getBodyBlockExprRange $ NE.last expressions
    }
getBodyExprRange (HigherPriorityExpr (InBracketsNested {ibExprs = e})) = ocAdRange e
getBodyExprRange (HigherPriorityExpr (EmptyNested anchored)) = adRange anchored

getBodyExprsRange :: NonEmpty BodyExpr -> Range
getBodyExprsRange exprs@((HigherPriorityExpr (EmptyNested _)) :| nextExpr : _) =
  Range {from = from $ getBodyExprRange nextExpr, to = to $ getBodyExprRange $ NE.last exprs}
getBodyExprsRange exprs =
  Range {from = from $ getBodyExprRange $ NE.head exprs, to = to $ getBodyExprRange $ NE.last exprs}

getBodyBlockExprRange :: ExprsBlock -> Range
getBodyBlockExprRange (ExprsBlock {ebExprs = exprs}) = Range {from = from $ getBodyExprRange $ NE.head exprs, to = to $ getBodyExprRange $ NE.last exprs}

getBodyBlockExprsadRange :: NonEmpty ExprsBlock -> Range
getBodyBlockExprsadRange exprs = Range {from = from $ getBodyBlockExprRange $ NE.head exprs, to = to $ getBodyBlockExprRange $ NE.last exprs}

getParamSigExprRange :: ParamSigExpr -> Range
getParamSigExprRange (AlphaNumParamSigExpr i) = adRange i
getParamSigExprRange (NumberParamSigExpr i) = adRange i
getParamSigExprRange (TextParamSigExpr i) = adRange i
getParamSigExprRange (HpParamSigExpr e) = ocAdRange e

getNameSigExprRange :: NameSigExpr -> Range
getNameSigExprRange (AlphaNumNameSigExpr i) = adRange i
getNameSigExprRange (SpecNameSigExpr i) = adRange i
