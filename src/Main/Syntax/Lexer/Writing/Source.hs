module Main.Syntax.Lexer.Writing.Source
  ( writeFuncs,
    writeFuncSig,
  )
where

import Control.Category (Category (id))
import Data.Eq (Eq ((==)))
import Data.Foldable (Foldable (foldl))
import Data.Function (const, ($))
import Data.List as L (map, reverse)
import Data.List.NonEmpty as NE (map, toList)
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Monoid (Monoid (mconcat))
import Data.String (String)
import Data.Text as T (Text, pack, unpack)
import Main.Syntax.Lexer.LexerModel
  ( BodyExpr
      ( AlphaNumBodyExpr,
        HigherPriorityExpr,
        NumberBodyExpr,
        SpecBodyExpr,
        TextBodyExpr,
        UnderscoreBodyExpr
      ),
    ExprsBlock (ExprsBlock, ebExprs, ebSemicolon),
    Func (Func, fBody, fSig),
    FuncBody (FuncBody),
    FuncSig (FuncSig, fsName, fsParams),
    FuncSigParams (InfixSigParams, PrefixSigParams),
    HoParamParams (HoInfixParamParams, HoPrefixParamParams),
    HpParamSigExpr (FoParamSigExpr, HoParamSigExpr, hpseName, hpseParams),
    NameSigExpr (AlphaNumNameSigExpr, SpecNameSigExpr),
    Nested (EmptyNested, InBracketsNested, IndentedNested, ibExprs, ibKind),
    Number (Number, dec, exp, mant, sign),
    ParamSigExpr (AlphaNumParamSigExpr, HpParamSigExpr, NumberParamSigExpr, TextParamSigExpr),
  )
import Shared.Location.Data
  ( Anchored (Anchored, adItem, adSpaces),
    BracketsKind (CurlyBrackets, RoundBrackets, SquareBrackets),
    OcAnchored
      ( OcAnchored,
        cAdSpaces,
        oAdSpaces,
        ocAdItem
      ),
  )
import Text.Shakespeare.Text as ST (st)
import Text.Show (Show (show))

writeAnchoredWith :: (a -> Text) -> Anchored a -> Text
writeAnchoredWith builder Anchored {adItem, adSpaces} = [st|#{adSpaces}#{builder adItem}|]

writeAnchored :: Anchored Text -> Text
writeAnchored = writeAnchoredWith id

writeNumber :: Anchored Number -> Text
writeNumber Anchored {adItem = Number {mant, dec, exp, sign}, adSpaces} =
  let decimalText = maybe "" (\d -> [st|.#{show d}|]) dec
      exponentText = maybe "" (\e -> [st|e#{show e}|]) exp
      signed :: String
      signed = if sign then "-" else ""
   in [st|#{adSpaces}#{signed}#{show mant}#{decimalText}#{exponentText}|]

writeParamSigExpr :: ParamSigExpr -> Text
writeParamSigExpr (AlphaNumParamSigExpr r) = writeAnchored r
writeParamSigExpr (NumberParamSigExpr r) = writeNumber r
writeParamSigExpr (TextParamSigExpr Anchored {adItem, adSpaces}) =
  let escapedText = pack $ reverse $ foldl (\acc c -> if c == '"' then c : c : acc else c : acc) "" $ unpack adItem
   in [st|#{adSpaces}"#{escapedText}"|]
writeParamSigExpr (HpParamSigExpr OcAnchored {ocAdItem = FoParamSigExpr expr, oAdSpaces = oS, cAdSpaces = cS}) = [st|#{oS}(#{writeParamSigExpr expr}#{cS})|]
writeParamSigExpr (HpParamSigExpr OcAnchored {ocAdItem = HoParamSigExpr {hpseName, hpseParams = HoPrefixParamParams params}, oAdSpaces = oS, cAdSpaces = cS}) =
  let paramsText = mconcat $ L.map (writeAnchoredWith (const "_")) params
      namesText = writeNameSigExpr hpseName
   in [st|#{oS}(#{namesText}#{paramsText}#{cS})|]
writeParamSigExpr (HpParamSigExpr OcAnchored {ocAdItem = HoParamSigExpr {hpseName, hpseParams = HoInfixParamParams leftParam rightParam}, oAdSpaces = oS, cAdSpaces = cS}) =
  [st|#{oS}(#{writeAnchoredWith (const "_") leftParam}#{writeNameSigExpr hpseName}#{writeAnchoredWith (const "_") rightParam}#{cS})|]

writeNameSigExpr :: NameSigExpr -> Text
writeNameSigExpr (AlphaNumNameSigExpr Anchored {adItem, adSpaces}) = [st|#{adSpaces}#{adItem}|]
writeNameSigExpr (SpecNameSigExpr Anchored {adItem, adSpaces}) = [st|#{adSpaces}#{adItem}|]

writeFuncSig :: FuncSig -> Text
writeFuncSig FuncSig {fsName, fsParams = PrefixSigParams params} =
  mconcat $ writeNameSigExpr fsName : L.map writeParamSigExpr params
writeFuncSig FuncSig {fsName, fsParams = InfixSigParams leftParam rightParam} =
  mconcat [writeParamSigExpr leftParam, writeNameSigExpr fsName, writeParamSigExpr rightParam]

writeExprsBlock :: ExprsBlock -> Text
writeExprsBlock ExprsBlock {ebExprs = expressions, ebSemicolon = semicolon} =
  let exprsText = mconcat $ L.map writeBodyExpr $ NE.toList expressions
      semicolonText = case semicolon of
        Nothing -> ""
        Just Anchored {adSpaces} -> [st|#{adSpaces};|]
   in [st|#{semicolonText}#{exprsText}|]

writeBodyExpr :: BodyExpr -> Text
writeBodyExpr (AlphaNumBodyExpr anchored) = writeAnchored anchored
writeBodyExpr (SpecBodyExpr anchored) = writeAnchored anchored
writeBodyExpr (NumberBodyExpr anchored) = writeNumber anchored
writeBodyExpr (TextBodyExpr Anchored {adItem, adSpaces}) =
  let escapedText = pack $ reverse $ foldl (\acc c -> if c == '"' then c : c : acc else c : acc) "" $ unpack adItem
   in [st|#{adSpaces}"#{escapedText}"|]
writeBodyExpr (HigherPriorityExpr (IndentedNested expressions)) = mconcat $ NE.toList $ NE.map writeExprsBlock expressions
writeBodyExpr (HigherPriorityExpr (InBracketsNested {ibExprs = OcAnchored {ocAdItem = expressions, oAdSpaces = oS, cAdSpaces = cS}, ibKind})) =
  let joinedExpressions = mconcat $ NE.toList $ NE.map writeExprsBlock expressions
   in case ibKind of
        RoundBrackets -> [st|#{oS}(#{joinedExpressions}#{cS})|]
        SquareBrackets -> [st|#{oS}[#{joinedExpressions}#{cS}]|]
        CurlyBrackets -> [st|#{oS}{#{joinedExpressions}#{cS}}|]
writeBodyExpr (HigherPriorityExpr (EmptyNested {})) = ""
writeBodyExpr (UnderscoreBodyExpr r) = writeAnchoredWith (const "_") r

writeFunc :: Anchored Func -> Text
writeFunc Anchored {adItem = Func {fSig, fBody = FuncBody Anchored {adItem = expressions, adSpaces = bS}}, adSpaces} =
  let sigText = writeFuncSig fSig
      bodyText = mconcat $ NE.toList $ NE.map writeExprsBlock expressions
   in [st|#{adSpaces}#{sigText}#{bS}->#{bodyText}|]

writeFuncs :: [Anchored Func] -> Text
writeFuncs items = mconcat $ L.map writeFunc items
