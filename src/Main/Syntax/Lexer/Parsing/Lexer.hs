module Main.Syntax.Lexer.Parsing.Lexer
  ( Lexer,
    lexing,
  )
where

import Control.Applicative ((<$>), (<*))
import Control.Monad (Monad (return), MonadFail (fail), void, when)
import Data.Char (Char)
import Data.Eq (Eq ((==)))
import Data.Function (($))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (Maybe (Just))
import Data.Text as T (Text, pack)
import GHC.Num (Num ((*)))
import GHC.Num.Integer (Integer)
import Main.Syntax.Lexer.LexerModel
  ( BodyExpr (AlphaNumBodyExpr, HigherPriorityExpr, NumberBodyExpr, SpecBodyExpr, TextBodyExpr, UnderscoreBodyExpr),
    ExprsBlock,
    Func (Func, fBody, fSig),
    FuncBody (FuncBody),
    FuncSig (FuncSig, fsName, fsParams, fsURL),
    FuncSigParams (InfixSigParams, PrefixSigParams),
    HoParamParams (HoInfixParamParams, HoPrefixParamParams),
    HpParamSigExpr (FoParamSigExpr, HoParamSigExpr, hpseKind, hpseName, hpseParams),
    NameSigExpr (AlphaNumNameSigExpr, SpecNameSigExpr),
    Nested (InBracketsNested, ibExprs, ibKind),
    Number (Number, dec, exp, mant, sign),
    ParamSigExpr (AlphaNumParamSigExpr, HpParamSigExpr, NumberParamSigExpr, TextParamSigExpr),
  )
import Main.Syntax.Lexer.Parsing.Layouting (mkLayouts)
import Shared.Conditional.Debug (dbg)
import Shared.Lexer.Data (Lexer)
import Shared.Location.Data (Anchored (adRange), BracketsKind (CurlyBrackets, RoundBrackets, SquareBrackets), OcAnchored (OcAnchored, cAdSpaces, oAdSpaces, ocAdItem, ocAdRange), Position (line), Range (from))
import Shared.Location.Location
  ( Indent (Indented, NotIndented),
    anchored,
    bracketsAnchored,
    keyword,
    spaces,
  )
import System.Directory.Internal.Prelude (Int)
import Text.Megaparsec
  ( anySingleBut,
    eof,
    hidden,
    many,
    notFollowedBy,
    optional,
    some,
    try,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char as TMC (alphaNumChar, char, letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

specChar :: Lexer Char
specChar =
  char '+'
    <|> char '-'
    <|> char '='
    <|> char '*'
    <|> char '/'
    <|> char '^'
    <|> char '&'
    <|> char ','
    <|> char '<'
    <|> char '>'
    <|> char '|'
    <|> char '?'
    <|> char '\''
    <|> char '%'
    <|> char ':'
    <|> char '.'
    <|> char '~'
    <|> char '!'
    <|> char '#'
    <|> char '@'
    <|> char '$'
    <|> char '`'
    <|> char '\\'

underscoreChar :: Lexer Char
underscoreChar = char '_' <?> "underscore"

semicolonChar :: Lexer Char
semicolonChar = char ';' <?> "semicolon"

specBody :: Lexer Text
specBody = (pack <$> some specChar) <?> "special characters identifier"

specSig :: Lexer Text
specSig =
  dbg
    "specSig"
    ( hidden
        ( do
            res <- pack <$> some specChar
            when (res == "->") $ fail "\"->\" is not allowed inside function signature" :: Lexer ()
            return res
        )
        <?> "special characters identifier"
    )

alphaNumChar :: Lexer Char
alphaNumChar = TMC.alphaNumChar <|> underscoreChar

alphaNum :: Lexer Text
alphaNum =
  dbg
    "alphaNum"
    $ do
      an <-
        hidden
          (pack <$> some Main.Syntax.Lexer.Parsing.Lexer.alphaNumChar)
          <?> "alphanumeric identifier"
      notFollowedBy $ char '.'
      if an == "_" then fail "alphanumeric can't be a single underscore" else return an

number :: Lexer Number
number =
  dbg
    "number"
    ( ( ( do
            signed <- optional $ char '-'
            let sign = signed == Just '-'
            mant <- hidden decimal
            dec <- optional $ try $ do
              void $ hidden $ char '.'
              hidden decimal
            exp <- optional $ try $ do
              void $ hidden (char 'e' <|> char 'E')
              expSigned <- optional $ char '-'
              let expSign :: Integer
                  expSign = if expSigned == Just '-' then -1 else 1
              expDec <- hidden decimal
              return $ expSign * expDec
            notFollowedBy letterChar
            return Number {mant, dec, exp, sign}
        )
          <?> "number"
      )
        <* notFollowedBy (char '_' <|> TMC.alphaNumChar)
    )

escapeQuotes :: Lexer Char
escapeQuotes = string "\"\"" $> '\"'

text :: Lexer Text
text =
  ( do
      void $ char '"'
      txt <- many (try (anySingleBut '\"') <|> try escapeQuotes)
      void $ char '"'
      return $ pack txt
  )
    <?> "text"

bodyBlocks :: Int -> Lexer (NonEmpty ExprsBlock)
bodyBlocks baseLine = do
  exprs <- some bodyExpr
  return $ mkLayouts baseLine exprs

tryBrackets :: BracketsKind -> Indent -> Lexer (BracketsKind, OcAnchored (NonEmpty ExprsBlock))
tryBrackets kind ind = try ((kind,) <$> bracketsAnchored kind ind bodyBlocks)

inBracketsExpr :: Indent -> Lexer Nested
inBracketsExpr ind = dbg "inBracketsExpr" $ do
  (ibKind, anchoredExprs) <-
    tryBrackets RoundBrackets ind
      <|> tryBrackets SquareBrackets ind
      <|> tryBrackets CurlyBrackets ind
  let exprs = ocAdItem anchoredExprs
  return InBracketsNested {ibExprs = OcAnchored {ocAdItem = exprs, ocAdRange = ocAdRange anchoredExprs, oAdSpaces = oAdSpaces anchoredExprs, cAdSpaces = cAdSpaces anchoredExprs}, ibKind}

anchoredUnderscore :: Lexer (Anchored ())
anchoredUnderscore = anchored Indented $ void underscoreChar

semicolon :: Lexer Text
semicolon = semicolonChar $> ";"

bodyExpr :: Lexer BodyExpr
bodyExpr =
  dbg "bodyExpr" $
    try (HigherPriorityExpr <$> inBracketsExpr Indented)
      <|> try (NumberBodyExpr <$> anchored Indented number)
      <|> try (AlphaNumBodyExpr <$> anchored Indented alphaNum)
      <|> try (UnderscoreBodyExpr <$> anchoredUnderscore)
      <|> try (SpecBodyExpr <$> anchored Indented specBody)
      <|> try (SpecBodyExpr <$> anchored Indented semicolon)
      <|> try (TextBodyExpr <$> anchored Indented text)

funcBody :: Lexer FuncBody
funcBody =
  dbg "funcBody" $
    FuncBody
      <$> anchored
        Indented
        ( do
            arrowItem <- keyword Indented "->"
            exprs <- some bodyExpr
            let arrowLine = line $ from $ adRange arrowItem
            return $ mkLayouts arrowLine exprs
        )

infixHoParametrizedParamSigExpr :: BracketsKind -> Lexer HpParamSigExpr
infixHoParametrizedParamSigExpr hpseKind = dbg "infixHoParametrizedParamSigExpr" $ do
  leftParam <- anchoredUnderscore
  hpseName <- try (AlphaNumNameSigExpr <$> anchored Indented alphaNum) <|> try (SpecNameSigExpr <$> anchored Indented specSig)
  rightParam <- anchoredUnderscore
  return HoParamSigExpr {hpseName, hpseKind, hpseParams = HoInfixParamParams leftParam rightParam}

prefixHoParametrizedParamSigExpr :: BracketsKind -> Lexer HpParamSigExpr
prefixHoParametrizedParamSigExpr hpseKind = dbg "prefixHoParametrizedParamSigExpr" $ do
  hpseName <- try (AlphaNumNameSigExpr <$> anchored Indented alphaNum) <|> try (SpecNameSigExpr <$> anchored Indented specSig)
  params <- some $ try anchoredUnderscore
  return HoParamSigExpr {hpseName, hpseKind, hpseParams = HoPrefixParamParams params}

sharedParamSigExpr :: Indent -> Lexer ParamSigExpr
sharedParamSigExpr ind =
  try (NumberParamSigExpr <$> anchored ind number)
    <|> try (AlphaNumParamSigExpr <$> anchored ind alphaNum)
    <|> try (TextParamSigExpr <$> anchored ind text)

hpFirstOrderParamSigExpr :: Lexer HpParamSigExpr
hpFirstOrderParamSigExpr = dbg "hpFirstOrderParamSigExpr" $ FoParamSigExpr <$> sharedParamSigExpr Indented

hpParamSigExprBrackets :: BracketsKind -> Indent -> Lexer (OcAnchored HpParamSigExpr)
hpParamSigExprBrackets bracketsKind ind =
  bracketsAnchored
    bracketsKind
    ind
    ( \_ ->
        try (prefixHoParametrizedParamSigExpr bracketsKind)
          <|> try hpFirstOrderParamSigExpr
          <|> try (infixHoParametrizedParamSigExpr bracketsKind)
    )

hpParamSigExpr :: Indent -> Lexer (OcAnchored HpParamSigExpr)
hpParamSigExpr ind = do
  dbg "hpParamSigExpr" $
    try (hpParamSigExprBrackets RoundBrackets ind) <|> try (hpParamSigExprBrackets SquareBrackets ind) <|> try (hpParamSigExprBrackets CurlyBrackets ind)

paramSigExpr :: Indent -> Lexer ParamSigExpr
paramSigExpr ind =
  dbg "paramSigExpr" $
    try (sharedParamSigExpr ind)
      <|> try (HpParamSigExpr <$> hpParamSigExpr ind)

specName :: Indent -> Lexer NameSigExpr
specName ind = dbg "specName" $ SpecNameSigExpr <$> anchored ind specSig <* notFollowedBy (decimal :: Lexer Integer)

alphaNumName :: Indent -> Lexer NameSigExpr
alphaNumName ind = dbg "alphaNumName" $ AlphaNumNameSigExpr <$> anchored ind alphaNum

funcName :: Indent -> Lexer NameSigExpr
funcName ind = dbg "funcName" $ try (alphaNumName ind) <|> try (specName ind)

alphaNumInfixFuncSig :: Indent -> Text -> Lexer FuncSig
alphaNumInfixFuncSig ind fsURL = dbg "alphaNumInfixFuncSig" $ do
  leftParam <- HpParamSigExpr <$> hpParamSigExpr ind
  fsName <- alphaNumName Indented
  rightParam <- HpParamSigExpr <$> hpParamSigExpr Indented
  return
    FuncSig
      { fsName,
        fsParams = InfixSigParams leftParam rightParam,
        fsURL
      }

specInfixFuncSig :: Indent -> Text -> Lexer FuncSig
specInfixFuncSig ind fsURL = dbg "specInfixFuncSig" $ do
  leftParam <- paramSigExpr ind
  fsName <- specName Indented
  rightParam <- paramSigExpr Indented
  return
    FuncSig
      { fsName,
        fsParams = InfixSigParams leftParam rightParam,
        fsURL
      }

prefixFuncSig :: Indent -> Text -> Lexer FuncSig
prefixFuncSig ind fsURL = dbg "prefixFuncSig" $ do
  fsName <- funcName ind
  fsParams <- many (paramSigExpr Indented)
  return
    FuncSig
      { fsName,
        fsParams = PrefixSigParams fsParams,
        fsURL
      }

infixFuncSig :: Indent -> Text -> Lexer FuncSig
infixFuncSig ind pfsURL = try (alphaNumInfixFuncSig ind pfsURL) <|> try (specInfixFuncSig ind pfsURL)

funcSig :: Text -> Lexer FuncSig
funcSig fURL =
  dbg "funcSig" $
    try (infixFuncSig NotIndented fURL) <|> try (prefixFuncSig NotIndented fURL)

func :: Text -> Lexer Func
func fURL = dbg "func" $ do
  fSig <- funcSig fURL
  fBody <- funcBody
  return Func {fSig, fBody}

lexing :: Text -> Lexer ([Anchored Func], Text)
lexing fileURL = do
  funcs <- dbg "funcs" $ some $ try $ anchored NotIndented (func fileURL)
  sp <- spaces
  eof
  return (funcs, sp)
