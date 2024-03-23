{-# LANGUAGE GADTs #-}

module Main.Syntax.Lexer.LexerModel
  ( Func (Func, fBody, fSig),
    LexerModel,
    Number (Number, mant, dec, exp, sign),
    Nested (InBracketsNested, IndentedNested, EmptyNested, ibExprs, ibKind),
    BodyExpr
      ( AlphaNumBodyExpr,
        SpecBodyExpr,
        NumberBodyExpr,
        UnderscoreBodyExpr,
        TextBodyExpr,
        HigherPriorityExpr
      ),
    ParamSigExpr
      ( AlphaNumParamSigExpr,
        NumberParamSigExpr,
        TextParamSigExpr,
        HpParamSigExpr
      ),
    NameSigExpr
      ( AlphaNumNameSigExpr,
        SpecNameSigExpr
      ),
    HpParamSigExpr (FoParamSigExpr, HoParamSigExpr, hpseName, hpseParams, hpseKind),
    HoParamParams (HoInfixParamParams, HoPrefixParamParams),
    ExprsBlock (ExprsBlock, ebExprs, ebSemicolon),
    FuncSig (FuncSig, fsName, fsParams, fsURL),
    FuncBody (FuncBody),
    FuncSigParams (PrefixSigParams, InfixSigParams),
  )
where

import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (Maybe)
import Data.Ord (Ord)
import Data.String (fromString)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Integer (Integer)
import GHC.Show (Show, show)
import Shared.Location.Data (Anchored, BracketsKind, OcAnchored)
import Text.Shakespeare.Text as ST (ToText (toText))

type Number :: Type
data Number = Number
  { mant :: Integer,
    dec :: Maybe Integer,
    exp :: Maybe Integer,
    sign :: Bool
  }
  deriving stock (Eq, Ord, Generic, Show)

type ExprsBlock :: Type
data ExprsBlock = ExprsBlock {ebExprs :: NonEmpty BodyExpr, ebSemicolon :: Maybe (Anchored ())}
  deriving stock (Eq, Ord, Generic, Show)

type Nested :: Type
data Nested
  = IndentedNested (NonEmpty ExprsBlock)
  | InBracketsNested {ibExprs :: OcAnchored (NonEmpty ExprsBlock), ibKind :: BracketsKind}
  | EmptyNested (Anchored ())
  deriving stock (Eq, Ord, Generic, Show)

type BodyExpr :: Type
data BodyExpr
  = AlphaNumBodyExpr (Anchored Text)
  | SpecBodyExpr (Anchored Text)
  | NumberBodyExpr (Anchored Number)
  | UnderscoreBodyExpr (Anchored ())
  | TextBodyExpr (Anchored Text)
  | HigherPriorityExpr Nested
  deriving stock (Eq, Ord, Generic, Show)

type HoParamParams :: Type
data HoParamParams = HoInfixParamParams (Anchored ()) (Anchored ()) | HoPrefixParamParams [Anchored ()]
  deriving stock (Eq, Ord, Generic, Show)

type NameSigExpr :: Type
data NameSigExpr = AlphaNumNameSigExpr (Anchored Text) | SpecNameSigExpr (Anchored Text)
  deriving stock (Eq, Ord, Generic, Show)

type HpParamSigExpr :: Type
data HpParamSigExpr
  = HoParamSigExpr {hpseName :: NameSigExpr, hpseParams :: HoParamParams, hpseKind :: BracketsKind}
  | FoParamSigExpr ParamSigExpr
  deriving stock (Eq, Ord, Generic, Show)

type ParamSigExpr :: Type
data ParamSigExpr
  = AlphaNumParamSigExpr (Anchored Text)
  | NumberParamSigExpr (Anchored Number)
  | TextParamSigExpr (Anchored Text)
  | HpParamSigExpr (OcAnchored HpParamSigExpr)
  deriving stock (Eq, Ord, Generic, Show)

type FuncBody :: Type
newtype FuncBody = FuncBody (Anchored (NonEmpty ExprsBlock))
  deriving stock (Eq, Generic, Show)

type FuncSigParams :: Type
data FuncSigParams = PrefixSigParams [ParamSigExpr] | InfixSigParams ParamSigExpr ParamSigExpr
  deriving stock (Eq, Generic, Show)

type FuncSig :: Type
data FuncSig = FuncSig
  { fsName :: NameSigExpr,
    fsParams :: FuncSigParams,
    fsURL :: Text
  }
  deriving stock (Eq, Generic, Show)

type Func :: Type
data Func = Func
  { fSig :: FuncSig,
    fBody :: FuncBody
  }
  deriving stock (Eq, Generic, Show)

type LexerModel :: Type
type LexerModel = [Anchored Func]

instance Hashable Number

instance Hashable HoParamParams

instance Hashable HpParamSigExpr

instance Hashable ParamSigExpr

instance Hashable NameSigExpr

instance Hashable Nested

instance Hashable BodyExpr

instance Hashable ExprsBlock

instance Hashable FuncSigParams

instance Hashable FuncSig

instance Hashable FuncBody

instance Hashable Func

instance ToText Number where
  toText = fromString . show

instance ToText ParamSigExpr where
  toText = fromString . show

instance ToText HpParamSigExpr where
  toText = fromString . show

instance ToText ExprsBlock where
  toText = fromString . show

instance ToText (NonEmpty ExprsBlock) where
  toText = fromString . show

instance ToText FuncSig where
  toText = fromString . show

instance ToText BodyExpr where
  toText = fromString . show

instance ToText [BodyExpr] where
  toText = fromString . show

instance ToText [[BodyExpr]] where
  toText = fromString . show

instance ToText Func where
  toText = fromString . show

instance ToText LexerModel where
  toText = fromString . show
