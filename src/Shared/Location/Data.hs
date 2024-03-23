module Shared.Location.Data
  ( Range (Range, from, to),
    Anchored (Anchored, adItem, adRange, adSpaces),
    OcAnchored (OcAnchored, ocAdItem, ocAdRange, oAdSpaces, cAdSpaces),
    Position (Position, line, column),
    BracketsKind (RoundBrackets, SquareBrackets, CurlyBrackets),
  )
where

import Data.Eq (Eq)
import Data.Hashable (Hashable)
import Data.Int (Int)
import Data.Kind (Type)
import Data.Ord (Ord)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import GHC.Show (Show (show))
import Shared.Conditional.Loc (locEnabled)
import Text.Shakespeare.Text (ToText (toText), st)

type Position :: Type
data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Hashable Position

type Range :: Type
data Range = Range
  { from :: Position,
    to :: Position
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Hashable Range

type Anchored :: Type -> Type
data Anchored a = Anchored
  { adSpaces :: Text,
    adRange :: Range,
    adItem :: a
  }
  deriving stock (Eq, Ord, Generic)

instance (Hashable a) => Hashable (Anchored a)

instance (Show a) => Show (Anchored a) where
  show Anchored {adItem, adRange, adSpaces} =
    if locEnabled
      then unpack [st|Anchored{adItem=#{show adItem},adRange=#{adRange},adSpaces=#{adSpaces}}|]
      else unpack [st|Anchored{adItem=#{show adItem}}|]

type OcAnchored :: Type -> Type
data OcAnchored a = OcAnchored
  { oAdSpaces :: Text,
    ocAdRange :: Range,
    ocAdItem :: a,
    cAdSpaces :: Text
  }
  deriving stock (Eq, Ord, Generic)

type BracketsKind :: Type
data BracketsKind
  = RoundBrackets
  | SquareBrackets
  | CurlyBrackets
  deriving stock (Eq, Ord, Show, Generic)

instance Hashable BracketsKind

instance (Hashable a) => Hashable (OcAnchored a)

instance (Show a) => Show (OcAnchored a) where
  show OcAnchored {ocAdItem, ocAdRange, oAdSpaces, cAdSpaces} =
    if locEnabled
      then unpack [st|OcAnchored{ocAdItem=#{show ocAdItem},ocAdRange=#{ocAdRange},oAdSpaces=#{oAdSpaces},cAdSpaces=#{cAdSpaces}}|]
      else unpack [st|OcAnchored{ocAdItem=#{show ocAdItem}}|]

instance ToText Position where
  toText Position {line = l, column = c} = toText [st|#{l}:#{c}|]

instance ToText Range where
  toText Range {from = f, to = t} = toText [st|#{f} - #{t}|]

instance (ToText a) => ToText (Anchored a) where
  toText Anchored {adItem = i, adRange = r} = toText [st|#{r} #{i}|]

instance (ToText a) => ToText (OcAnchored a) where
  toText OcAnchored {ocAdItem = i, ocAdRange = r} = toText [st|#{r} #{i}|]

instance ToText BracketsKind where
  toText RoundBrackets = "RoundBrackets"
  toText CurlyBrackets = "CurlyBrackets"
  toText SquareBrackets = "SquareBrackets"
