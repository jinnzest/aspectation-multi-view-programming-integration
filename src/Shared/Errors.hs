module Shared.Errors
  ( Errors (Errors),
    errors,
    Error
      ( MultiAnchoredError,
        mrError,
        AnchoredError,
        rError,
        eadRange,
        PositionedError,
        position,
        pError,
        Error
      ),
  )
where

import Data.Eq (Eq)
import Data.Function ((.))
import Data.Kind (Type)
import Data.String (fromString)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Show (Show, show)
import Shared.Location.Data (Position, Range)
import Text.Shakespeare.Text (ToText (toText))

type Errors :: Type
newtype Errors = Errors
  { errors :: [Error]
  }
  deriving stock (Eq, Generic, Show)

type Error :: Type
data Error
  = MultiAnchoredError {adRanges :: [Range], mrError :: Text}
  | AnchoredError {eadRange :: Range, rError :: Text}
  | PositionedError {position :: Position, pError :: Text}
  | Error Text
  deriving stock (Eq, Generic, Show)

instance ToText Errors where
  toText = fromString . show
