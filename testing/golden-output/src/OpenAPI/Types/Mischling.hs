-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

-- | Contains the types generated from the schema Mischling
module OpenAPI.Types.Mischling where

import qualified Prelude as GHC.Integer.Type
import qualified Prelude as GHC.Maybe
import qualified Control.Monad.Fail
import qualified Data.Aeson
import qualified Data.Aeson as Data.Aeson.Encoding.Internal
import qualified Data.Aeson as Data.Aeson.Types
import qualified Data.Aeson as Data.Aeson.Types.FromJSON
import qualified Data.Aeson as Data.Aeson.Types.ToJSON
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified Data.ByteString
import qualified Data.ByteString as Data.ByteString.Internal
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Scientific
import qualified Data.Text
import qualified Data.Text as Data.Text.Internal
import qualified Data.Time.Calendar as Data.Time.Calendar.Days
import qualified Data.Time.LocalTime as Data.Time.LocalTime.Internal.ZonedTime
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Int
import qualified GHC.Show
import qualified GHC.Types
import qualified OpenAPI.Common
import OpenAPI.TypeAlias
import {-# SOURCE #-} OpenAPI.Types.Cat
import {-# SOURCE #-} OpenAPI.Types.Dog
import {-# SOURCE #-} OpenAPI.Types.PetByAge
import {-# SOURCE #-} OpenAPI.Types.PetByType

-- | Defines the object schema located at @components.schemas.Mischling.allOf@ in the specification.
-- 
-- 
data Mischling = Mischling {
  -- | age
  mischlingAge :: GHC.Types.Int
  -- | ageThird
  , mischlingAgeThird :: (GHC.Maybe.Maybe GHC.Types.Int)
  -- | another_relative
  , mischlingAnother_relative :: (GHC.Maybe.Maybe MischlingAnother_relativeVariants)
  -- | bark
  , mischlingBark :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | binary
  , mischlingBinary :: (GHC.Maybe.Maybe OpenAPI.Common.JsonByteString)
  -- | breed
  , mischlingBreed :: (GHC.Maybe.Maybe MischlingBreed)
  -- | byte
  , mischlingByte :: (GHC.Maybe.Maybe OpenAPI.Common.JsonByteString)
  -- | double
  , mischlingDouble :: (GHC.Maybe.Maybe GHC.Types.Double)
  -- | father
  , mischlingFather :: (GHC.Maybe.Maybe Data.Aeson.Types.Internal.Object)
  -- | first_relative
  , mischlingFirst_relative :: (GHC.Maybe.Maybe MischlingFirst_relative)
  -- | float
  -- 
  -- Constraints:
  -- 
  -- * Must be a multiple of 1.0e-2
  , mischlingFloat :: (GHC.Maybe.Maybe GHC.Types.Float)
  -- | huntssecond
  , mischlingHuntssecond :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | int32
  , mischlingInt32 :: (GHC.Maybe.Maybe GHC.Int.Int32)
  -- | int64
  -- 
  -- Constraints:
  -- 
  -- * Must be a multiple of 4.0
  , mischlingInt64 :: (GHC.Maybe.Maybe GHC.Int.Int64)
  -- | integer
  , mischlingInteger :: (GHC.Maybe.Maybe GHC.Types.Int)
  -- | nickname
  , mischlingNickname :: (GHC.Maybe.Maybe Data.Text.Internal.Text)
  -- | number
  , mischlingNumber :: (GHC.Maybe.Maybe GHC.Types.Double)
  -- | pet_type
  , mischlingPet_type :: (GHC.Maybe.Maybe Data.Aeson.Types.Internal.Object)
  -- | relative
  , mischlingRelative :: (GHC.Maybe.Maybe MischlingRelativeVariants)
  -- | secondFather
  , mischlingSecondFather :: (GHC.Maybe.Maybe Data.Aeson.Types.Internal.Object)
  -- | str
  -- 
  -- Constraints:
  -- 
  -- * Maximum length of 244
  -- * Minimum length of 100
  , mischlingStr :: (GHC.Maybe.Maybe Data.Text.Internal.Text)
  -- | strDate
  , mischlingStrDate :: (GHC.Maybe.Maybe Data.Text.Internal.Text)
  -- | strDateTime
  , mischlingStrDateTime :: (GHC.Maybe.Maybe Data.Text.Internal.Text)
  -- | tags
  , mischlingTags :: (GHC.Maybe.Maybe ([Data.Text.Internal.Text]))
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON Mischling
    where {toJSON obj = Data.Aeson.Types.Internal.object (Data.Foldable.concat (["age" Data.Aeson.Types.ToJSON..= mischlingAge obj] : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("ageThird" Data.Aeson.Types.ToJSON..=)) (mischlingAgeThird obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("another_relative" Data.Aeson.Types.ToJSON..=)) (mischlingAnother_relative obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("bark" Data.Aeson.Types.ToJSON..=)) (mischlingBark obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("binary" Data.Aeson.Types.ToJSON..=)) (mischlingBinary obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("breed" Data.Aeson.Types.ToJSON..=)) (mischlingBreed obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("byte" Data.Aeson.Types.ToJSON..=)) (mischlingByte obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("double" Data.Aeson.Types.ToJSON..=)) (mischlingDouble obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("father" Data.Aeson.Types.ToJSON..=)) (mischlingFather obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("first_relative" Data.Aeson.Types.ToJSON..=)) (mischlingFirst_relative obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("float" Data.Aeson.Types.ToJSON..=)) (mischlingFloat obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("huntssecond" Data.Aeson.Types.ToJSON..=)) (mischlingHuntssecond obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("int32" Data.Aeson.Types.ToJSON..=)) (mischlingInt32 obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("int64" Data.Aeson.Types.ToJSON..=)) (mischlingInt64 obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("integer" Data.Aeson.Types.ToJSON..=)) (mischlingInteger obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("nickname" Data.Aeson.Types.ToJSON..=)) (mischlingNickname obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("number" Data.Aeson.Types.ToJSON..=)) (mischlingNumber obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("pet_type" Data.Aeson.Types.ToJSON..=)) (mischlingPet_type obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("relative" Data.Aeson.Types.ToJSON..=)) (mischlingRelative obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("secondFather" Data.Aeson.Types.ToJSON..=)) (mischlingSecondFather obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("str" Data.Aeson.Types.ToJSON..=)) (mischlingStr obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("strDate" Data.Aeson.Types.ToJSON..=)) (mischlingStrDate obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("strDateTime" Data.Aeson.Types.ToJSON..=)) (mischlingStrDateTime obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("tags" Data.Aeson.Types.ToJSON..=)) (mischlingTags obj) : GHC.Base.mempty));
           toEncoding obj = Data.Aeson.Encoding.Internal.pairs (GHC.Base.mconcat (Data.Foldable.concat (["age" Data.Aeson.Types.ToJSON..= mischlingAge obj] : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("ageThird" Data.Aeson.Types.ToJSON..=)) (mischlingAgeThird obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("another_relative" Data.Aeson.Types.ToJSON..=)) (mischlingAnother_relative obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("bark" Data.Aeson.Types.ToJSON..=)) (mischlingBark obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("binary" Data.Aeson.Types.ToJSON..=)) (mischlingBinary obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("breed" Data.Aeson.Types.ToJSON..=)) (mischlingBreed obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("byte" Data.Aeson.Types.ToJSON..=)) (mischlingByte obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("double" Data.Aeson.Types.ToJSON..=)) (mischlingDouble obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("father" Data.Aeson.Types.ToJSON..=)) (mischlingFather obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("first_relative" Data.Aeson.Types.ToJSON..=)) (mischlingFirst_relative obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("float" Data.Aeson.Types.ToJSON..=)) (mischlingFloat obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("huntssecond" Data.Aeson.Types.ToJSON..=)) (mischlingHuntssecond obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("int32" Data.Aeson.Types.ToJSON..=)) (mischlingInt32 obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("int64" Data.Aeson.Types.ToJSON..=)) (mischlingInt64 obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("integer" Data.Aeson.Types.ToJSON..=)) (mischlingInteger obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("nickname" Data.Aeson.Types.ToJSON..=)) (mischlingNickname obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("number" Data.Aeson.Types.ToJSON..=)) (mischlingNumber obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("pet_type" Data.Aeson.Types.ToJSON..=)) (mischlingPet_type obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("relative" Data.Aeson.Types.ToJSON..=)) (mischlingRelative obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("secondFather" Data.Aeson.Types.ToJSON..=)) (mischlingSecondFather obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("str" Data.Aeson.Types.ToJSON..=)) (mischlingStr obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("strDate" Data.Aeson.Types.ToJSON..=)) (mischlingStrDate obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("strDateTime" Data.Aeson.Types.ToJSON..=)) (mischlingStrDateTime obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("tags" Data.Aeson.Types.ToJSON..=)) (mischlingTags obj) : GHC.Base.mempty)))}
instance Data.Aeson.Types.FromJSON.FromJSON Mischling
    where {parseJSON = Data.Aeson.Types.FromJSON.withObject "Mischling" (\obj -> (((((((((((((((((((((((GHC.Base.pure Mischling GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "age")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "ageThird")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "another_relative")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "bark")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "binary")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "breed")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "byte")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "double")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "father")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "first_relative")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "float")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "huntssecond")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "int32")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "int64")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "integer")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "nickname")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "number")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "pet_type")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "relative")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "secondFather")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "str")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "strDate")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "strDateTime")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "tags"))}
-- | Create a new 'Mischling' with all required fields.
mkMischling :: GHC.Types.Int -- ^ 'mischlingAge'
  -> Mischling
mkMischling mischlingAge = Mischling{mischlingAge = mischlingAge,
                                     mischlingAgeThird = GHC.Maybe.Nothing,
                                     mischlingAnother_relative = GHC.Maybe.Nothing,
                                     mischlingBark = GHC.Maybe.Nothing,
                                     mischlingBinary = GHC.Maybe.Nothing,
                                     mischlingBreed = GHC.Maybe.Nothing,
                                     mischlingByte = GHC.Maybe.Nothing,
                                     mischlingDouble = GHC.Maybe.Nothing,
                                     mischlingFather = GHC.Maybe.Nothing,
                                     mischlingFirst_relative = GHC.Maybe.Nothing,
                                     mischlingFloat = GHC.Maybe.Nothing,
                                     mischlingHuntssecond = GHC.Maybe.Nothing,
                                     mischlingInt32 = GHC.Maybe.Nothing,
                                     mischlingInt64 = GHC.Maybe.Nothing,
                                     mischlingInteger = GHC.Maybe.Nothing,
                                     mischlingNickname = GHC.Maybe.Nothing,
                                     mischlingNumber = GHC.Maybe.Nothing,
                                     mischlingPet_type = GHC.Maybe.Nothing,
                                     mischlingRelative = GHC.Maybe.Nothing,
                                     mischlingSecondFather = GHC.Maybe.Nothing,
                                     mischlingStr = GHC.Maybe.Nothing,
                                     mischlingStrDate = GHC.Maybe.Nothing,
                                     mischlingStrDateTime = GHC.Maybe.Nothing,
                                     mischlingTags = GHC.Maybe.Nothing}
-- | Defines the object schema located at @components.schemas.Mischling.allOf.properties.another_relative.oneOf@ in the specification.
-- 
-- 
data MischlingAnother_relativeOneOf5 = MischlingAnother_relativeOneOf5 {
  -- | hunts
  mischlingAnother_relativeOneOf5Hunts :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | pet_type
  , mischlingAnother_relativeOneOf5Pet_type :: (GHC.Maybe.Maybe Data.Text.Internal.Text)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON MischlingAnother_relativeOneOf5
    where {toJSON obj = Data.Aeson.Types.Internal.object (Data.Foldable.concat (Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("hunts" Data.Aeson.Types.ToJSON..=)) (mischlingAnother_relativeOneOf5Hunts obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("pet_type" Data.Aeson.Types.ToJSON..=)) (mischlingAnother_relativeOneOf5Pet_type obj) : GHC.Base.mempty));
           toEncoding obj = Data.Aeson.Encoding.Internal.pairs (GHC.Base.mconcat (Data.Foldable.concat (Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("hunts" Data.Aeson.Types.ToJSON..=)) (mischlingAnother_relativeOneOf5Hunts obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("pet_type" Data.Aeson.Types.ToJSON..=)) (mischlingAnother_relativeOneOf5Pet_type obj) : GHC.Base.mempty)))}
instance Data.Aeson.Types.FromJSON.FromJSON MischlingAnother_relativeOneOf5
    where {parseJSON = Data.Aeson.Types.FromJSON.withObject "MischlingAnother_relativeOneOf5" (\obj -> (GHC.Base.pure MischlingAnother_relativeOneOf5 GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "hunts")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "pet_type"))}
-- | Create a new 'MischlingAnother_relativeOneOf5' with all required fields.
mkMischlingAnother_relativeOneOf5 :: MischlingAnother_relativeOneOf5
mkMischlingAnother_relativeOneOf5 = MischlingAnother_relativeOneOf5{mischlingAnother_relativeOneOf5Hunts = GHC.Maybe.Nothing,
                                                                    mischlingAnother_relativeOneOf5Pet_type = GHC.Maybe.Nothing}
-- | Defines the oneOf schema located at @components.schemas.Mischling.allOf.properties.another_relative.oneOf@ in the specification.
-- 
-- 
data MischlingAnother_relativeVariants =
   MischlingAnother_relativeEmptyString -- ^ Represents the JSON value @""@
  | MischlingAnother_relativeTest -- ^ Represents the JSON value @"test"@
  | MischlingAnother_relativeCat Cat
  | MischlingAnother_relativePetByType PetByType
  | MischlingAnother_relativeText Data.Text.Internal.Text
  | MischlingAnother_relativeListTText ([Data.Text.Internal.Text])
  | MischlingAnother_relativeMischlingAnother_relativeOneOf5 MischlingAnother_relativeOneOf5
  deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON MischlingAnother_relativeVariants
    where {toJSON (MischlingAnother_relativeCat a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (MischlingAnother_relativePetByType a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (MischlingAnother_relativeText a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (MischlingAnother_relativeListTText a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (MischlingAnother_relativeMischlingAnother_relativeOneOf5 a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (MischlingAnother_relativeEmptyString) = "";
           toJSON (MischlingAnother_relativeTest) = "test"}
instance Data.Aeson.Types.FromJSON.FromJSON MischlingAnother_relativeVariants
    where {parseJSON val = if | val GHC.Classes.== "" -> GHC.Base.pure MischlingAnother_relativeEmptyString
                              | val GHC.Classes.== "test" -> GHC.Base.pure MischlingAnother_relativeTest
                              | GHC.Base.otherwise -> case (MischlingAnother_relativeCat Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((MischlingAnother_relativePetByType Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((MischlingAnother_relativeText Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((MischlingAnother_relativeListTText Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((MischlingAnother_relativeMischlingAnother_relativeOneOf5 Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> Data.Aeson.Types.Internal.Error "No variant matched")))) of
                                                      {Data.Aeson.Types.Internal.Success a -> GHC.Base.pure a;
                                                       Data.Aeson.Types.Internal.Error a -> Control.Monad.Fail.fail a}}
-- | Defines the enum schema located at @components.schemas.Mischling.allOf.properties.breed@ in the specification.
-- 
-- 
data MischlingBreed =
   MischlingBreedOther Data.Aeson.Types.Internal.Value -- ^ This case is used if the value encountered during decoding does not match any of the provided cases in the specification.
  | MischlingBreedTyped Data.Text.Internal.Text -- ^ This constructor can be used to send values to the server which are not present in the specification yet.
  | MischlingBreedEnumDingo -- ^ Represents the JSON value @"Dingo"@
  | MischlingBreedEnumHusky -- ^ Represents the JSON value @"Husky"@
  | MischlingBreedEnumRetriever -- ^ Represents the JSON value @"Retriever"@
  | MischlingBreedEnumShepherd -- ^ Represents the JSON value @"Shepherd"@
  deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON MischlingBreed
    where {toJSON (MischlingBreedOther val) = val;
           toJSON (MischlingBreedTyped val) = Data.Aeson.Types.ToJSON.toJSON val;
           toJSON (MischlingBreedEnumDingo) = "Dingo";
           toJSON (MischlingBreedEnumHusky) = "Husky";
           toJSON (MischlingBreedEnumRetriever) = "Retriever";
           toJSON (MischlingBreedEnumShepherd) = "Shepherd"}
instance Data.Aeson.Types.FromJSON.FromJSON MischlingBreed
    where {parseJSON val = GHC.Base.pure (if | val GHC.Classes.== "Dingo" -> MischlingBreedEnumDingo
                                             | val GHC.Classes.== "Husky" -> MischlingBreedEnumHusky
                                             | val GHC.Classes.== "Retriever" -> MischlingBreedEnumRetriever
                                             | val GHC.Classes.== "Shepherd" -> MischlingBreedEnumShepherd
                                             | GHC.Base.otherwise -> MischlingBreedOther val)}
-- | Defines the object schema located at @components.schemas.Mischling.allOf.properties.first_relative.allOf@ in the specification.
-- 
-- 
data MischlingFirst_relative = MischlingFirst_relative {
  -- | age
  mischlingFirst_relativeAge :: (GHC.Maybe.Maybe GHC.Types.Int)
  -- | another_relative
  , mischlingFirst_relativeAnother_relative :: (GHC.Maybe.Maybe MischlingFirst_relativeAnother_relativeVariants)
  -- | hunts
  , mischlingFirst_relativeHunts :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | pet_type
  , mischlingFirst_relativePet_type :: MischlingFirst_relativePet_type
  -- | relative
  , mischlingFirst_relativeRelative :: (GHC.Maybe.Maybe MischlingFirst_relativeRelativeVariants)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON MischlingFirst_relative
    where {toJSON obj = Data.Aeson.Types.Internal.object (Data.Foldable.concat (Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("age" Data.Aeson.Types.ToJSON..=)) (mischlingFirst_relativeAge obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("another_relative" Data.Aeson.Types.ToJSON..=)) (mischlingFirst_relativeAnother_relative obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("hunts" Data.Aeson.Types.ToJSON..=)) (mischlingFirst_relativeHunts obj) : ["pet_type" Data.Aeson.Types.ToJSON..= mischlingFirst_relativePet_type obj] : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("relative" Data.Aeson.Types.ToJSON..=)) (mischlingFirst_relativeRelative obj) : GHC.Base.mempty));
           toEncoding obj = Data.Aeson.Encoding.Internal.pairs (GHC.Base.mconcat (Data.Foldable.concat (Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("age" Data.Aeson.Types.ToJSON..=)) (mischlingFirst_relativeAge obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("another_relative" Data.Aeson.Types.ToJSON..=)) (mischlingFirst_relativeAnother_relative obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("hunts" Data.Aeson.Types.ToJSON..=)) (mischlingFirst_relativeHunts obj) : ["pet_type" Data.Aeson.Types.ToJSON..= mischlingFirst_relativePet_type obj] : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("relative" Data.Aeson.Types.ToJSON..=)) (mischlingFirst_relativeRelative obj) : GHC.Base.mempty)))}
instance Data.Aeson.Types.FromJSON.FromJSON MischlingFirst_relative
    where {parseJSON = Data.Aeson.Types.FromJSON.withObject "MischlingFirst_relative" (\obj -> ((((GHC.Base.pure MischlingFirst_relative GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "age")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "another_relative")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "hunts")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "pet_type")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "relative"))}
-- | Create a new 'MischlingFirst_relative' with all required fields.
mkMischlingFirst_relative :: MischlingFirst_relativePet_type -- ^ 'mischlingFirst_relativePet_type'
  -> MischlingFirst_relative
mkMischlingFirst_relative mischlingFirst_relativePet_type = MischlingFirst_relative{mischlingFirst_relativeAge = GHC.Maybe.Nothing,
                                                                                    mischlingFirst_relativeAnother_relative = GHC.Maybe.Nothing,
                                                                                    mischlingFirst_relativeHunts = GHC.Maybe.Nothing,
                                                                                    mischlingFirst_relativePet_type = mischlingFirst_relativePet_type,
                                                                                    mischlingFirst_relativeRelative = GHC.Maybe.Nothing}
-- | Defines the oneOf schema located at @components.schemas.Mischling.allOf.properties.first_relative.allOf.properties.another_relative.oneOf@ in the specification.
-- 
-- 
data MischlingFirst_relativeAnother_relativeVariants =
   MischlingFirst_relativeAnother_relativeCat Cat
  | MischlingFirst_relativeAnother_relativePetByType PetByType
  | MischlingFirst_relativeAnother_relativeText Data.Text.Internal.Text
  deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON MischlingFirst_relativeAnother_relativeVariants
    where {toJSON (MischlingFirst_relativeAnother_relativeCat a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (MischlingFirst_relativeAnother_relativePetByType a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (MischlingFirst_relativeAnother_relativeText a) = Data.Aeson.Types.ToJSON.toJSON a}
instance Data.Aeson.Types.FromJSON.FromJSON MischlingFirst_relativeAnother_relativeVariants
    where {parseJSON val = case (MischlingFirst_relativeAnother_relativeCat Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((MischlingFirst_relativeAnother_relativePetByType Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((MischlingFirst_relativeAnother_relativeText Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> Data.Aeson.Types.Internal.Error "No variant matched")) of
                           {Data.Aeson.Types.Internal.Success a -> GHC.Base.pure a;
                            Data.Aeson.Types.Internal.Error a -> Control.Monad.Fail.fail a}}
-- | Defines the enum schema located at @components.schemas.Mischling.allOf.properties.first_relative.allOf.properties.pet_type@ in the specification.
-- 
-- 
data MischlingFirst_relativePet_type =
   MischlingFirst_relativePet_typeOther Data.Aeson.Types.Internal.Value -- ^ This case is used if the value encountered during decoding does not match any of the provided cases in the specification.
  | MischlingFirst_relativePet_typeTyped Data.Text.Internal.Text -- ^ This constructor can be used to send values to the server which are not present in the specification yet.
  | MischlingFirst_relativePet_typeEnumCat -- ^ Represents the JSON value @"Cat"@
  | MischlingFirst_relativePet_typeEnumDog -- ^ Represents the JSON value @"Dog"@
  deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON MischlingFirst_relativePet_type
    where {toJSON (MischlingFirst_relativePet_typeOther val) = val;
           toJSON (MischlingFirst_relativePet_typeTyped val) = Data.Aeson.Types.ToJSON.toJSON val;
           toJSON (MischlingFirst_relativePet_typeEnumCat) = "Cat";
           toJSON (MischlingFirst_relativePet_typeEnumDog) = "Dog"}
instance Data.Aeson.Types.FromJSON.FromJSON MischlingFirst_relativePet_type
    where {parseJSON val = GHC.Base.pure (if | val GHC.Classes.== "Cat" -> MischlingFirst_relativePet_typeEnumCat
                                             | val GHC.Classes.== "Dog" -> MischlingFirst_relativePet_typeEnumDog
                                             | GHC.Base.otherwise -> MischlingFirst_relativePet_typeOther val)}
-- | Defines the oneOf schema located at @components.schemas.Mischling.allOf.properties.first_relative.allOf.properties.relative.anyOf@ in the specification.
-- 
-- 
data MischlingFirst_relativeRelativeVariants =
   MischlingFirst_relativeRelativeCat Cat
  | MischlingFirst_relativeRelativePetByType PetByType
  | MischlingFirst_relativeRelativeText Data.Text.Internal.Text
  deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON MischlingFirst_relativeRelativeVariants
    where {toJSON (MischlingFirst_relativeRelativeCat a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (MischlingFirst_relativeRelativePetByType a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (MischlingFirst_relativeRelativeText a) = Data.Aeson.Types.ToJSON.toJSON a}
instance Data.Aeson.Types.FromJSON.FromJSON MischlingFirst_relativeRelativeVariants
    where {parseJSON val = case (MischlingFirst_relativeRelativeCat Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((MischlingFirst_relativeRelativePetByType Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((MischlingFirst_relativeRelativeText Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> Data.Aeson.Types.Internal.Error "No variant matched")) of
                           {Data.Aeson.Types.Internal.Success a -> GHC.Base.pure a;
                            Data.Aeson.Types.Internal.Error a -> Control.Monad.Fail.fail a}}
-- | Defines the oneOf schema located at @components.schemas.Mischling.allOf.properties.relative.anyOf@ in the specification.
-- 
-- 
data MischlingRelativeVariants =
   MischlingRelativeCat Cat
  | MischlingRelativePetByType PetByType
  | MischlingRelativeText Data.Text.Internal.Text
  deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON MischlingRelativeVariants
    where {toJSON (MischlingRelativeCat a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (MischlingRelativePetByType a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (MischlingRelativeText a) = Data.Aeson.Types.ToJSON.toJSON a}
instance Data.Aeson.Types.FromJSON.FromJSON MischlingRelativeVariants
    where {parseJSON val = case (MischlingRelativeCat Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((MischlingRelativePetByType Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((MischlingRelativeText Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> Data.Aeson.Types.Internal.Error "No variant matched")) of
                           {Data.Aeson.Types.Internal.Success a -> GHC.Base.pure a;
                            Data.Aeson.Types.Internal.Error a -> Control.Monad.Fail.fail a}}
