{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Defines functionality for the generation of models from OpenAPI schemas
module OpenAPI.Generate.Model where

-- ( getSchemaType,
--   resolveSchemaReferenceWithoutWarning,
--   getConstraintDescriptionsOfSchema,
--   defineModelForSchemaNamed,
--   defineModelForSchema,
--   TypeWithDeclaration,
-- )

import Control.Applicative
import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Either as E
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Scientific as Scientific
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Calendar
import Debug.Trace (trace)
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import Language.Haskell.TH.Syntax
import qualified OpenAPI.Common as OC
import OpenAPI.Generate.Doc (appendDoc, emptyDoc)
import qualified OpenAPI.Generate.Doc as Doc
import OpenAPI.Generate.Internal.Util
import qualified OpenAPI.Generate.ModelDependencies as Dep
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.OptParse as OAO
import OpenAPI.Generate.OptParse.Types
import qualified OpenAPI.Generate.Types as OAT
import qualified OpenAPI.Generate.Types.Schema as OAS
import Unsafe.Coerce
import Prelude hiding (maximum, minimum)

-- | The type of a model and the declarations needed for defining it
type TypeWithDeclaration = (Q Type, Dep.ModelContentWithDependencies)

type BangTypesSelfDefined = (Q [VarBangType], Q Doc, Dep.Models)

data TypeAliasStrategy = CreateTypeAlias | DontCreateTypeAlias
  deriving (Show, Eq, Ord)

addDependencies :: Dep.Models -> OAM.Generator (TypeWithDeclaration, TypeWithDeclaration) -> OAM.Generator (TypeWithDeclaration, TypeWithDeclaration)
addDependencies dependenciesToAdd typeDef = do
  ((type', (content, dependencies)), (type'', (content', dependencies'))) <- typeDef
  pure ((type', (content, Set.union dependencies dependenciesToAdd)), (type'', (content', Set.union dependencies' dependenciesToAdd)))

-- | default derive clause for the objects
objectDeriveClause :: [Q DerivClause]
objectDeriveClause =
  [ derivClause
      Nothing
      [ conT ''Show,
        conT ''Eq
      ]
  ]

liftAesonValueWithOverloadedStrings :: Bool -> Aeson.Value -> Q Exp
liftAesonValueWithOverloadedStrings useOverloadedStrings (Aeson.String a) =
  let s = stringE $ T.unpack a
   in if useOverloadedStrings
        then [|$s|]
        else [|Aeson.String $s|]
liftAesonValueWithOverloadedStrings _ (Aeson.Number n) =
  -- Without the manual handling of numbers, TH tries to use
  -- `Scientific.Scientific` which is not exposed.
  let coefficient = Scientific.coefficient n
      base10Exponent = Scientific.base10Exponent n
   in [|Aeson.Number (Scientific.scientific coefficient base10Exponent)|]
liftAesonValueWithOverloadedStrings _ a = [|a|]

liftAesonValue :: Aeson.Value -> Q Exp
liftAesonValue = liftAesonValueWithOverloadedStrings True

aesonValueToName :: Aeson.Value -> Text
aesonValueToName =
  ( \case
      "" -> "EmptyString"
      x -> x
  )
    . uppercaseFirstText
    . T.replace "\"" ""
    . showAesonValue

showAesonValue :: Aeson.Value -> Text
showAesonValue = LT.toStrict . Aeson.encodeToLazyText

-- | Defines all the models for a schema
defineModelForSchema :: Bool -> Text -> OAS.Schema -> OAM.Generator (Dep.ModelWithDependencies, Dep.ModelWithDependencies)
defineModelForSchema shouldGenTypes schemaName schema = do
  let aliasWithText description =
        createAlias schemaName description CreateTypeAlias $
          pure (([t|Aeson.Value|], (emptyDoc, Set.empty)), ([t|Aeson.Value|], (emptyDoc, Set.empty)))
      blackListAlias = aliasWithText "This alias is created because of the generator configuration and possibly could have a more precise type."
      whiteListAlias = aliasWithText $ "This is just a type synonym and possibly could have a more precise type because the schema name @" <> schemaName <> "@ is not whitelisted."
  settingOpaqueSchemas <- OAM.getSetting OAO.settingOpaqueSchemas
  whiteListedSchemas <- OAM.getSetting OAO.settingWhiteListedSchemas
  (namedSchema, mkDep) <-
    OAM.nested schemaName $
      if schemaName `elem` settingOpaqueSchemas
        then blackListAlias
        else if null whiteListedSchemas || schemaName `elem` whiteListedSchemas then defineModelForSchemaNamedWithTypeAliasStrategy shouldGenTypes CreateTypeAlias schemaName schema else whiteListAlias
  pure ((transformToModuleName schemaName, snd namedSchema), ("Trans" <> (transformToModuleName schemaName), snd mkDep))

-- | Defines all the models for a schema and returns the declarations with the type of the root model
defineModelForSchemaNamed :: Bool -> Text -> OAS.Schema -> OAM.Generator (TypeWithDeclaration, TypeWithDeclaration)
defineModelForSchemaNamed shouldGenTypes = defineModelForSchemaNamedWithTypeAliasStrategy shouldGenTypes DontCreateTypeAlias

defineModelForSchemaNamed' :: Bool -> Text -> OAS.Schema -> OAM.Generator (TypeWithDeclaration)
defineModelForSchemaNamed' shouldGenTypes text schema = fst <$> defineModelForSchemaNamedWithTypeAliasStrategy shouldGenTypes DontCreateTypeAlias text schema

-- | defines the definitions for a schema and returns a type to the "entrypoint" of the schema
defineModelForSchemaNamedWithTypeAliasStrategy :: Bool -> TypeAliasStrategy -> Text -> OAS.Schema -> OAM.Generator (TypeWithDeclaration, TypeWithDeclaration)
defineModelForSchemaNamedWithTypeAliasStrategy shouldGenTypes strategy schemaName schema =
  case schema of
    OAT.Concrete concrete -> defineModelForSchemaConcrete shouldGenTypes strategy schemaName concrete
    OAT.Reference reference -> do
      refName <- haskellifyNameM True $ getSchemaNameFromReference reference
      OAM.logTrace $ "Encountered reference '" <> reference <> "' which references the type '" <> T.pack (nameBase refName) <> "'"
      pure ((varT refName, (emptyDoc, transformReferenceToDependency reference)), (varT refName, (emptyDoc, transformReferenceToDependency reference)))

getSchemaNameFromReference :: Text -> Text
getSchemaNameFromReference = T.replace "#/components/schemas/" ""

transformReferenceToDependency :: Text -> Set.Set Text
transformReferenceToDependency = Set.singleton . transformToModuleName . getSchemaNameFromReference

-- | Transforms a 'OAS.Schema' (either a reference or a concrete object) to @'Maybe' 'OAS.SchemaObject'@
-- If a reference is found it is resolved. If it is not found, no log message is generated.
resolveSchemaReferenceWithoutWarning :: OAS.Schema -> OAM.Generator (Maybe OAS.SchemaObject)
resolveSchemaReferenceWithoutWarning schema =
  case schema of
    OAT.Concrete concrete -> pure $ Just concrete
    OAT.Reference ref -> OAM.getSchemaReferenceM ref

resolveSchemaReference :: Text -> OAS.Schema -> OAM.Generator (Maybe (OAS.SchemaObject, Dep.Models))
resolveSchemaReference schemaName schema =
  case schema of
    OAT.Concrete concrete -> pure $ Just (concrete, Set.empty)
    OAT.Reference ref -> do
      p <- OAM.getSchemaReferenceM ref
      when (Maybe.isNothing p) $
        OAM.logWarning $
          "Reference '"
            <> ref
            <> "' to schema from '"
            <> schemaName
            <> "' could not be found and therefore will be skipped."
      pure $ (,transformReferenceToDependency ref) <$> p

-- | creates an alias depending on the strategy
createAlias :: Text -> Text -> TypeAliasStrategy -> OAM.Generator (TypeWithDeclaration, TypeWithDeclaration) -> OAM.Generator (TypeWithDeclaration, TypeWithDeclaration)
createAlias schemaName description strategy res = do
  schemaName' <- haskellifyNameM True schemaName
  ((type', (content, dependencies)), (type'', (content', dependencies'))) <- res
  path <- getCurrentPathEscaped
  pure $ case strategy of
    CreateTypeAlias ->
      ( ( type',
          ( content
              `appendDoc` ( ( Doc.generateHaddockComment
                                [ "Defines an alias for the schema located at @" <> path <> "@ in the specification.",
                                  "",
                                  description
                                ]
                                $$
                            )
                              . ppr
                              <$> tySynD schemaName' [] type'
                          ),
            dependencies
          )
        ),
        ( type'',
          ( content'
              `appendDoc` ( ( Doc.generateHaddockComment
                                [ "Defines an alias for the schema located at @" <> path <> "@ in the specification.",
                                  "",
                                  description
                                ]
                                $$
                            )
                              . ppr
                              <$> tySynD schemaName' [] type''
                          ),
            dependencies'
          )
        )
      )
    DontCreateTypeAlias -> ((type', (content, dependencies)), (type'', (content', dependencies')))

-- | returns the type of a schema. Second return value is a 'Q' Monad, for the types that have to be created
defineModelForSchemaConcrete :: Bool -> TypeAliasStrategy -> Text -> OAS.SchemaObject -> OAM.Generator (TypeWithDeclaration, TypeWithDeclaration)
defineModelForSchemaConcrete shouldGenTypes strategy schemaName schema = do
  nonNullableTypeSuffix <- OAM.getSetting OAO.settingNonNullableTypeSuffix
  let enumValues = OAS.schemaObjectEnum schema
      schemaNameWithNonNullableSuffix = if OAS.schemaObjectNullable schema then schemaName <> nonNullableTypeSuffix else schemaName
  typeWithDeclaration <-
    if null enumValues
      then defineModelForSchemaConcreteIgnoreEnum shouldGenTypes strategy schemaNameWithNonNullableSuffix schema
      else defineEnumModel shouldGenTypes schemaNameWithNonNullableSuffix schema enumValues
  if OAS.schemaObjectNullable schema
    then defineNullableTypeAlias strategy schemaName typeWithDeclaration
    else pure typeWithDeclaration

defineNullableTypeAlias :: TypeAliasStrategy -> Text -> (TypeWithDeclaration, TypeWithDeclaration) -> OAM.Generator (TypeWithDeclaration, TypeWithDeclaration)
defineNullableTypeAlias strategy schemaName ((type', (content, dependencies)), (type'', (content', dependencies'))) = do
  nonNullableTypeSuffix <- OAM.getSetting OAO.settingNonNullableTypeSuffix
  let nullableType = appT (varT ''OC.Nullable) type'
  let nullableType' = appT (varT ''OC.Nullable) type''
  case strategy of
    CreateTypeAlias -> do
      path <- getCurrentPathEscaped
      name <- haskellifyNameM True schemaName
      pure
        ( ( varT name,
            ( content
                `appendDoc` ( ( Doc.generateHaddockComment
                                  [ "Defines a nullable type alias for '"
                                      <> schemaName
                                      <> nonNullableTypeSuffix
                                      <> "' as the schema located at @"
                                      <> path
                                      <> "@ in the specification is marked as nullable."
                                  ]
                                  $$
                              )
                                . ppr
                                <$> tySynD name [] nullableType
                            ),
              dependencies
            )
          ),
          ( varT name,
            ( content'
                `appendDoc` ( ( Doc.generateHaddockComment
                                  [ "Defines a nullable type alias for '"
                                      <> schemaName
                                      <> nonNullableTypeSuffix
                                      <> "' as the schema located at @"
                                      <> path
                                      <> "@ in the specification is marked as nullable."
                                  ]
                                  $$
                              )
                                . ppr
                                <$> tySynD name [] nullableType'
                            ),
              dependencies'
            )
          )
        )
    DontCreateTypeAlias -> pure ((nullableType, (content, dependencies)), (nullableType', (content', dependencies')))

-- | Creates a Model, ignores enum values
defineModelForSchemaConcreteIgnoreEnum :: Bool -> TypeAliasStrategy -> Text -> OAS.SchemaObject -> OAM.Generator (TypeWithDeclaration, TypeWithDeclaration)
defineModelForSchemaConcreteIgnoreEnum shouldGenTypes strategy schemaName schema = do
  settings <- OAM.getSettings
  let schemaDescription = getDescriptionOfSchema schema
      typeAliasing = createAlias schemaName schemaDescription strategy
  case schema of
    OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeArray} -> defineArrayModelForSchema shouldGenTypes strategy schemaName schema
    OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeObject} ->
      let allOfNull = null $ OAS.schemaObjectAllOf schema
          oneOfNull = null $ OAS.schemaObjectOneOf schema
          anyOfNull = null $ OAS.schemaObjectAnyOf schema
       in case (allOfNull, oneOfNull, anyOfNull) of
            (False, _, _) -> OAM.nested "allOf" $ defineAllOfSchema shouldGenTypes schemaName schemaDescription $ OAS.schemaObjectAllOf schema
            (_, False, _) -> OAM.nested "oneOf" $ typeAliasing $ defineOneOfSchema shouldGenTypes schemaName schemaDescription $ OAS.schemaObjectOneOf schema
            (_, _, False) -> OAM.nested "anyOf" $ defineAnyOfSchema shouldGenTypes strategy schemaName schemaDescription $ OAS.schemaObjectAnyOf schema
            _ -> defineObjectModelForSchema shouldGenTypes strategy schemaName schema
    _ ->
      typeAliasing $ pure ((varT $ getSchemaType settings schema, (emptyDoc, Set.empty)), (varT $ getSchemaType settings schema, (emptyDoc, Set.empty)))

defineEnumModel :: Bool -> Text -> OAS.SchemaObject -> [Aeson.Value] -> OAM.Generator (TypeWithDeclaration, TypeWithDeclaration)
defineEnumModel shouldGenTypes schemaName schema enumValues = do
  ((typ, (_, dependencies)), (typ', (_, dependencies'))) <- defineModelForSchemaConcreteIgnoreEnum shouldGenTypes DontCreateTypeAlias (schemaName <> "EnumValue") schema
  (jsonImplementation, newType, name) <- defineEnumModelWrap typ
  (jsonImplementation', newType', name') <- defineEnumModelWrap typ'
  OAM.logInfo $ "Define as enum named '" <> T.pack (nameBase name) <> "'"
  pure ((varT name, (newType `appendDoc` jsonImplementation, dependencies)), (varT name', (newType' `appendDoc` jsonImplementation', dependencies')))
  where
    defineEnumModelWrap typ = do
      name <- haskellifyNameM True schemaName
      let getConstructor (a, _, _) = a
          getValueInfo value = do
            cname <- haskellifyNameM True (schemaName <> T.pack "Enum" <> aesonValueToName value)
            pure (normalC cname [], cname, value)
      constructorsInfo <- mapM getValueInfo enumValues
      fallbackName <- haskellifyNameM True $ schemaName <> "Other"
      typedName <- haskellifyNameM True $ schemaName <> "Typed"
      path <- getCurrentPathEscaped
      let nameValuePairs = fmap (\(_, a, b) -> (a, b)) constructorsInfo
          toBangType t = do
            ban <- bang noSourceUnpackedness noSourceStrictness
            banT <- t
            pure (ban, banT)
          fallbackC = normalC fallbackName [toBangType (varT ''Aeson.Value)]
          typedC = normalC typedName [toBangType typ]
          jsonImplementation = defineJsonImplementationForEnum name fallbackName typedName nameValuePairs
          comments = fmap (("Represents the JSON value @" <>) . (<> "@") . showAesonValue) enumValues
          newType =
            ( Doc.generateHaddockComment
                [ "Defines the enum schema located at @" <> path <> "@ in the specification.",
                  "",
                  getDescriptionOfSchema schema
                ]
                $$
            )
              . ( `Doc.sideBySide`
                    ( text ""
                        $$ Doc.sideComments
                          ( "This case is used if the value encountered during decoding does not match any of the provided cases in the specification."
                              : "This constructor can be used to send values to the server which are not present in the specification yet."
                              : comments
                          )
                    )
                )
              . Doc.reformatADT
              . ppr
              <$> dataD
                (pure [])
                name
                []
                Nothing
                (fallbackC : typedC : (getConstructor <$> constructorsInfo))
                objectDeriveClause
      pure (jsonImplementation, newType, name)

defineJsonImplementationForEnum :: Name -> Name -> Name -> [(Name, Aeson.Value)] -> Q Doc
defineJsonImplementationForEnum name fallbackName typedName nameValues =
  -- without this function, a N long string takes up N lines, as every
  -- new character starts on a new line
  let (e, p) = (\n -> (varE n, varP n)) $ mkName "val"
      fromJsonCases =
        multiIfE $
          fmap
            ( \(name', value) -> normalGE [|$e == $(liftAesonValue value)|] (varE name')
            )
            nameValues
            <> [normalGE [|otherwise|] [|$(varE fallbackName) $e|]]
      fromJsonFn =
        funD
          (mkName "parseJSON")
          [clause [p] (normalB [|pure $fromJsonCases|]) []]
      fromJson = instanceD (cxt []) [t|Aeson.FromJSON $(varT name)|] [fromJsonFn]
      toJsonFnClause n ps ex =
        funD
          (mkName "toJSON")
          [ clause
              [conP n ps]
              (normalB ex)
              []
          ]
      toJsonClause (name', value) = toJsonFnClause name' [] $ liftAesonValue $ Aeson.toJSON value
      toJsonFns =
        toJsonFnClause fallbackName [p] e
          : toJsonFnClause typedName [p] [|Aeson.toJSON $e|]
          : (toJsonClause <$> nameValues)
      toJson = instanceD (cxt []) [t|Aeson.ToJSON $(varT name)|] toJsonFns
   in fmap ppr toJson `appendDoc` fmap ppr fromJson

-- | defines anyOf types
--
-- If the subschemas consist only of objects an allOf type without any required field can be generated
-- If there are differen subschema types, per schematype a oneOf is generated
defineAnyOfSchema :: Bool -> TypeAliasStrategy -> Text -> Text -> [OAS.Schema] -> OAM.Generator (TypeWithDeclaration, TypeWithDeclaration)
defineAnyOfSchema shouldGenTypes strategy schemaName description schemas = do
  schemasWithDependencies <- mapMaybeM (resolveSchemaReference schemaName) schemas
  let concreteSchemas = fmap fst schemasWithDependencies
      schemasWithoutRequired = fmap (\o -> o {OAS.schemaObjectRequired = False}) concreteSchemas
      notObjectSchemas = filter (\o -> OAS.schemaObjectType o /= OAS.SchemaTypeObject) concreteSchemas
      newDependencies = Set.unions $ fmap snd schemasWithDependencies
  if null notObjectSchemas
    then do
      OAM.logTrace "anyOf does not contain any schemas which are not of type object and will therefore be defined as allOf"
      addDependencies newDependencies $ defineAllOfSchema shouldGenTypes schemaName description (fmap OAT.Concrete schemasWithoutRequired)
    else do
      OAM.logTrace "anyOf does contain at least one schema which is not of type object and will therefore be defined as oneOf"
      createAlias schemaName description strategy $ defineOneOfSchema shouldGenTypes schemaName description schemas

--    this would be the correct implementation
--    but it generates endless loop because some implementations use anyOf as a oneOf
--    where the schema reference itself
--      let objectSchemas = filter (\o -> OAS.schemaObjectType o == OAS.SchemaTypeObject) concreteSchemas
--      (propertiesCombined, _) <- fuseSchemasAllOf schemaName (fmap OAT.Concrete objectSchemas)
--      if null propertiesCombined then
--        createAlias schemaName strategy $ defineOneOfSchema schemaName schemas
--        else
--          let schemaPrototype = head objectSchemas
--              newSchema = schemaPrototype {OAS.schemaObjectProperties = propertiesCombined, OAS.schemaObjectRequired = Set.empty}
--          in
--            createAlias schemaName strategy $ defineOneOfSchema schemaName (fmap OAT.Concrete (newSchema : notObjectSchemas))

-- | defines a OneOf Schema
--
-- creates types for all the subschemas and then creates an adt with constructors for the different
-- subschemas. Constructors are numbered
defineOneOfSchema :: Bool -> Text -> Text -> [OAS.Schema] -> OAM.Generator (TypeWithDeclaration, TypeWithDeclaration)
defineOneOfSchema shouldGenTypes schemaName description schemas = do
  when (null schemas) $ OAM.logWarning "oneOf does not contain any sub-schemas and will therefore be defined as a void type"
  settings <- OAM.getSettings
  let name = haskellifyName (OAO.settingConvertToCamelCase settings) True $ schemaName <> "Variants"
  let fixedValueStrategy = OAO.settingFixedValueStrategy settings
      (schemas', schemasWithFixedValues) = extractSchemasWithFixedValues fixedValueStrategy schemas
  let defineIndexed schema index = defineModelForSchemaNamed shouldGenTypes (schemaName <> "OneOf" <> T.pack (show index)) schema
  let indexedSchemas = zip schemas' ([1 ..] :: [Integer])
  OAM.logInfo $ "Define as oneOf named '" <> T.pack (nameBase name) <> "'"
  (variants) <- mapM (uncurry defineIndexed) indexedSchemas
  let variantDefinitions = vcat <$> mapM (fst . snd . fst) variants
      dependencies = Set.unions $ fmap (snd . snd . fst) variants
      types = fmap (fst . fst) variants
  let variantDefinitions' = vcat <$> mapM (fst . snd . snd) variants
      dependencies' = Set.unions $ fmap (snd . snd . snd) variants
      types' = fmap (fst . snd) variants
  innerRes <- defineOneOfSchemaWrap variants schemasWithFixedValues settings name dependencies variantDefinitions types
  innerRes' <- defineOneOfSchemaWrap variants schemasWithFixedValues settings name dependencies' variantDefinitions' types'
  -- innerRes = (varT name, (variantDefinitions `appendDoc` dataDefinition `appendDoc` toJson `appendDoc` fromJson, dependencies))
  pure (innerRes, innerRes')
  where
    defineOneOfSchemaWrap variants schemasWithFixedValues settings name dependencies variantDefinitions types = do
      path <- getCurrentPathEscaped
      let indexedTypes = zip types ([1 ..] :: [Integer])
          haskellifyConstructor = haskellifyName (OAO.settingConvertToCamelCase settings) True
          getConstructorName (typ, n) = do
            t <- typ
            let suffix = if OAO.settingUseNumberedVariantConstructors settings then "Variant" <> T.pack (show n) else typeToSuffix t
            pure $ haskellifyConstructor $ schemaName <> suffix
          constructorNames = fmap getConstructorName indexedTypes
          createTypeConstruct (typ, n) = do
            t <- typ
            bang' <- bang noSourceUnpackedness noSourceStrictness
            haskellifiedName <- getConstructorName (typ, n)
            normalC haskellifiedName [pure (bang', t)]
          createConstructorNameForSchemaWithFixedValue =
            haskellifyConstructor
              . (schemaName <>)
              . aesonValueToName
          createConstructorForSchemaWithFixedValue =
            (`normalC` [])
              . createConstructorNameForSchemaWithFixedValue
          fixedValueComments = fmap (("Represents the JSON value @" <>) . (<> "@") . showAesonValue) schemasWithFixedValues
          emptyCtx = pure []
          patternName = mkName "a"
          p = varP patternName
          e = varE patternName
          fromJsonFn =
            let paramName = mkName "val"
                body = do
                  constructorNames' <- sequence constructorNames
                  let resultExpr =
                        foldr
                          ( \constructorName expr ->
                              [|($(varE constructorName) <$> Aeson.fromJSON $(varE paramName)) <|> $expr|]
                          )
                          [|Aeson.Error "No variant matched"|]
                          constructorNames'
                      parserExpr =
                        [|
                          case $resultExpr of
                            Aeson.Success $p -> pure $e
                            Aeson.Error $p -> fail $e
                          |]
                  case schemasWithFixedValues of
                    [] -> parserExpr
                    _ ->
                      multiIfE $
                        fmap
                          ( \value ->
                              let constructorName = createConstructorNameForSchemaWithFixedValue value
                               in normalGE [|$(varE paramName) == $(liftAesonValue value)|] [|pure $(varE constructorName)|]
                          )
                          schemasWithFixedValues
                          <> [normalGE [|otherwise|] parserExpr]
             in funD
                  (mkName "parseJSON")
                  [ clause
                      [varP paramName]
                      (normalB body)
                      []
                  ]
          toJsonFn =
            funD
              (mkName "toJSON")
              ( fmap
                  ( \constructorName -> do
                      n <- constructorName
                      clause
                        [conP n [p]]
                        (normalB [|Aeson.toJSON $e|])
                        []
                  )
                  constructorNames
                  <> fmap
                    ( \value ->
                        let constructorName = createConstructorNameForSchemaWithFixedValue value
                         in clause
                              [conP constructorName []]
                              (normalB $ liftAesonValue value)
                              []
                    )
                    schemasWithFixedValues
              )
          dataDefinition =
            ( Doc.generateHaddockComment
                [ "Defines the oneOf schema located at @" <> path <> "@ in the specification.",
                  "",
                  description
                ]
                $$
            )
              . (`Doc.sideBySide` (text "" $$ Doc.sideComments fixedValueComments))
              . Doc.reformatADT
              . ppr
              <$> dataD
                emptyCtx
                name
                []
                Nothing
                (fmap createConstructorForSchemaWithFixedValue schemasWithFixedValues <> fmap createTypeConstruct indexedTypes)
                [ derivClause
                    Nothing
                    [ conT ''Show,
                      conT ''Eq
                    ]
                ]
          toJson = ppr <$> instanceD emptyCtx [t|Aeson.ToJSON $(varT name)|] [toJsonFn]
          fromJson = ppr <$> instanceD emptyCtx [t|Aeson.FromJSON $(varT name)|] [fromJsonFn]
          innerRes = (varT name, (variantDefinitions `appendDoc` dataDefinition `appendDoc` toJson `appendDoc` fromJson, dependencies))
      pure innerRes

typeToSuffix :: Type -> Text
typeToSuffix (ConT name') = T.pack $ nameBase name'
typeToSuffix (VarT name') =
  let x = T.pack $ nameBase name'
   in if x == "[]" then "List" else x
typeToSuffix (AppT type1 type2) = typeToSuffix type1 <> typeToSuffix type2
typeToSuffix x = T.pack $ show x

-- | combines schemas so that it is usefull for a allOf fusion
fuseSchemasAllOf :: Text -> [OAS.Schema] -> OAM.Generator (Map.Map Text OAS.Schema, Bool)
fuseSchemasAllOf schemaName schemas = do
  schemasWithDependencies <- mapMaybeM (resolveSchemaReference schemaName) schemas
  let concreteSchemas = fmap fst schemasWithDependencies
  subSchemaInformation <- mapM (getPropertiesForAllOf schemaName) concreteSchemas
  let propertiesCombined = foldl (Map.unionWith const) Map.empty (fmap fst subSchemaInformation)
  let requiredCombined = not (null subSchemaInformation) && head (fmap snd subSchemaInformation)
  pure (propertiesCombined, requiredCombined)

-- | gets properties for an allOf merge
-- looks if subschemas define further subschemas
getPropertiesForAllOf :: Text -> OAS.SchemaObject -> OAM.Generator (Map.Map Text OAS.Schema, Bool)
getPropertiesForAllOf schemaName schema =
  let allOf = OAS.schemaObjectAllOf schema
      anyOf = OAS.schemaObjectAnyOf schema
      relevantSubschemas = allOf <> anyOf
   in if null relevantSubschemas
        then pure (OAS.schemaObjectProperties schema, OAS.schemaObjectRequired schema)
        else do
          (allOfProps, allOfRequired) <- fuseSchemasAllOf schemaName allOf
          (anyOfProps, _) <- fuseSchemasAllOf schemaName anyOf
          pure (Map.unionWith const allOfProps anyOfProps, allOfRequired)

-- | defines a allOf subschema
-- Fuses the subschemas together
defineAllOfSchema :: Bool -> Text -> Text -> [OAS.Schema] -> OAM.Generator (TypeWithDeclaration, TypeWithDeclaration)
defineAllOfSchema shouldGenTypes schemaName description schemas = do
  newDefs <- defineNewSchemaForAllOf schemaName description schemas
  case newDefs of
    Just (newSchema, newDependencies) ->
      addDependencies newDependencies $ defineModelForSchemaConcrete shouldGenTypes DontCreateTypeAlias schemaName newSchema
    Nothing -> pure (([t|Aeson.Object|], (emptyDoc, Set.empty)), ([t|Aeson.Object|], (emptyDoc, Set.empty)))

-- | defines a new Schema, which properties are fused
defineNewSchemaForAllOf :: Text -> Text -> [OAS.Schema] -> OAM.Generator (Maybe (OAS.SchemaObject, Dep.Models))
defineNewSchemaForAllOf schemaName description schemas = do
  schemasWithDependencies <- mapMaybeM (resolveSchemaReference schemaName) schemas
  let concreteSchemas = fmap fst schemasWithDependencies
      newDependencies = Set.unions $ fmap snd schemasWithDependencies
  (propertiesCombined, requiredCombined) <- fuseSchemasAllOf schemaName schemas
  if Map.null propertiesCombined
    then do
      OAM.logWarning "allOf does not contain any schemas with properties."
      pure Nothing
    else do
      let schemaPrototype = head concreteSchemas
          newSchema = schemaPrototype {OAS.schemaObjectProperties = propertiesCombined, OAS.schemaObjectRequired = requiredCombined, OAS.schemaObjectDescription = Just description}
      OAM.logTrace $ "Define allOf as record named '" <> schemaName <> "'"
      pure $ Just (newSchema, newDependencies)

-- | defines an array
defineArrayModelForSchema :: Bool -> TypeAliasStrategy -> Text -> OAS.SchemaObject -> OAM.Generator (TypeWithDeclaration, TypeWithDeclaration)
defineArrayModelForSchema shouldGenTypes strategy schemaName schema = do
  arrayItemTypeSuffix <- case strategy of
    CreateTypeAlias -> OAM.getSetting OAO.settingArrayItemTypeSuffix
    DontCreateTypeAlias -> pure "" -- The suffix is only relevant for top level declarations because only there a named type of the array even exists
  ((type', (content, dependencies)), (type'', (content', dependencies'))) <-
    case OAS.schemaObjectItems schema of
      Just itemSchema -> OAM.nested "items" $ defineModelForSchemaNamed shouldGenTypes (schemaName <> arrayItemTypeSuffix) itemSchema
      -- not allowed by the spec
      Nothing -> do
        OAM.logWarning "Array type was defined without a items schema and therefore cannot be defined correctly"
        pure (([t|Aeson.Object|], (emptyDoc, Set.empty)), ([t|Aeson.Object|], (emptyDoc, Set.empty)))
  let arrayType = appT listT type'
  let arrayType' = appT listT type''
  schemaName' <- haskellifyNameM True schemaName
  OAM.logTrace $ "Define as list named '" <> T.pack (nameBase schemaName') <> "'"
  path <- getCurrentPathEscaped
  pure
    ( ( arrayType,
        ( content `appendDoc` case strategy of
            CreateTypeAlias ->
              ( Doc.generateHaddockComment
                  [ "Defines an alias for the schema located at @" <> path <> "@ in the specification.",
                    "",
                    getDescriptionOfSchema schema
                  ]
                  $$
              )
                . ppr
                <$> tySynD schemaName' [] arrayType
            DontCreateTypeAlias -> emptyDoc,
          dependencies
        )
      ),
      ( arrayType',
        ( content' `appendDoc` case strategy of
            CreateTypeAlias ->
              ( Doc.generateHaddockComment
                  [ "Defines an alias for the schema located at @" <> path <> "@ in the specification.",
                    "",
                    getDescriptionOfSchema schema
                  ]
                  $$
              )
                . ppr
                <$> tySynD schemaName' [] arrayType'
            DontCreateTypeAlias -> emptyDoc,
          dependencies'
        )
      )
    )

-- | Defines a record
defineObjectModelForSchema :: Bool -> TypeAliasStrategy -> Text -> OAS.SchemaObject -> OAM.Generator (TypeWithDeclaration, TypeWithDeclaration)
defineObjectModelForSchema shouldGenTypes strategy schemaName schema =
  if OAS.isSchemaEmpty schema
    then createAlias schemaName (getDescriptionOfSchema schema) strategy $ pure (([t|Aeson.Object|], (emptyDoc, Set.empty)), ([t|Aeson.Object|], (emptyDoc, Set.empty)))
    else do
      settings <- OAM.getSettings
      path <- getCurrentPathEscaped
      let convertToCamelCase = OAO.settingConvertToCamelCase settings
          name = haskellifyName convertToCamelCase True schemaName
          required = OAS.schemaObjectRequired schema
          fixedValueStrategy = OAO.settingFixedValueStrategy settings
          (props, propsWithFixedValues) = extractPropertiesWithFixedValues fixedValueStrategy $ Map.toList $ OAS.schemaObjectProperties schema
          propsWithNames = zip (fmap fst props) $ fmap (haskellifyName convertToCamelCase False . ("" <>) . uppercaseFirstText . (\(x, y, z) -> x) . fst) props
          emptyCtx = pure []
      OAM.logInfo $ "Define as record named '" <> T.pack (nameBase name) <> "'"
      (bangTypes, propertyContent, propertyDependencies) <- propertiesToBangTypes shouldGenTypes "" props
      propertyDescriptions <- getDescriptionOfProperties props
      let dataDefinition = do
            bangs <- bangTypes
            let record = recC name (pure <$> bangs)
            flip Doc.zipCodeAndComments propertyDescriptions
              . T.lines
              . T.pack
              . show
              . Doc.reformatRecord
              . ppr
              <$> dataD emptyCtx name [] Nothing [record] objectDeriveClause
          toJsonInstance = createToJSONImplementation name propsWithNames propsWithFixedValues
          fromJsonInstance = createFromJSONImplementation name propsWithNames
          mkFunction = createMkFunction name propsWithNames bangTypes
      pure
        ( if shouldGenTypes
            then
              ( varT name,
                ( pure
                    ( Doc.generateHaddockComment
                        [ "Defines the object schema located at @" <> path <> "@ in the specification.",
                          "",
                          getDescriptionOfSchema schema
                        ]
                    )
                    `appendDoc` dataDefinition
                    `appendDoc` toJsonInstance
                    `appendDoc` fromJsonInstance
                    `appendDoc` propertyContent,
                  propertyDependencies
                )
              )
            else
              ( varT name,
                ( pure
                    ( Doc.generateHaddockComment
                        [ "Defines the object schema located at @" <> path <> "@ in the specification.",
                          "",
                          getDescriptionOfSchema schema
                        ]
                    )
                    `appendDoc` mkFunction
                    `appendDoc` propertyContent,
                  propertyDependencies
                )
              ),
          ( varT name,
            ( pure
                ( Doc.generateHaddockComment
                    [ "Defines the object schema located at @" <> path <> "@ in the specification.",
                      "",
                      getDescriptionOfSchema schema
                    ]
                )
                `appendDoc` mkFunction,
              propertyDependencies
            )
          )
        )

extractPropertiesWithFixedValues :: FixedValueStrategy -> [(Text, OAS.Schema)] -> ([((Text, Bool, Maybe Aeson.Value), OAS.Schema)], [((Text, Bool), Aeson.Value)])
extractPropertiesWithFixedValues fixedValueStrategy =
  E.partitionEithers
    . fmap
      ( \(name, schema) ->
          ( BF.bimap ((name, extractSchemaWithRequired schema, extractSchemaWithStaticValue schema),) ((name, extractSchemaWithRequired schema),) $
              if extractSchemaWithRequired schema
                then extractSchemaWithFixedValue fixedValueStrategy schema
                else Left schema
          )
      )

extractSchemasWithFixedValues :: FixedValueStrategy -> [OAS.Schema] -> ([OAS.Schema], [Aeson.Value])
extractSchemasWithFixedValues fixedValueStrategy =
  E.partitionEithers . fmap (extractSchemaWithFixedValue fixedValueStrategy)

extractSchemaWithRequired :: OAS.Schema -> Bool
extractSchemaWithRequired schema@(OAT.Concrete OAS.SchemaObject {..}) = schemaObjectRequired
extractSchemaWithRequired _ = False

extractSchemaWithStaticValue :: OAS.Schema -> Maybe Aeson.Value
extractSchemaWithStaticValue schema@(OAT.Concrete OAS.SchemaObject {..}) = schemaObjectValue
extractSchemaWithStaticValue _ = Nothing

extractSchemaWithFixedValue :: FixedValueStrategy -> OAS.Schema -> Either OAS.Schema Aeson.Value
extractSchemaWithFixedValue FixedValueStrategyExclude schema@(OAT.Concrete OAS.SchemaObject {..}) =
  case schemaObjectEnum of
    [value] -> Right value
    _ -> Left schema
extractSchemaWithFixedValue _ schema = Left schema

createMkFunction :: Name -> [((Text, Bool, Maybe Aeson.Value), Name)] -> Q [VarBangType] -> Q Doc
createMkFunction name propsWithNames bangTypes = do
  bangs <- bangTypes
  let fnName = mkName $ "mk" <> nameBase name
      propsWithTypes =
        ( \(((originalName, required, maybeVal), propertyName), (_, _, propertyType)) ->
            (propertyName, propertyType, required, maybeVal)
        )
          <$> zip propsWithNames bangs
      requiredPropsWithTypes = filter (\(_, _, isRequired, maybeVal) -> Maybe.isNothing maybeVal) propsWithTypes
      parameterPatterns = (\(propertyName, _, _, _) -> varP propertyName) <$> requiredPropsWithTypes
      parameterDescriptions = (\(propertyName, _, _, _) -> "'" <> T.pack (nameBase propertyName) <> "'") <$> requiredPropsWithTypes
      recordExpr =
        ( \(propertyName, propertyType, isRequired, maybeVal) ->
            fieldExp
              propertyName
              ( case maybeVal of
                  Just val ->
                    let orgVal = case val of
                          Aeson.Number num -> if show propertyType == "VarT GHC.Types.Double" then pure $ LitE $ IntegerL (Scientific.coefficient num) else [|val|]
                          Aeson.String str -> if show propertyType == "VarT Data.Text.Internal.Text" then pure $ LitE $ StringL (T.unpack str) else [|val|]
                          Aeson.Bool bool -> pure $ ConE $ mkName $ show bool
                          -- Aeson.Object obj -> [|obj|]
                          _ -> [|val|]
                     in if isRequired then orgVal else AppE (VarE $ mkName "Just") <$> orgVal
                  Nothing -> varE propertyName
              )
        )
          <$> propsWithTypes
      expr = recConE name recordExpr
      fnType = foldr (\(_, propertyType, _, maybeVal) t -> [t|$(pure propertyType) -> $t|]) (conT name) requiredPropsWithTypes
  pure
    ( Doc.generateHaddockComment
        [ "Create a new '" <> T.pack (nameBase name) <> "' with all required fields."
        ]
    )
    `appendDoc` fmap
      ( ( `Doc.sideBySide`
            Doc.sideComments parameterDescriptions
        )
          . Doc.breakOnTokens ["->"]
          . ppr
      )
      (sigD fnName fnType)
    `appendDoc` fmap ppr (funD fnName [clause parameterPatterns (normalB expr) []])

-- | create toJSON implementation for an object
createToJSONImplementation :: Name -> [((Text, Bool, Maybe Aeson.Value), Name)] -> [((Text, Bool), Aeson.Value)] -> Q Doc
createToJSONImplementation objectName recordNames propsWithFixedValues =
  let emptyDefs = pure []
      fnArgName = mkName "obj"
      toAssertion ((jsonName, required, _), hsName) =
        if required
          then [|[$(stringE $ T.unpack jsonName) Aeson..= $(varE hsName) $(varE fnArgName)]|]
          else [|(maybe mempty (pure . ($(stringE $ T.unpack jsonName) Aeson..=)) ($(varE hsName) $(varE fnArgName)))|]
      toFixedAssertion ((jsonName, _), value) =
        [|[$(stringE $ T.unpack jsonName) Aeson..= $(liftAesonValueWithOverloadedStrings False value)]|]
      assertions = fmap toAssertion recordNames <> fmap toFixedAssertion propsWithFixedValues
      assertionsList = [|(List.concat $(toExprList assertions))|]
      toExprList = foldr (\x expr -> uInfixE x (varE $ mkName ":") expr) [|mempty|]
      defaultJsonImplementation =
        [ funD
            (mkName "toJSON")
            [ clause
                [varP fnArgName]
                ( normalB
                    [|Aeson.object $assertionsList|]
                )
                []
            ],
          funD
            (mkName "toEncoding")
            [ clause
                [varP fnArgName]
                ( normalB
                    [|Aeson.pairs (mconcat $assertionsList)|]
                )
                []
            ]
        ]
   in ppr <$> instanceD emptyDefs [t|Aeson.ToJSON $(varT objectName)|] defaultJsonImplementation

-- | create FromJSON implementation for an object
createFromJSONImplementation :: Name -> [((Text, Bool, Maybe Aeson.Value), Name)] -> Q Doc
createFromJSONImplementation objectName recordNames =
  let fnArgName = mkName "obj"
      withObjectLamda =
        foldl
          ( \prev ((propName, required, _), _) ->
              let propName' = stringE $ T.unpack propName
                  arg = varE fnArgName
                  readPropE =
                    if required
                      then [|$arg Aeson..: $propName'|]
                      else [|$arg Aeson..:! $propName'|]
               in [|$prev <*> $readPropE|]
          )
          [|pure $(varE objectName)|]
          recordNames
   in ppr
        <$> instanceD
          (cxt [])
          [t|Aeson.FromJSON $(varT objectName)|]
          [ funD
              (mkName "parseJSON")
              [ clause
                  []
                  ( normalB
                      [|Aeson.withObject $(stringE $ show objectName) $(lam1E (varP fnArgName) withObjectLamda)|]
                  )
                  []
              ]
          ]

-- | create "bangs" record fields for properties
propertiesToBangTypes :: Bool -> Text -> [((Text, Bool, Maybe Aeson.Value), OAS.Schema)] -> OAM.Generator BangTypesSelfDefined
propertiesToBangTypes _ _ [] = pure (pure [], emptyDoc, Set.empty)
propertiesToBangTypes shouldGenTypes schemaName props = OAM.nested "properties" $ do
  propertySuffix <- OAM.getSetting OAO.settingPropertyTypeSuffix
  convertToCamelCase <- OAM.getSetting OAO.settingConvertToCamelCase
  let createBang :: Text -> Text -> Bool -> Q Type -> Q VarBangType
      createBang recordName propName required myType = do
        bang' <- bang noSourceUnpackedness noSourceStrictness
        type' <-
          if required
            then myType
            else appT (varT ''Maybe) myType
        pure (haskellifyName convertToCamelCase False propName, bang', type')
      propToBangType :: ((Text, Bool, Maybe Aeson.Value), OAS.Schema) -> OAM.Generator (Q VarBangType, Q Doc, Dep.Models)
      propToBangType ((recordName, required, _), schema) = do
        let propName = schemaName <> uppercaseFirstText recordName
        ((myType, (content, depenencies)), _) <- OAM.nested recordName $ defineModelForSchemaNamed shouldGenTypes (propName <> propertySuffix) schema
        let myBang = createBang recordName propName required myType
        pure (myBang, content, depenencies)
      foldFn :: OAM.Generator BangTypesSelfDefined -> ((Text, Bool, Maybe Aeson.Value), OAS.Schema) -> OAM.Generator BangTypesSelfDefined
      foldFn accHolder next = do
        (varBang, content, dependencies) <- accHolder
        (nextVarBang, nextContent, nextDependencies) <- propToBangType next
        pure
          ( varBang `liftedAppend` fmap pure nextVarBang,
            content `appendDoc` nextContent,
            Set.union dependencies nextDependencies
          )
  foldl foldFn (pure (pure [], emptyDoc, Set.empty)) props

getDescriptionOfSchema :: OAS.SchemaObject -> Text
getDescriptionOfSchema schema = Doc.escapeText $ Maybe.fromMaybe "" $ OAS.schemaObjectDescription schema

getDescriptionOfProperties :: [((Text, Bool, Maybe Aeson.Value), OAS.Schema)] -> OAM.Generator [Text]
getDescriptionOfProperties =
  mapM
    ( \((name, _, _), schema) -> do
        schema' <- resolveSchemaReferenceWithoutWarning schema
        let description = maybe "" (": " <>) $ schema' >>= OAS.schemaObjectDescription
            constraints = T.unlines $ ("* " <>) <$> getConstraintDescriptionsOfSchema schema'
        pure $ Doc.escapeText $ name <> description <> (if T.null constraints then "" else "\n\nConstraints:\n\n" <> constraints)
    )

-- | Extracts the constraints of a 'OAS.SchemaObject' as human readable text
getConstraintDescriptionsOfSchema :: Maybe OAS.SchemaObject -> [Text]
getConstraintDescriptionsOfSchema schema =
  let showConstraint desc = showConstraintSurrounding desc ""
      showConstraintSurrounding prev after = fmap $ (prev <>) . (<> after) . T.pack . show
      exclusiveMaximum = maybe False OAS.schemaObjectExclusiveMaximum schema
      exclusiveMinimum = maybe False OAS.schemaObjectExclusiveMinimum schema
   in Maybe.catMaybes
        [ showConstraint "Must be a multiple of " $ schema >>= OAS.schemaObjectMultipleOf,
          showConstraint ("Maxium " <> if exclusiveMaximum then " (exclusive)" else "" <> " of ") $ schema >>= OAS.schemaObjectMaximum,
          showConstraint ("Minimum " <> if exclusiveMinimum then " (exclusive)" else "" <> " of ") $ schema >>= OAS.schemaObjectMinimum,
          showConstraint "Maximum length of " $ schema >>= OAS.schemaObjectMaxLength,
          showConstraint "Minimum length of " $ schema >>= OAS.schemaObjectMinLength,
          ("Must match pattern '" <>) . (<> "'") <$> (schema >>= OAS.schemaObjectPattern),
          showConstraintSurrounding "Must have a maximum of " " items" $ schema >>= OAS.schemaObjectMaxItems,
          showConstraintSurrounding "Must have a minimum of " " items" $ schema >>= OAS.schemaObjectMinItems,
          schema
            >>= ( \case
                    True -> Just "Must have unique items"
                    False -> Nothing
                )
              . OAS.schemaObjectUniqueItems,
          showConstraintSurrounding "Must have a maximum of " " properties" $ schema >>= OAS.schemaObjectMaxProperties,
          showConstraintSurrounding "Must have a minimum of " " properties" $ schema >>= OAS.schemaObjectMinProperties
        ]

-- | Extracts the 'Name' of a 'OAS.SchemaObject' which should be used for primitive types
getSchemaType :: OAO.Settings -> OAS.SchemaObject -> Name
getSchemaType OAO.Settings {settingUseIntWithArbitraryPrecision = True} OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeInteger} = ''Integer
getSchemaType _ OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeInteger, schemaObjectFormat = Just "int32"} = ''Int.Int32
getSchemaType _ OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeInteger, schemaObjectFormat = Just "int64"} = ''Int.Int64
getSchemaType _ OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeInteger} = ''Int
getSchemaType OAO.Settings {settingUseFloatWithArbitraryPrecision = True} OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeNumber} = ''Scientific.Scientific
getSchemaType _ OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeNumber, schemaObjectFormat = Just "float"} = ''Float
getSchemaType _ OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeNumber, schemaObjectFormat = Just "double"} = ''Double
getSchemaType _ OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeNumber} = ''Double
getSchemaType _ OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeString, schemaObjectFormat = Just "byte"} = ''OC.JsonByteString
getSchemaType _ OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeString, schemaObjectFormat = Just "binary"} = ''OC.JsonByteString
getSchemaType OAO.Settings {settingUseDateTypesAsString = True} OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeString, schemaObjectFormat = Just "date"} = ''Day
getSchemaType OAO.Settings {settingUseDateTypesAsString = True} OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeString, schemaObjectFormat = Just "date-time"} = ''OC.JsonDateTime
getSchemaType _ OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeString} = ''Text
getSchemaType _ OAS.SchemaObject {schemaObjectType = OAS.SchemaTypeBool} = ''Bool
getSchemaType _ OAS.SchemaObject {} = ''Text

getCurrentPathEscaped :: OAM.Generator Text
getCurrentPathEscaped = Doc.escapeText . T.intercalate "." <$> OAM.getCurrentPath

-- tempFunc :: Q ()
-- tempFunc = runQ [| let (i :: Maybe Int) = Aeson.decode "2" |]

parseJsonTH :: Aeson.Value -> Type -> Q Exp
parseJsonTH val qt = do
  [|Aeson.decode (encodeUtf8 str) :: Maybe qt|]
