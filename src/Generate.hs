{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Generate where

import           Prelude         hiding (unwords)

import qualified Cases           as C
import           Data.Monoid     ((<>))
import           Data.Text       hiding (map)
import           Database
import           Text.Countable
import           Util            (q, qc)
import           System.FilePath (dropExtension)

data TableDefinition = TableDefinition
  { tableName :: Text
  , columns   :: [InformationSchemaColumn]
  }

fileHeaderText :: String -> Text
fileHeaderText filePath = [qc|
  \{-# LANGUAGE TemplateHaskell #-}
  \{-# LANGUAGE MultiParamTypeClasses #-}
  \{-# LANGUAGE FlexibleContexts #-}
  \{-# LANGUAGE FlexibleInstances #-}

  module {dropExtension filePath} where

  import qualified Data.Aeson                 as JSON
  import           Data.Profunctor
  import           Data.Profunctor.Product
  import           Data.Profunctor.Product.Default
  import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
  import           Data.Scientific
  import           Data.Text
  import           Data.Time
  import           Data.UUID
  import           GHC.Int
  import           Opaleye hiding (fromNullable, Query)

  -- | A newtype around @a -> Maybe b@ to facilitate conversions from the
  -- Nullable types.
  newtype ToMaybe a b = ToMaybe \{ unToMaybe :: a -> Maybe b }

  instance Profunctor ToMaybe where
    dimap f g (ToMaybe h) = ToMaybe (fmap g . h . f)

  instance ProductProfunctor ToMaybe where
    empty = ToMaybe pure
    (ToMaybe f) ***! (ToMaybe g) = ToMaybe (\(x, y) -> (,) <$> f x <*> g y)

  -- | This instance makes sure that values which are required in the output are
  -- required in the input.
  instance Default ToMaybe (Maybe a) a where
    def = ToMaybe id

  -- | This instance allows values which are optional in the output to be
  -- optional in the input.
  instance Default ToMaybe (Maybe a) (Maybe a) where
    def = ToMaybe pure

  -- | Convert from any Nullable type by "sequencing" over all the fields.
  fromNullable :: Default ToMaybe a b => a -> Maybe b
  fromNullable = unToMaybe def
  |]

fullTableText :: String -> TableDefinition -> Text
fullTableText schema td = [qc|
  ---- Types for table: {schema}.{tableName td} ----

  -- {schema}.{tableName td} Abstract Type --
  {genAbstractType td}

  -- {schema}.{tableName td} Concrete Type --
  {genConcreteType td}

  -- {schema}.{tableName td} Pg Types Read  --
  {genPgTypes Read td}

  -- {schema}.{tableName td} Pg Types Write --
  {genPgTypes Write td}

  -- {schema}.{tableName td} Pg Types Nullable --
  {genPgTypes Nullable td}

  -- {schema}.{tableName td} Nullable Type --
  {genNullableType td}

  -- {schema}.{tableName td} Sequence --
  {genSequence td}

  $(makeAdaptorAndInstance "p{typeName td}" ''{typeName td}')

  -- {schema}.{tableName td} Table Definition --
  {tableDefinition schema td}
  |]

genAbstractType :: TableDefinition -> Text
genAbstractType t = [qc|
  data {tname}' {args} =
    {tname}
      \{ {columnRows}
      }|]
  where
    tname = typeName t
    args = unwords $ map polyArg (columns t)
    polyArg :: InformationSchemaColumn -> Text
    polyArg c = [qc|c{show $ ordinal_position c}|]
    columnRows = intercalate "\n    , " columnDescs
    columnDescs = Prelude.map columnDesc (columns t)
    columnDesc c = [qc|{fieldName c} :: {polyArg c}|]

genConcreteType :: TableDefinition -> Text
genConcreteType t = [qc|type {typeName t} = {typeName t}' {concreteTypes}|]
  where
    concreteTypes = unwords $ map parenthesize (columns t)
    parenthesize c = if is_nullable c == "YES"
                     then [qc|({typeNameToHType c})|]
                     else typeNameToHType c

data TableDefType = Read | Write | Nullable deriving (Show, Eq)

genPgTypes :: TableDefType -> TableDefinition -> Text
genPgTypes Nullable t =
  [qc|type {typeName t}NullableColumns = {typeName t}' {pgTypes}|]
  where
    pgTypes = unwords $ map alwaysNullable (columns t)
    alwaysNullable c = [qc|(Column {pgTypeForNullableColumn c})|]
genPgTypes dt t = [qc|type {typeName t}{show dt}Columns = {typeName t}' {pgTypes}|]
  where
    pgTypes = unwords $ map specializeForId (columns t)
    specializeForId c = if (column_name c == "id" || is_nullable c == "YES") && dt == Write
                        then [qc|(Maybe (Column {pgTypeForColumn c}))|]
                        else [qc|(Column {pgTypeForColumn c})|]

genNullableType :: TableDefinition -> Text
genNullableType t = [qc|type {typeName t}Nullable = {typeName t}' {maybeTypes}|]
  where
    maybeTypes = unwords $ map parenthesize (columns t)
    parenthesize c = [qc|({typeNameToHTypeMaybe ApplyToMaybe c})|]

genSequence :: TableDefinition -> Text
genSequence t = [qc|
  {fromNullableSig}
  {fromNullableDef}|]
  where
    fromNullableSig :: Text
    fromNullableSig =
      [qc|fromNullable{typeName t} :: {typeName t}Nullable -> Maybe {typeName t}|]
    fromNullableDef :: Text
    fromNullableDef = [qc|fromNullable{typeName t} = fromNullable|]

tableDefinition :: String -> TableDefinition -> Text
tableDefinition schema t = [qc|
  {typeName'}Table :: Table {typeName t}WriteColumns {typeName t}ReadColumns
  {typeName'}Table = tableWithSchema "{schema}" "{tableName t}" (p{typeName t}
    {typeName t}
      \{ {fieldDefinitions}
      }
    )|]
  where
    typeName' = singularize . camelize' $ tableName t
    fieldDefinitions = intercalate "\n    , " $ map fieldDefinition (columns t)
    fieldDefinition c = if column_name c == "id" || is_nullable c == "YES"
                        then [qc|{fieldName c} = optionalTableField "{column_name c}"|]
                        else [qc|{fieldName c} = requiredTableField "{column_name c}"|]

typeNameToHType :: InformationSchemaColumn -> Text
typeNameToHType col = typeNameToHTypeMaybe apply col
  where
    apply = if is_nullable col == "YES"
              then ApplyToMaybe
              else DoNotApplyToMaybe

data ApplyToMaybe = ApplyToMaybe | DoNotApplyToMaybe

-- Some dirty mappings, these don't account for Array correctly, there should
-- be another check for that on the another column, the udt_name will be _type for
-- an array of `type`
typeNameToHTypeMaybe :: ApplyToMaybe
                     -> InformationSchemaColumn
                     -> Text
typeNameToHTypeMaybe applyToMaybe col =
  case udt_name col of
    "bool"        -> mval <> "Bool"
    "int2"        -> mval <> "Int16"
    "int4"        -> mval <> "Int32"
    "int8"        -> mval <> "Int64"
    "float4"      -> mval <> "Float"
    "float8"      -> mval <> "Double"
    "numeric"     -> mval <> "Scientific"
    "char"        -> mval <> "Char"
    "text"        -> mval <> "Text"
    "bytea"       -> mval <> "ByteString"
    "date"        -> mval <> "Day"
    "timestamp"   -> mval <> "LocalTime"
    "timestamptz" -> mval <> "UTCTime"
    "time"        -> mval <> "TimeOfDay"
    "timetz"      -> mval <> "TimeOfDay"
    "interval"    -> mval <> "DiffTime"
    "uuid"        -> mval <> "UUID"
    "json"        -> mval <> "JSON.Value"
    "jsonb"       -> mval <> "JSON.Value"
    "varchar"     -> mval <> "Text"
    "_varchar"    -> "[Text]"
    "_int4"       -> "[Int32]"
    "oid"         -> mval <> "Int64"
    "inet"        -> mval <> "Text"
    other         -> mval <> "FIXME_" <> other
  where
    mval = case applyToMaybe of
             ApplyToMaybe      -> "Maybe "
             DoNotApplyToMaybe -> ""

pgTypeForNullableColumn :: InformationSchemaColumn -> Text
pgTypeForNullableColumn c = pgTypeForColumn c{is_nullable = "YES"}

pgTypeForColumn :: InformationSchemaColumn -> Text
pgTypeForColumn col =
  case udt_name col of
    "bool"        -> nullify "PGBool"
    "int2"        -> nullify "PGInt2"
    "int4"        -> nullify "PGInt4"
    "int8"        -> nullify "PGInt8"
    "float4"      -> nullify "PGFloat4"
    "float8"      -> nullify "PGFloat8"
    "numeric"     -> nullify "PGNumeric"
    "char"        -> nullify "PGText"
    "text"        -> nullify "PGText"
    "bytea"       -> nullify "PGBytea"
    "date"        -> nullify "PGDate"
    "timestamp"   -> nullify "PGTimestamp"
    "timestamptz" -> nullify "PGTimestamptz"
    "time"        -> nullify "PGTime"
    "timetz"      -> nullify "PGTime"
    "interval"    -> nullify "PGInt8"
    "uuid"        -> nullify "PGUuid"
    "json"        -> nullify "PGJson"
    "jsonb"       -> nullify "PGJsonb"
    "varchar"     -> nullify "PGText"
    "_varchar"    -> nullify "(PGArray Text)"
    "_int4"       -> nullify "(PGArray Int4)"
    "oid"         -> nullify "PGInt8"
    "inet"        -> nullify "PGText"
    other         -> nullify $ "FIXME_" <> other
  where
    nullify v = if is_nullable col == "YES"
                then "(Nullable " <> v <> ")"
                else v

--- Formatting functions

camelize :: Text -> Text
camelize = C.process C.title C.camel

camelize' :: Text -> Text
camelize' = C.process C.lower C.camel

typeName :: TableDefinition -> Text
typeName = singularize . camelize . tableName

fieldName :: InformationSchemaColumn -> Text
fieldName c = haskelly (table_name c) <> camelize (column_name c)
  where haskelly = singularize . camelize' 
