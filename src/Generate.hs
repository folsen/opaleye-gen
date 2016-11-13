{-# LANGUAGE OverloadedStrings #-}
module Generate where

import           Prelude        hiding (unwords)

import qualified Cases          as C
import           Data.Monoid    ((<>), mconcat)
import           Data.Text      hiding (map)
import           Database
import           Text.Countable

data TableDefinition = TableDefinition
  { tableName :: Text
  , columns   :: [InformationSchemaColumn]
  }

fileHeaderText :: Text
fileHeaderText = intercalate "\n"
  [ "{-# LANGUAGE TemplateHaskell #-}"
  , "{-# LANGUAGE MultiParamTypeClasses #-}"
  , "{-# LANGUAGE FlexibleInstances #-}"
  , "\nmodule Database where\n"
  , "import qualified Data.Aeson                 as JSON"
  , "import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)"
  , "import           Data.Scientific"
  , "import           Data.Text"
  , "import           Data.Time"
  , "import           Data.UUID"
  , "import           GHC.Int"
  , "import           Opaleye"
  ]

fullTableText :: TableDefinition -> Text
fullTableText td = mconcat
  [ "---- Types for table: " <> tableName td <> " ----\n\n"
  , genAbstractType td, "\n\n"
  , genConcreteType td, "\n\n"
  , genPgTypes Read td, "\n\n"
  , genPgTypes Write td, "\n\n"
  , genPgTypes Nullable td, "\n\n"
  , genMaybeType td, "\n\n"
  , genSequence td, "\n\n"
  , "$(makeAdaptorAndInstance \"p", typeName td, "\" ''", typeName td,"')\n\n"
  , tableDefinition td, "\n"]


genAbstractType :: TableDefinition -> Text
genAbstractType t = mconcat [ "data ", tname, "' ", args," =\n  ", tname, "\n    { "] <> columnRows <> "\n    }"
  where
    tname = typeName t
    args = unwords $ map polyArg (columns t)
    polyArg c = "c" <> pack (show $ ordinal_position c)
    columnRows = intercalate "\n    , " columnDescs
    columnDescs = Prelude.map columnDesc (columns t)
    columnDesc c = fieldName c <> " :: " <> polyArg c

genConcreteType :: TableDefinition -> Text
genConcreteType t = mconcat ["type ", typeName t, " = ", typeName t, "' "] <> concreteTypes
  where
    concreteTypes = unwords $ map parenthesize (columns t)
    parenthesize c = if is_nullable c == "YES"
                     then "(" <> typeNameToHType c <> ")"
                     else typeNameToHType c

data TableDefType = Read | Write | Nullable deriving (Show, Eq)

genPgTypes :: TableDefType -> TableDefinition -> Text
genPgTypes Nullable t = mconcat ["type ", typeName t, "NullableColumns = ", typeName t, "' "] <> pgTypes
  where
    pgTypes = unwords $ map alwaysNullable (columns t)
    alwaysNullable c = "(Column " <> pgTypeForColumn c {is_nullable = "YES"} <> ")"
genPgTypes dt t = mconcat ["type ", typeName t, pack (show dt), "Columns = ", typeName t, "' "] <> pgTypes
  where
    pgTypes = unwords $ map specializeForId (columns t)
    specializeForId c = if (column_name c == "id" || is_nullable c == "YES") && dt == Write
                        then "(Maybe (Column " <> pgTypeForColumn c <> "))"
                        else "(Column " <> pgTypeForColumn c <> ")"

genMaybeType :: TableDefinition -> Text
genMaybeType t = mconcat ["type ", typeName t, "Maybe = ", typeName t, "' "] <> maybeTypes
  where
    maybeTypes = unwords $ map parenthesize (columns t)
    parenthesize c = "(" <> typeNameToHTypeMaybe ApplyToMaybe c <> ")"

genSequence :: TableDefinition -> Text
genSequence t = sequenceSig <> "\n" <> sequenceDef
  where
    sequenceSig = mconcat ["sequence", typeName t, " :: ", typeName t, "Maybe -> Maybe ", typeName t]
    sequenceDef = "sequence" <> typeName t <> " (" <> typeName t <> " " <> pats <> ") = pure " <> typeName t <> args
    pats = unwords (fst <$> namesWithColumns)
    namesWithColumns = Prelude.zip (("c" <>) . pack . show <$> [1::Int ..]) (columns t)
    args = mconcat $ (" <*> " <>) . makeArg <$> namesWithColumns
    makeArg (n, c) = if is_nullable c == "YES"
                       then "pure " <> n
                       else n

tableDefinition :: TableDefinition -> Text
tableDefinition t = mconcat [ typeName'
                            , "Table :: Table "
                            , typeName t, "WriteColumns "
                            , typeName t, "ReadColumns\n"
                            , typeName', "Table = Table \"", tableName t, "\""
                            , " (p", typeName t, "\n  ", typeName t, "\n"
                            , "    { ", fieldDefinitions, "\n"
                            , "    }\n  )"]
  where
    typeName' = singularize . C.camelize $ tableName t
    fieldDefinitions = intercalate "\n    , " $ map fieldDefinition (columns t)
    fieldDefinition c = if column_name c == "id" || is_nullable c == "YES"
                        then fieldName c <> " = optional \"" <> column_name c <> "\""
                        else fieldName c <> " = required \"" <> column_name c <> "\""

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
    other         -> error $ "Unimplemented PostgresQL type conversion for " <> show other
  where
    mval = case applyToMaybe of
             ApplyToMaybe      -> "Maybe "
             DoNotApplyToMaybe -> ""

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
    other         -> error $ "Unimplemented PostgresQL type conversion for " <> show other
  where
    nullify v = if is_nullable col == "YES"
                then "(Nullable " <> v <> ")"
                else v

--- Formatting functions

camelize :: Text -> Text
camelize = C.process C.title C.camel

typeName :: TableDefinition -> Text
typeName = singularize . camelize . tableName

fieldName :: InformationSchemaColumn -> Text
fieldName c = haskelly (table_name c) <> camelize (column_name c)
  where haskelly = singularize . C.camelize
