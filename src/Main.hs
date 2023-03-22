{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.Arrow              (returnA)
import           Control.Monad
import qualified Data.ByteString.Char8      as Char8
import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.Monoid                ((<>))
import           Data.Ord                   (comparing)
import           Data.Text                  hiding (map)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye
import           Options.Applicative        hiding (columns)
import           System.IO

import           Database
import           Generate


data Args = Args
  { dbString   :: String
  , outputPath :: String 
  , schema     :: String }

argsParser :: Parser Args
argsParser = Args
     <$> strOption
         ( long "database"
        <> short 'd'
        <> metavar "DATABASE"
        <> help "Database to generate the types from" )
     <*> strOption
         ( long "output"
        <> short 'o'
        <> metavar "FILENAME"
        <> help "The output path (including filename) for the file you want to generate. Relative paths work." )
     <*> strOption
         ( long "schema"
        <> short 's'
        <> metavar "SCHEMA"
        <> help "Table schema" )
allPublicColumns :: String -> Query InformationSchemaColumnTable
allPublicColumns schema = proc () -> do
  columnRow <- queryTable informationSchemaColumnTable -< ()
  restrict -< table_schema columnRow .== toNullable (pgString schema)
  returnA -< columnRow

groupColumns :: [InformationSchemaColumn] -> [TableDefinition]
groupColumns cs = map (\t -> t { columns = sortColumns (columns t)}) $ M.elems tableMap
  where
    tableMap = L.foldl' collect M.empty cs
    collect m c =
      case M.lookup (table_name c) m of
        Just t  -> M.insert (table_name c) (t { columns = c : columns t }) m
        Nothing -> M.insert (table_name c) (TableDefinition (table_name c) [c]) m

sortColumns :: [InformationSchemaColumn] -> [InformationSchemaColumn]
sortColumns = L.sortBy (comparing ordinal_position)

generateFile :: Args -> IO ()
generateFile args = do
  conn <- PGS.connectPostgreSQL (Char8.pack $ dbString args)
  allColumns <- runQuery conn (allPublicColumns (schema args)) :: IO [InformationSchemaColumn]

  h <- openFile (outputPath args) WriteMode
  hPutStrLn h . unpack $ fileHeaderText (outputPath args)
  forM_ (groupColumns allColumns) $ \td ->
    hPutStrLn h . unpack $ fullTableText (schema args) td
  hClose h

main :: IO ()
main = execParser opts >>= generateFile
  where
    opts = info (helper <*> argsParser)
      ( fullDesc
     <> progDesc "Generate a Database file with generates Opaleye types for a specific database."
     <> header "opaleye-gen - Generate Opaleye boilerplate")
