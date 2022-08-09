-- Criar o testrecord e o eventrecord não funciona sem isso
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( 
    ) where

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import Data.Csv as Cassava

import qualified Data.Vector as V

import Data.Text (Text)
import qualified Data.Text.Encoding as Text

--Importado para ser usado no Cassanva.decodeByName
import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- DataType que representa um registro do meu csv
{-
Se ouvesse um tipo de dado customizado aqui, ao inves de 'Text',
seria necessario fazer um:
instance FromField ItemType
-}
data Event = 
    Item
        { name :: Text
        , date :: Text
        , description :: Text
        }
    deriving (Eq, Show)

-- Para o Cassandra decodificar os registros, eles tem que ser instância de FromRecord (sem Header) ou FromNamedRecord (com Header)
instance FromNamedRecord Event where
    parseNamedRecord m = 
        Item
            <$> m .: "nome"
            <*> m .: "data"
            <*> m .: "desc"

testrecord :: ByteString
testrecord = "nome,data,desc\namanha,01/01/2022,insert description\n"

eventrecord :: Event
eventrecord =
    Item {
        name = "depoisDeAmanha",
        date = "02/01/2022",
        description = "insert desc"
    }

-- Quando Either retornar String, significa que houve erro
-- 
decodeItems :: ByteString -> Either String (Vector Event)
decodeItems = fmap snd . Cassava.decodeByName

--decodeItemsFromFile :: FilePath -> IO (Either String (Vector Event))
--decodeItemsFromFile filePath = catchShowIO (BL.readFile filePath) >>= return . either Left decodeItems

main = do
  --Tutorial de 2 segundos na pag inicial da biblioteca
  csvData <- BL.readFile "./csv/teste.csv"
  case decode NoHeader csvData of
    Left err -> putStrLn err
    Right v -> V.forM_ v $ \ (col1, col2, col3) ->
      putStrLn $ "Linha do meu csv: " ++ col1 ++ " " ++ col2 ++ " " ++ col3
  
  -- Alguns testes que fui fazendo pra ver como funciona
  let registro = Cassava.decodeByName "nome,data,desc\n\
                                       \amanha,01/01/2022,insert description\n"
                                         :: Either String (Header, Vector Event)

  let segundoRegistro = decodeItems "nome,data,desc\namanha,01/01/2022,insert description\n"

  let registro3 = Cassava.decodeByName "nome,data,desc\namanha,01/01/2022,insert description\n"
                                        :: Either String (Header, Vector Event)

  let registro4 = decodeItems testrecord
  
  putStrLn $ show registro
  putStrLn $ show segundoRegistro
  putStrLn $ show registro3
  putStrLn $ show registro4