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
Se ouvesse um ADT customizado aqui, ao inves de 'Text',
seria necessario fazer um:
instance FromField adtName
instance ToField adtName

Para o cassava saber como transformar esse ADT em uma parte do registro csv e vice versa
-}
data Event =
    Item
        { name :: Text
        , date :: Text
        , description :: Text
        }
    deriving (Eq, Show)

-- Para o Cassava decodificar os registros, eles tem que ser instância de FromRecord (sem Header) ou FromNamedRecord (com Header)
instance FromNamedRecord Event where
    parseNamedRecord m =
        Item
            <$> fmap Text.decodeLatin1 (m .: "nome")
            <*> m .: "data"
            <*> m .: "desc"

-- Para o Cassava codificar o ADT para csv, é necessário que o ADT seja uma instancia de ToNamedRecord (ToRecord sem Header). Se o ADT tiver um outro ADT dentro dele, tbm é necessário 
instance ToNamedRecord Event where
  toNamedRecord Item{..} =
    Cassava.namedRecord
      -- noCSV .= noADT
      [ "nome" .= name
      , "data" .= date
      , "desc" .= description
      ]

-- Diz para o Cassava a ordem padrão do meu Header
instance DefaultOrdered Event where
  headerOrder _ =
    Cassava.header
      [ "nome"
      , "data"
      , "desc"
      ]

-- Tem que por aqui o que estiver escrito no CSV
eventHeader :: Header
eventHeader = Vector.fromList ["nome", "data", "desc"]

mkEvent :: Text -> Text -> Text -> Event
mkEvent name date desc = Item {name = name, date = date, description = desc}

-- Dado que tenho um Header fixo, a função codifica o ADT para o formato csv
encodeEvent :: [Event] -> ByteString
encodeEvent = Cassava.encodeByName eventHeader

-- Dado que Event é uma instancia de DefaultOrdered, posso codificar assim tambem. Eh util receber um vector event pq é isso que o cassava retorna
-- Foldable.toList transforma o Vector em []
encodeEvent' :: Vector Event -> ByteString
encodeEvent' = Cassava.encodeDefaultOrderedByName . Foldable.toList

{-
--Função que tava no tutorial que não entendi direito, talvez seja util
encodeItems
  :: Vector Item
  -> ByteString
encodeItems =
  Cassava.encodeDefaultOrderedByName . Foldable.toList
-}

encodeEventsToFile :: FilePath -> Vector Event -> IO (Either String ())
encodeEventsToFile filePath = catchShowIO . BL.writeFile filePath . encodeEvent'

-- Quando Either retornar String, significa que houve erro
decodeEvents :: ByteString -> Either String (Vector Event)
decodeEvents = fmap snd . Cassava.decodeByName

decodeEventsFromFile :: FilePath -> IO (Either String (Vector Event))
decodeEventsFromFile filePath = catchShowIO (BL.readFile filePath) >>= return . either Left decodeEvents

-- Funcao de auxilio que tenta realizar uma acao e retorna uma exceçao se alguma acontecer
catchShowIO :: IO a -> IO (Either String a)
catchShowIO action =
  fmap Right action
    `Exception.catch` handleIOException
  where
    handleIOException :: IOException -> IO (Either String a)
    handleIOException = return . Left . show

testrecord :: ByteString
testrecord = "nome,data,desc\namanha,01/01/2022,insert description\n"

eventrecord :: Event
eventrecord =
    Item {
        name = "depoisDeAmanha",
        date = "02/01/2022",
        description = "insert desc"
    }

main = do
  --Tutorial de 2 segundos na pag inicial da biblioteca
  csvData <- BL.readFile "./csv/teste.csv"
  case decode NoHeader csvData of
    Left err -> putStrLn err
    Right v -> V.forM_ v $ \ (col1, col2, col3) ->
      putStrLn $ "Linha do meu csv: " ++ col1 ++ " " ++ col2 ++ " " ++ col3

  -- Alguns testes que fui fazendo pra ver como funciona
  --Testei construindo os eventos na mão
  let registro = Cassava.decodeByName "nome,data,desc\n\
                                       \amanha,01/01/2022,insert description\n"
                                         :: Either String (Header, Vector Event)

  -- Consigo decodificar com a função auxiliar e construindo o evento na mão
  let segundoRegistro = decodeEvents "nome,data,desc\namanha,01/01/2022,insert description\n"

  -- Consigo decodificar o csv com a função auxiliar e o evento que tinha deixado pronto
  let registro3 = decodeEvents testrecord

  -- Função que le um arquivo funcionando
  registro4 <-decodeEventsFromFile "./csv/teste.csv"

  -- Se eu tenho um Header posso codificar para csv assim
  let cod1 = encodeEvent [eventrecord, eventrecord]

  let vec = Vector.singleton eventrecord
  let cod2 = encodeEvent' vec

  encodeEventsToFile "./csv/codificado.csv" vec

  print registro
  print segundoRegistro
  print registro3
  print registro4
  print cod1
  print cod2