-- Criar o testrecord e o eventrecord não funciona sem isso
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib
  (Prefs,
  Category,
  Event,
  mkEvent,
  encodeEventsToFile,
  decodeEventsFromFile,
  insertToFile
  ) where

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import Data.Csv as Cassava

import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Graphics.UI.Gtk ( Color(..) )

--Importado para ser usado no Cassanva.decodeByName
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Either

-- DataType que representa um registro do meu csv
{-
Se ouvesse um ADT customizado aqui, ao inves de 'Text',
seria necessario fazer um:
instance FromField adtName
instance ToField adtName

Para o cassava saber como transformar esse ADT em uma parte do registro csv e vice versa
-}
data Prefs =
  Prefs
    { autodelete :: Bool
    }

data Category =
  Cat
    { catName :: Text
    , catColor :: Text
    }

data Event =
  Item
    { name :: Text
    , day :: Int
    , month :: Int
    , year :: Int
    , description :: Text
    , category :: Text
    , recurrent :: Bool
    , color :: ColorSrc
    }
  deriving (Eq, Show)

-- ADT que indica a fonte de onde o evento recebe sua cor. O usuário poderá optar por dar uma cor especifca para um evento apenas, ele poderá deixar o evento com a mesma cor da categoria que ele pertence, ou deixa para a cor ser ajustada automaticamente conforme a data se aproxima.
data ColorSrc = Custom Text | Category | Gradient deriving (Eq, Show)

-- Para o Cassava decodificar os registros, eles tem que ser instância de FromRecord (sem Header) ou FromNamedRecord (com Header)
instance FromNamedRecord Event where
  parseNamedRecord m =
    Item
      <$> fmap Text.decodeLatin1 (m .: "name")
      <*> m .: "day"
      <*> m .: "month"
      <*> m .: "year"
      <*> m .: "description"
      <*> m .: "category"
      <*> m .: "recurrent"
      <*> m .: "color"

-- Usado para o Cassava saber como ler o que vem do CSV e transformar em booleano
instance FromField Bool where
  --parseField "True" =
  --  pure True

  --parseField otherType =
  --  pure False
  parseField field
    | field == "True" = pure True
    | otherwise       = pure False

instance FromField ColorSrc where
  parseField "Category" = pure Category
  parseField "Gradient" = pure Gradient
  parseField othertype = Custom <$> parseField othertype
--TODO:  parseField color = Custom color

-- Para o Cassava codificar o ADT para csv, é necessário que o ADT seja uma instancia de ToNamedRecord (ToRecord sem Header). Se o ADT tiver um outro ADT dentro dele, tbm é necessário 
instance ToNamedRecord Event where
  toNamedRecord Item{..} =
    Cassava.namedRecord
      -- noCSV        .= noADT
      [ "name"        .= name
      , "day"         .= day
      , "month"       .= month
      , "year"        .= year
      , "description" .= description
      , "category"    .= category
      , "recurrent"   .= recurrent
      , "color"       .= color
      ]

instance ToField Bool where
  toField True  = "True"
  toField False = "False"

instance ToField ColorSrc where
  toField Gradient      = "Gradient"
  toField Category      = "Category"
  toField (Custom text) = toField text

-- Diz para o Cassava a ordem padrão do meu Header
instance DefaultOrdered Event where
  headerOrder _ =
    Cassava.header
      [ "name"
      , "day"
      , "month"
      , "year"
      , "description"
      , "category"
      , "recurrent"
      , "color"
      ]

-- Tem que por aqui o que estiver escrito no CSV
eventHeader :: Header
eventHeader = Vector.fromList ["name", "day", "month", "year", "description", "category", "recurrent", "color"]

mkEvent :: Text -> Int -> Int -> Int -> Text -> Text -> Bool -> ColorSrc -> Event
mkEvent n d m y desc cat re c = Item {name = n, day = d, month = m, year = y,  description = desc, category = cat, recurrent = re, color = c}

-- Dado que tenho um Header fixo, a função codifica o ADT para o formato csv
encodeEvent :: [Event] -> ByteString
encodeEvent = Cassava.encodeByName eventHeader

-- Dado que Event é uma instancia de DefaultOrdered, posso codificar assim tambem. Eh util receber um vector event pq é isso que o cassava retorna
-- Foldable.toList transforma o Vector em []
encodeEvent' :: Vector Event -> ByteString
encodeEvent' = Cassava.encodeDefaultOrderedByName . Foldable.toList

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

-- Funcao que adiciona um evento ao final de um arquivo csv. Se o arquivo não existir, ele é criado
insertToFile :: FilePath -> Event -> IO (Either String ())
insertToFile filePath event = do
  eventVec <- file
  let newVec = Vector.cons event eventVec
  encodeEventsToFile filePath newVec
  where
    -- Desenvolopo o vetor do Either
    file :: IO (Vector Event)
    file = do 
        fileRead <- decodeEventsFromFile filePath
        return $ fromRight (Vector.singleton event) fileRead

testrecord :: ByteString
testrecord = "name,day,month,year,description,category,recurrent,color\namanha,01,01,2022,insert description,none,False,Gradient\n"

eventrecord :: Event
eventrecord =
  Item { name     = "depoisDeAmanha"
    , day         = 02
    , month       = 01
    , year        = 2022
    , description = "insert desc"
    , category    = "none"
    , recurrent   = False
    , color       = Custom "#EEEEEE"
  }

main :: IO ()
main = do
  --Tutorial de 2 segundos na pag inicial da biblioteca
  csvData <- BL.readFile "./csv/teste.csv"
  case decode NoHeader csvData of
    Left err -> putStrLn err
    Right v -> Vector.forM_ v $ \ (col1, col2, col3) ->
      putStrLn $ "Linha do meu csv: " ++ col1 ++ " " ++ col2 ++ " " ++ col3

  -- Alguns testes que fui fazendo pra ver como funciona
  --Testei construindo os eventos na mão
  --let registro = Cassava.decodeByName "name,day,month,year,description,category,\
  --                                    \recurrent,color\namanha,01,01,2022,insert \
  --                                    \description,none,False,Gradient\n"
  --                                      :: Either String (Header, Vector Event)

  -- Consigo decodificar com a função auxiliar e construindo o evento na mão
  --let segundoRegistro = decodeEvents "name,day,month,year,description,category,recurrent,color\namanha,01,01,2022,insert description,none,False,Gradient\n"

  -- Consigo decodificar o csv com a função auxiliar e o evento que tinha deixado pronto
  let registro3 = decodeEvents testrecord

  -- Função que le um arquivo funcionando
  registro4 <-decodeEventsFromFile "./csv/teste.csv"

  -- Se eu tenho um Header posso codificar para csv assim
  let cod1 = encodeEvent [eventrecord, eventrecord]

  let vec = Vector.singleton eventrecord
  let cod2 = encodeEvent' vec

  _ <- encodeEventsToFile "./csv/codificado.csv" vec

  registro5 <- insertToFile "./csv/codificadoooo.csv" eventrecord

  --print registro
  --print segundoRegistro
  print registro3
  print registro4
  print cod1
  print cod2
  print registro5