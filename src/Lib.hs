-- Criar o testrecord e o eventrecord não funciona sem isso
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-
ADTS:
Prefs
Category
Event
ColorSrc

Quando quiser acessar um arquivos csv's do projeto:
categoryPath :: FilePath
eventPath    :: FilePath

Funções úteis:
getEvents          :: IO [Event]
insertToFile       :: FilePath -> Event -> IO (Either String ())
encodeEventsToFile :: FilePath -> [Event] -> IO (Either String ())

mkCategory :: Text -> Text -> Category
mkEvent    :: Text -> Int -> Int -> Int -> Text -> Text -> Bool -> ColorSrc -> Event

filterEvents       :: Eq a => a -> (Event -> a) -> [Event] -> [Event]
filterEventsByCat  :: Text -> [Event] -> [Event]
filterCloseEvents  :: Int -> Int -> Int -> [Event] -> [Event]
filterDistantEvets :: Int -> Int -> Int -> [Event] -> [Event]
filterEventName    :: Text -> [Event] -> [Event]
filterHasDesc      :: [Event] -> [Event]
filterIsReg        :: [Event] -> [Event]
-}

module Lib where

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import Data.Csv as Cassava

import Data.Text (Text, isInfixOf)
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

categoryPath :: FilePath
categoryPath = "./csv/categories.csv"

eventPath :: FilePath
eventPath = "./csv/events.csv"

data Prefs =
  Prefs
    { autodelete :: Bool
    }

data Category =
  Category
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
data ColorSrc = Custom Text | CatName | Gradient deriving (Eq, Show)

instance FromNamedRecord Prefs where
  parseNamedRecord m =
    Prefs
      <$> m .: "autodelete"

-- Para o Cassava decodificar o campo do csv e transformar no meu ADT, ele tem que ser instancia de FromNamedRecord
instance FromNamedRecord Category where
  parseNamedRecord m =
    Category
      <$> m .: "catname"
      <*> m .: "catcolor"

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
  parseField "True" =  pure True
  parseField _      = pure False

-- Usado para o Cassava saber como ler o que esta escrito no CSV e transformar no meu ADT de Cor. Se a string que estiver nesse campo não for 'Category' nem 'Gradient, ela sera um código hexadecimal, que sera transferido para minha cor Custom.
instance FromField ColorSrc where
  parseField "Category" = pure CatName
  parseField "Gradient" = pure Gradient
  parseField othertype = Custom <$> parseField othertype

instance ToNamedRecord Category where
  toNamedRecord Category{..} =
    Cassava.namedRecord
    -- noCSV .= noADT
    [ "catname"  .= catName
    , "catcolor" .= catColor
    ]

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
  toField CatName       = "Category"
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

-- Tem que por aqui o que estiver escrito no CSV, é a primeira linha do arquivo
categoryHeader :: Header
categoryHeader = Vector.fromList ["catname", "catcolor"]

-- Tem que por aqui o que estiver escrito no CSV
eventHeader :: Header
eventHeader = Vector.fromList ["name", "day", "month", "year", "description", "category", "recurrent", "color"]

mkCategory :: Text -> Text -> Category
mkCategory n c = Category {catName = n, catColor = c}

mkEvent :: Text -> Int -> Int -> Int -> Text -> Text -> Bool -> ColorSrc -> Event
mkEvent n d m y desc cat re c = Item {name = n, day = d, month = m, year = y,  description = desc, category = cat, recurrent = re, color = c}

-- Dado que tenho um Header fixo, a função codifica o ADT para o formato csv
encodeEvent :: [Event] -> ByteString
encodeEvent = Cassava.encodeByName eventHeader

-- Dado que Event é uma instancia de DefaultOrdered, posso codificar assim tambem. Eh util receber um vector event pq é isso que o cassava retorna
-- Foldable.toList transforma o Vector em []
encodeEvent' :: [Event] -> ByteString
encodeEvent' = Cassava.encodeDefaultOrderedByName

encodeEventsToFile :: FilePath -> [Event] -> IO (Either String ())
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

-- Função que, apos executar a leitra dos registro, transforma o Vector que é retornado em uma lista
getEvents :: IO [Event]
getEvents = do
  Foldable.toList <$> ioVec
  where
    ioVec :: IO (Vector Event)
    ioVec = do
      file <- decodeEventsFromFile eventPath
      return $ fromRight Vector.empty file

-- Funcao que adiciona um evento ao final de um arquivo csv. Se o arquivo não existir, ele é criado. Primeiro preciso ler o arquivo que já existe, depois coloco e evento novo no final e escrevo tudo de volta no arquivo.
insertToFile :: FilePath -> Event -> IO (Either String ())
insertToFile filePath newEvent = do
  eventVec <- file
  let eventList = Foldable.toList eventVec ++ [newEvent]
  --let newVec = Vector.cons event eventVec
  -- Não sei pq mas sem esse print o encode retorna que o arquivo ta bloqueado
  print eventVec
  encodeEventsToFile filePath eventList
  where
    -- Desenvolopo o vetor do Either
    file :: IO (Vector Event)
    file = do
        fileRead <- decodeEventsFromFile filePath
        return $ fromRight (Vector.singleton newEvent) fileRead

-- Recebe uma lista de eventos e transforma ela em um Vector de Eventos
buildEventVector :: [Event] -> Vector Event
buildEventVector = foldr Vector.cons Vector.empty

-- Função que filtra uma lista de Eventos, selecionando aqueles que tem um valor especifico em um dos campos.
-- Além da lista a ser filtrada e do valor deseja, é necessário passar qual dos campos de Event será comparado
filterEvents :: Eq a => a -> (Event -> a) -> [Event] -> [Event]
filterEvents value getter es = [e | e <- es, getter e == value]

-- Recebe os eventos do csv e devolve apenas os de uma categoria especifica. category é a função definida em data Event
filterEventsByCat :: Text -> [Event] -> [Event]
filterEventsByCat catname = filterEvents catname category

-- Filtra a lita de Eventos passada como parametro para devolver os eventos que acontecerão ANTES do dia informado
filterCloseEvents :: Int -> Int -> Int -> [Event] -> [Event]
filterCloseEvents d m y es = [e | e <- es, (year e < y) || (year e == y && month e < m) || (year e == y && month e == m && day e < d) ]

-- Filtra a lita de Eventos passada como parametro para devolver os eventos que acontecerão DEPOIS do dia informado
filterDistantEvets :: Int -> Int -> Int -> [Event] -> [Event]
filterDistantEvets d m y es = [e | e <- es, (year e > y) || (year e == y && month e > m) || (year e == y && month e == m && day e > d) ]

-- Filtra a lita de Eventos passada como parametro e devole aqueles que cuja substring de argumento está presente no nome do evento
filterEventName :: Text -> [Event] -> [Event]
filterEventName n es = [e | e <- es, n `isInfixOf` name e]

-- Filtra a lita de Eventos passada como parametro e devolve apenas eventos que tem alguma descrição
filterHasDesc :: [Event] -> [Event]
filterHasDesc es = [e | e <- es, description e /= ""]

-- Filtra a lita de Eventos passada como parametro e devolve apenas eventos que tem são recorrentes
filterIsReg :: [Event] -> [Event]
filterIsReg es = [e | e <- es, recurrent e]

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
  let cod2 = encodeEvent' $ Foldable.toList vec

  _ <- encodeEventsToFile "./csv/codificado.csv" $ Foldable.toList vec

  registro5 <- insertToFile "./csv/codificadoooo.csv" eventrecord

  --print registro
  --print segundoRegistro
  print registro3
  print registro4
  print cod1
  print cod2
  print registro5