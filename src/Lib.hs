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

Variaveis úteis:

categoryPath :: FilePath
eventPath    :: FilePath

Funções úteis:

getToday :: IO (Integer, Int, Int)

daysleft :: Event -> (Integer, Int, Int) -> Int
dateToWeekDay :: Int -> Int -> Int -> String

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

sortEventsBy :: String -> [Event] -> [Event]
sortEventsBy method es
Ver os comentarios na função para ver qual string passar
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
import Data.List (sortOn)

--Importado para ser usado no Cassanva.decodeByName
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Either

import Data.Time.Clock
import Data.Time.Calendar
import Data.List (groupBy)

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

-- Devolve o (ano, mes, dia)
getToday :: IO (Integer, Int, Int)
getToday = getCurrentTime >>= return . toGregorian . utctDay

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
    , regularity :: Int
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
      <*> m .: "regularity"
      <*> m .: "color"

-- Usado para o Cassava saber como ler o que vem do CSV e transformar em booleano
instance FromField Bool where
  parseField "True" = pure True
  parseField _      = pure False

-- Usado para o Cassava saber como ler o que esta escrito no CSV e transformar no meu ADT de Cor. Se a string que estiver nesse campo não for 'Category' nem 'Gradient, ela sera um código hexadecimal, que sera transferido para minha cor Custom.
instance FromField ColorSrc where
  parseField "Category" = pure CatName
  parseField "Gradient" = pure Gradient
  parseField othertype  = Custom <$> parseField othertype

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
      , "regularity"  .= regularity
      , "color"       .= color
      ]

instance ToField Bool where
  toField True  = "True"
  toField False = "False"

instance ToField ColorSrc where
  toField Gradient      = "Gradient"
  toField CatName       = "Category"
  toField (Custom text) = toField text

-- Diz para o Cassava a ordem padrão do header das categorias
instance DefaultOrdered Category where
  headerOrder _ =
    Cassava.header
      [ "catname"
      , "catcolor"
      ]

-- Diz para o Cassava a ordem padrão do meu Header de Events
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
      , "regularity"
      , "color"
      ]

-- Tem que por aqui o que estiver escrito no CSV, é a primeira linha do arquivo
categoryHeader :: Header
categoryHeader = Vector.fromList ["catname", "catcolor"]

-- Tem que por aqui o que estiver escrito no CSV
eventHeader :: Header
eventHeader = Vector.fromList ["name", "day", "month", "year", "description", "category", "recurrent", "regularity", "color"]

mkCategory :: Text -> Text -> Category
mkCategory n c = Category {catName = n, catColor = c}

mkEvent :: Text -> Int -> Int -> Int -> Text -> Text -> Bool -> Int -> ColorSrc -> Event
mkEvent n d m y desc cat re reg c = Item {name = n, day = d, month = m, year = y,  description = desc, category = cat, recurrent = re, regularity = reg, color = c}

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
  -- Não sei pq mas sem esse print o encode retorna que o arquivo ta bloqueado
  print eventVec
  encodeEventsToFile filePath eventList
  where
    -- Desenvelopo o vetor do Either
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

-- Filtra a lista de Eventos passada como parametro para devolver os eventos que acontecerão ANTES do dia informado
filterCloseEvents :: Int -> Int -> Int -> [Event] -> [Event]
filterCloseEvents d m y es = [e | e <- es, (year e < y) || (year e == y && month e < m) || (year e == y && month e == m && day e < d) ]

-- Filtra a lista de Eventos passada como parametro para devolver os eventos que acontecerão DEPOIS do dia informado
filterDistantEvets :: Int -> Int -> Int -> [Event] -> [Event]
filterDistantEvets d m y es = [e | e <- es, (year e > y) || (year e == y && month e > m) || (year e == y && month e == m && day e > d) ]

-- Filtra a lista de Eventos passada como parametro e devole aqueles que cuja substring de argumento está presente no nome do evento
filterEventName :: Text -> [Event] -> [Event]
filterEventName n es = [e | e <- es, n `isInfixOf` name e]

-- Filtra a lista de Eventos passada como parametro e devolve apenas eventos que tem alguma descrição
filterHasDesc :: [Event] -> [Event]
filterHasDesc es = [e | e <- es, description e /= ""]

-- Filtra a lista de Eventos passada como parametro e devolve apenas eventos que são recorrentes
filterIsReg :: [Event] -> [Event]
filterIsReg es = [e | e <- es, recurrent e]

filterEventsBy :: String -> Text -> [Event] -> [Event]
filterEventsBy method v es
 | method == "category" = filterEventsByCat v es
 | otherwise = es

-- Recebe um evento e uma triple representando o dia de hoje, e calcula quantos dias faltam para o evento
-- Leva em conta se o Evento é recorrente ou não, e se ele já aconteceu ou vai acontecer
daysleft :: Event -> (Integer, Int, Int) -> Int
daysleft e (ty, tm, td)
--  Caso o evento for recorrente, e a data armazenada já tiver passado, mostro a próxima ocorrencia
  | recurrent e && diff /= abs diff = nextDay
  | otherwise                       = diff
  where
    d     = day e
    m     = month e
    y     = toInteger $ year e
    today = fromGregorian ty tm td
    diff  = fromInteger $ diffDays (fromGregorian y m d) today
    -- Caso seja um evento regular
    reg     = regularity e
    nextDay = diff `mod` reg

-- Função que recebe 3 inteiros representando uma data e devolve uma string com o dia da semana dessa data
dateToWeekDay :: Int -> Int -> Int -> String
dateToWeekDay d m y = string
  where
    y0 =  y - (14 - m) `div` 12
    x = y0 + y0 `div` 4 - y0 `div` 100 + y0 `div` 400
    m0 = m + 12 * ((14 - m) `div` 12) - 2
    d0 = (d + x + 31 * m0 `div` 12) `mod` 7
    string = case d0 of
      0 -> "Domingo"
      1 -> "Segunda-feira"
      2 -> "Terça-feira"
      3 -> "Quarta-feira"
      4 -> "Quinta-feira"
      5 -> "Sexta-feira"
      6 -> "Sábado"
      _ -> "Erro"

-- Ordena a lista de eventos recebida de acordo com o critério informado na String.
-- Se a String recebida não for uma das predefinidas, a lista é retornada sem alterações
sortEventsBy :: String -> [Event] -> [Event]
sortEventsBy method es
 | method == "name"        = sortOn name es
 | method == "date"        = sortByDate es
 | method == "recurrent"   = filterIsReg es   ++ [e | e <- es, not $ recurrent e]
 | method == "description" = filterHasDesc es ++ [e | e <- es, description e == ""]
 | otherwise               = es

-- Reordena a lista de eventos de acordo com o mais antigo para o mais recente
sortByDate :: [Event] -> [Event]
sortByDate es = concat $ concat dsorted
  where
    ysorted     = sortOn year es
    yearGroups  = groupBy yearEq ysorted
    msorted     = sortByMonth yearGroups
    monthGroups = map (groupBy monthEq) msorted
    dsorted     = sortByDay monthGroups


-- TODO: REMOVER
-- Função auxiliar. Agrupa uma lista de eventos em várias listas de de eventos de acordo com algum dos campos do Evento.
-- Por exemplo, se quiser agrupar de tal forma que cada sublista tenha os anos iguais: listsOfDates year es
-- Para meses iguais: listsOfDates month es
-- E assim para qualquer campo instancia de Eq de Event
listsOfDates :: Eq b => (a -> b) -> [a] -> [[a]]
listsOfDates _ []  = []
listsOfDates _ [e] = [[e]]
listsOfDates getter (e:es)
 | getter e == getter (head es) = (e : [head es]) : listsOfDates getter (tail es)
 | otherwise                = [e] : listsOfDates getter es

yearEq :: Event -> Event -> Bool
yearEq e1 e2 = year e1 == year e2

monthEq :: Event -> Event -> Bool
monthEq e1 e2 = month e1 == month e2

-- Função auxiliar. Recebe varias listas de eventos, que estão separadas por anos, isto é [[2020], [2021], [2004]]
-- Organiza cada uma dessas listas por mes
sortByMonth :: [[Event]] -> [[Event]]
sortByMonth = map (sortOn month)

-- Função auxiliar. Funciona que nem a ordenação por mes, mas agora as listas estão ordenadas por mes
sortByDay :: [[[Event]]] -> [[[Event]]]
sortByDay = map (map (sortOn day))

testrecord :: ByteString
testrecord = "name,day,month,year,description,category,recurrent,color\namanha,01,01,2022,insert description,none,False,Gradient\n"

eventrecord :: Event
eventrecord =
  Item { name     = "depoisDeAmanha"
    , day         = 04
    , month       = 05
    , year        = 2000
    , description = "insert desc"
    , category    = "none"
    , recurrent   = True
    , regularity  = 7
    , color       = Custom "#EEEEEE"
  }

eventrecord2 :: Event
eventrecord2 =
  Item { name     = "AntesdeHoje"
    , day         = 10
    , month       = 05
    , year        = 2000
    , description = "insert desc"
    , category    = "none"
    , recurrent   = True
    , regularity  = 7
    , color       = Custom "#EEEEEE"
  }

eventrecord3 :: Event
eventrecord3 =
  Item { name     = "depoisDeAmanha"
    , day         = 28
    , month       = 05
    , year        = 2000
    , description = "insert desc"
    , category    = "none"
    , recurrent   = False
    , regularity  = 7
    , color       = Custom "#EEEEEE"
  }

eventrecord4 :: Event
eventrecord4 =
  Item { name     = "AntesdeHoje"
    , day         = 02
    , month       = 10
    , year        = 2023
    , description = "insert desc"
    , category    = "none"
    , recurrent   = False
    , regularity  = 7
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

  -- Consigo decodificar o csv com a função auxiliar e o evento que tinha deixado pronto
  --let registro3 = decodeEvents testrecord

  -- Função que le um arquivo funcionando
  --registro4 <-decodeEventsFromFile "./csv/teste.csv"

  --let vec = Vector.singleton eventrecord
  --let cod2 = encodeEvent' $ Foldable.toList vec

  --_ <- encodeEventsToFile "./csv/codificado.csv" $ Foldable.toList vec

  let dia = dateToWeekDay 02 01 2000
  hoje <- getToday

  let diff  = daysleft eventrecord  hoje
  let diff2 = daysleft eventrecord2 hoje
  let diff3 = daysleft eventrecord3 hoje
  let diff4 = daysleft eventrecord4 hoje
  let events = [eventrecord3, eventrecord4, eventrecord, eventrecord2]
--  let concatedList  = listsOfYears events
--  let concatedList'  = listsOfYears (sortOn year events)
  --print registro3
  --print registro4
  --print cod2
  {-print dia
  putStrLn "Diff True, depois: "
  print diff
  putStrLn "Diff True, antes: "
  print diff2
  putStrLn "Diff False, depois: "
  print diff3
  putStrLn "Diff False, antes: "
  print diff4-}
  print $ sortEventsBy "date" events
  --putStrLn "\nTentativa 1 : "
  --print concatedList
  --putStrLn "\nTentativa 2 : "
  --print concatedList'