-- Criar o testrecord e o eventrecord não funciona sem isso
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- TODO: Cores na GUI; Prefs de campos pra visualizar

{-
##################################################################################################

ADTS:

##################################################################################################

Prefs
Category
Event
ColorSrc

##################################################################################################

Variaveis úteis:

##################################################################################################

prefsPath    :: FilePath
categoryPath :: FilePath
eventPath    :: FilePath

##################################################################################################

Funções úteis:

##################################################################################################

getToday :: IO (Integer, Int, Int)

daysleft      :: Event -> (Integer, Int, Int) -> Int
dateToWeekDay :: Int -> Int -> Int -> String

getEvents              :: IO [Event]
getPrefs               :: IO Prefs
savePrefs              :: Prefs -> IO (Either String ())
insertEvent            :: Event -> IO (Either String ())
insertCategory         :: Category -> IO (Either String ())
encodeToFile           :: FilePath -> [adt] -> IO (Either String ())
deleteCategoryFromFile :: String -> IO (Either String ())
deleteEventFromFile    :: String -> IO (Either String ())

deleteAdtByField :: Eq field => (adt -> field) -> field -> [adt] -> [adt]

mkPrefs    :: Bool -> Int -> Int -> String -> Int -> Int -> Int -> Prefs
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

findCatColor :: [Category] -> String -> String
gradient     :: Int -> Int -> Color

##################################################################################################
-}

module Lib where

-- Usado na hora de ler e salvar informações nos arquivos
import Control.Exception (IOException)
import qualified Control.Exception as Exception

-- Usado para transformar os Vector que o cassava retorna em listas []
import qualified Data.Foldable as Foldable

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

-- Biblioteca para ler CSV
import Data.Csv as Cassava
    ( DefaultOrdered(..)
    , FromField(..)
    , FromNamedRecord(..)
    , ToField(..)
    , ToNamedRecord(..)
    , Header
    , (.:)
    , (.=)
    , header
    , namedRecord
    , decodeByName
    , encodeByName
    , encodeDefaultOrderedByName
    )

import Data.Text (Text, isInfixOf, pack, unpack)
import qualified Data.Text.Encoding as Text

import Graphics.UI.Gtk (Color(..))
import Data.List (sortOn, groupBy)

import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- Usado para acessar o Vector que é retornado pelo Cassava
import Data.Either (fromRight)

import Data.Time.Clock    (getCurrentTime, UTCTime(utctDay))
import Data.Time.Calendar (diffDays, fromGregorian, toGregorian)
import Data.Word (Word16)

prefsPath :: FilePath
prefsPath = "./csv/prefs.csv"

categoryPath :: FilePath
categoryPath = "./csv/categories.csv"

eventPath :: FilePath
eventPath = "./csv/events.csv"

-- Devolve o (ano, mes, dia) de hoje de acordo com o sistema operacional
getToday :: IO (Integer, Int, Int)
getToday = getCurrentTime >>= return . toGregorian . utctDay

-- O usuário poderá optar por algumas preferências, que são armazenadas em um csv através desse Adt
-- Ele poderá escolher se os eventos que passarem vão ser deletados automaticamente
-- Ele também poderá escolher um filtro e uma ordem padrão para a exibição dos eventos
data Prefs =
  Prefs
    { autodelete         :: Bool
    , defaultSort        :: Int
    , defaultFilter      :: Int
    , defaultFilterName  :: String
    , defaultFilterDay   :: Int
    , defaultFilterMonth :: Int
    , defaultFilterYear  :: Int
    }
    deriving Show

-- ADT que serve para armazenar quais as cores de cada categoria de eventos
data Category =
  Category
    { catName  :: Text
    , catColor :: Text
    }
  deriving Show

-- ADT principal do projeto. Guarda todas as informações necessárias sobre um evento para que o programa possa funcionar corretamente
-- Regularidade é o intervalo de dias que demora para um evento recorrente acontecer outra vez
-- Cada evento pertencerá a uma categoria para ficarem melhor organizados
data Event =
  Item
    { name        :: Text
    , day         :: Int
    , month       :: Int
    , year        :: Int
    , description :: Text
    , category    :: Text
    , recurrent   :: Bool
    , regularity  :: Int
    , color       :: ColorSrc
    }
  deriving (Eq, Show)

-- ADT que indica a fonte de onde o evento recebe sua cor quando mostrado na GUI. O usuário poderá optar por dar uma cor especifca para um evento apenas, ele poderá deixar o evento com a mesma cor da categoria que ele pertence, ou deixar para a cor ser ajustada automaticamente conforme a data se aproxima.
data ColorSrc = Custom Text | CatName | Gradient Int deriving (Eq, Show)

instance FromNamedRecord Prefs where
  parseNamedRecord m =
    Prefs
      <$> m .: "autodelete"
      <*> m .: "defaultSort"
      <*> m .: "defaultFilter"
      <*> m .: "defaultFilterName"
      <*> m .: "defaultFilterDay"
      <*> m .: "defaultFilterMonth"
      <*> m .: "defaultFilterYear"

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
      -- Decote latin para os campos que possuem texto terem uma abrangencia maior de caracteres
      <$> fmap Text.decodeLatin1 (m .: "name")
      <*> m .: "day"
      <*> m .: "month"
      <*> m .: "year"
      <*> fmap Text.decodeLatin1 (m .: "description")
      <*> fmap Text.decodeLatin1 (m .: "category")
      <*> m .: "recurrent"
      <*> m .: "regularity"
      <*> m .: "color"

-- Usado para o Cassava saber como ler o que vem do CSV e transformar em booleano
instance FromField Bool where
  parseField "True" = pure True
  parseField _      = pure False

-- Usado para o Cassava saber como ler o que esta escrito no CSV e transformar no meu ADT de Cor. Se a string que estiver nesse campo não for 'Category' nem 'Gradient, ela sera um código, que sera transferido para minha cor Custom.
instance FromField ColorSrc where
  parseField "Category" = pure CatName
  parseField "1"        = pure $ Gradient 1
  parseField "2"        = pure $ Gradient 2
  parseField "3"        = pure $ Gradient 3
  parseField othertype  = Custom <$> parseField othertype

-- Para o Cassava codificar o ADT para csv, é necessário que o ADT seja uma instancia de ToNamedRecord (ToRecord sem Header).
-- Se o ADT tiver um outro ADT dentro dele, é necessário fazer instancia de ToField
instance ToNamedRecord Prefs where
  toNamedRecord Prefs{..} =
    Cassava.namedRecord
    -- noCSV       .= noADT
    [ "autodelete"         .= autodelete
    , "defaultSort"        .= defaultSort
    , "defaultFilter"      .= defaultFilter
    , "defaultFilterName"  .= defaultFilterName
    , "defaultFilterDay"   .= defaultFilterDay
    , "defaultFilterMonth" .= defaultFilterMonth
    , "defaultFilterYear"  .= defaultFilterYear
    ]

instance ToNamedRecord Category where
  toNamedRecord Category{..} =
    Cassava.namedRecord
    -- noCSV     .= noADT
    [ "catname"  .= catName
    , "catcolor" .= catColor
    ]

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

-- Instancia para o cassava saber como transformar a string que leu no CSV para o Adt do haskell
instance ToField Bool where
  toField True  = "True"
  toField False = "False"

-- Instancia para o cassava saber como transformar a string que leu no CSV para o Adt do haskell
instance ToField ColorSrc where
  toField (Gradient urgency) = toField urgency
  toField CatName            = "Category"
  toField (Custom text)      = toField text

-- Diz para o Cassava a ordem padrão do header de Prefs
instance DefaultOrdered Prefs where
  headerOrder _ =
    Cassava.header
      [ "autodelete"
      , "defaultSort"
      , "defaultFilter"
      , "defaultFilterName"
      , "defaultFilterDay"
      , "defaultFilterMonth"
      , "defaultFilterYear"
      ]

-- Diz para o Cassava a ordem padrão do header de Category
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

-- Header que deveria ser usado caso o ADT não fosse uma instancia de DefaultOrdered 
prefsHeader :: Header
prefsHeader = Vector.fromList ["autodelete", "defaultSort", "defaultFilter", "defaultFilterName", "defaultFilterDay", "defaultFilterMonth", "defaultFilterYear"]

-- Tem que por aqui o que estiver escrito no CSV, é a primeira linha do arquivo
categoryHeader :: Header
categoryHeader = Vector.fromList ["catname", "catcolor"]

-- Tem que por aqui o que estiver escrito no CSV
eventHeader :: Header
eventHeader = Vector.fromList ["name", "day", "month", "year", "description", "category", "recurrent", "regularity", "color"]

mkPrefs :: Bool -> Int -> Int -> String -> Int -> Int -> Int -> Prefs
mkPrefs ad ds df dfn dfd dfm dfy = Prefs {autodelete = ad, defaultSort = ds, defaultFilter = df, defaultFilterName = dfn, defaultFilterDay = dfd, defaultFilterMonth = dfm, defaultFilterYear = dfy}

mkCategory :: Text -> Text -> Category
mkCategory n c = Category {catName = n, catColor = c}

mkEvent :: Text -> Int -> Int -> Int -> Text -> Text -> Bool -> Int -> ColorSrc -> Event
mkEvent n d m y desc cat re reg c = Item {name = n, day = d, month = m, year = y,  description = desc, category = cat, recurrent = re, regularity = reg, color = c}

-- Dado que tenho um Header fixo, a função codifica o ADT para uma stream no formato csv
encodeAdt :: ToNamedRecord adt => [adt] -> ByteString
encodeAdt = Cassava.encodeByName eventHeader

-- Dado que o ADT é uma instancia de DefaultOrdered também, posso codificar assim outra forma.
encodeAdt' :: (DefaultOrdered adt, ToNamedRecord adt) => [adt] -> ByteString
encodeAdt' = Cassava.encodeDefaultOrderedByName

-- Uso encodeAdt' para transformar o conteudo da lista de Adt's para o formato certo e BL.writeFile  escreve no path informado
encodeToFile :: (DefaultOrdered adt, ToNamedRecord adt) => FilePath -> [adt] -> IO (Either String ())
encodeToFile filePath = catchShowIO . BL.writeFile filePath . encodeAdt'

-- Recebo a stream do aquivo e uso a função do Cassa para decodificar o arquivo. A função retorna com um header, mas este é descartado
decodeAdt :: FromNamedRecord adt => ByteString -> Either String (Vector adt)
decodeAdt = fmap snd . Cassava.decodeByName

-- Leio o arquivo em prefsPath e aplico nele a função decodeAdt
decodePrefsFromFile :: IO (Either String (Vector Prefs))
decodePrefsFromFile = catchShowIO (BL.readFile prefsPath) >>= return . either Left decodeAdt

-- Leio o arquivo em categoryPath e aplico nele a função decodeAdt
decodeCategoriesFromFile :: IO (Either String (Vector Category))
decodeCategoriesFromFile = catchShowIO (BL.readFile categoryPath) >>= return . either Left decodeAdt

-- Leio o arquivo em eventPath e aplico nele a função decodeAdt
decodeEventsFromFile :: IO (Either String (Vector Event))
decodeEventsFromFile = catchShowIO (BL.readFile eventPath) >>= return . either Left decodeAdt

-- Funcao de auxilio que tenta realizar uma acao e retorna uma exceçao se alguma acontecer
catchShowIO :: IO a -> IO (Either String a)
catchShowIO action =
  fmap Right action
    `Exception.catch` handleIOException
  where
    handleIOException :: IOException -> IO (Either String a)
    handleIOException = return . Left . show

-- Executa a leitura das prefs no csv. Caso o arquivo de prefs não exista, valores Padronizados de Preferencia são retornados,
-- que estão indicado na função fromRight. Apos isso, converte para o ADT de Prefs e o retorna
getPrefs :: IO Prefs
getPrefs = do
  (ty, tm, td) <- getToday
  prefvec <- fmap (Foldable.toList . fromRight (Vector.singleton (Prefs False 5 6 "" (fromInteger ty) tm td))) decodePrefsFromFile
  return $ head prefvec

-- Função que, apos executar a leitra dos registros, transforma o Vector que é retornado em uma lista com as categorias criadas
-- Se o arquivo não for encontrado, é retornada uma lista vazia
getCategories :: IO [Category]
getCategories = do fmap (Foldable.toList . fromRight Vector.empty) decodeCategoriesFromFile

-- Função que, apos executar a leitra dos registros, transforma o Vector que é retornado em uma lista
-- Se o arquivo não for encontrado, é retornada uma lista vazia
-- Foldable.toList transforma o Vector em []
getEvents :: IO [Event]
getEvents = do fmap (Foldable.toList . fromRight Vector.empty) decodeEventsFromFile

savePrefs :: Prefs -> IO (Either String ())
savePrefs prefs = encodeToFile prefsPath [prefs]

-- Funcao que adiciona uma categoria ao final do arquivo csv das categorias. Se o arquivo não existir, ele é criado.
-- Primeiro preciso ler o arquivo que já existe, depois coloco a categoria nova no final e escrevo tudo de volta no arquivo.
insertCategory :: Category -> IO (Either String ())
insertCategory newCategory = do
  categoryVec <- file
  let categoryList = Foldable.toList categoryVec ++ [newCategory]
  print categoryVec
  encodeToFile categoryPath categoryList
  where
    file :: IO (Vector Category)
    file = do fmap (fromRight Vector.empty) decodeCategoriesFromFile

-- Funcao que adiciona um evento ao final do arquivo csv dos eventos. Se o arquivo não existir, ele é criado.
-- Primeiro preciso ler o arquivo que já existe, depois coloco o evento novo no final e escrevo tudo de volta no arquivo.
insertEvent :: Event -> IO (Either String ())
insertEvent newEvent = do
  eventVec <- file
  let eventList = Foldable.toList eventVec ++ [newEvent]
  -- Não sei pq mas sem esse print o encode retorna que o arquivo ta bloqueado
  print eventVec
  encodeToFile eventPath eventList
  where
    -- Desenvelopo o vetor do Either
    file :: IO (Vector Event)
    file = do fmap (fromRight Vector.empty) decodeEventsFromFile

-- Função genérica que retira de uma lista de adt qualquer "instancia" que tiver um valor específico em um de seus campos.
-- Se nenhum item da lista tiver o valor especificado, a lista é inalterada
deleteAdtByField :: Eq field => (adt -> field) -> field -> [adt] -> [adt]
deleteAdtByField getter v es = [e | e <- es, getter e /= v]

-- Sobrescreve o arquivo de categorias, porem sem aquela que tiver o nome informado
deleteCategoryFromFile :: String -> IO (Either String ())
deleteCategoryFromFile k = do
  catVec <- file
  let cs = Foldable.toList catVec
  let newcs = deleteAdtByField catName (pack k) cs
  print catVec
  encodeToFile categoryPath newcs
  where
    file :: IO (Vector Category)
    file = do fmap (fromRight Vector.empty) decodeCategoriesFromFile

-- Sobrescreve o arquivo de eventos, porem sem aquele que tiver o nome informado
deleteEventFromFile :: String -> IO (Either String ())
deleteEventFromFile k = do
  eventVec <- file
  let es = Foldable.toList eventVec
  let newes = deleteAdtByField name (pack k) es
  print eventVec
  encodeToFile eventPath newes
  where
    file :: IO (Vector Event)
    file = do fmap (fromRight Vector.empty) decodeEventsFromFile

-- deprecated
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

-- Recebe um evento e uma triple representando o dia de hoje, e calcula quantos dias faltam para o evento
-- Leva em conta se o Evento é recorrente ou não, e se ele já aconteceu ou vai acontecer
daysleft :: Event -> (Integer, Int, Int) -> Int
daysleft e (ty, tm, td)
  -- Caso o evento for recorrente, e a data armazenada já tiver passado, mostro a próxima ocorrencia
  | recurrent e && diff /= abs diff = nextDay
  | otherwise                       = diff
  where
    -- Extraio os dados do evento recebido
    d     = day e
    m     = month e
    y     = toInteger $ year e
    -- Transformo a tripla recebida em um Day
    today = fromGregorian ty tm td
    -- Agora é possível calcular a diferença de dias
    diff  = fromInteger $ diffDays (fromGregorian y m d) today
    -- Caso seja um evento regular, reg é o intervalo de tempo para o evento ocorrer outra vez
    reg     = regularity e
    nextDay = diff `mod` reg

-- Função que recebe 3 inteiros representando uma data e devolve uma string com o dia da semana dessa data
dateToWeekDay :: Int -> Int -> Int -> String
dateToWeekDay d m y = string
  where
    -- Fórmula para saber o dia da semana de uma data d/m/y no calendário gregoriano
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
 -- Ordem alfabética
 | method == "name"        = sortOn name es
 -- Ordena alfabeticamente as categorias, e alfabeticamente o nome de cada evento de uma mesma categorias
 | method == "category"    = sortByCategory es
 -- Data mais antiga para a mais recente
 | method == "date"        = sortByDate es
 -- Coloca os eventos recorrentes na frente, sem alterar a ordem entre eles ou entre os que não são recorrentes
 | method == "recurrent"   = filterIsReg es   ++ [e | e <- es, not $ recurrent e]
 -- Ordem com a mesma ideia dos eventos recorrentes, porém o critério agora é ter descrição
 | method == "description" = filterHasDesc es ++ [e | e <- es, description e == ""]
 | otherwise               = es

-- Reordena a lista de eventos de acordo com o mais antigo para o mais recente.
-- Primeiro os eventos devem ser ordenados pelo ano. Depois agrupados em listas para cada ano que houver.
-- Então podem ser organizados por mes, e o processo é repetido para os dias.
sortByDate :: [Event] -> [Event]
sortByDate es = concat $ concat dsorted
  where
    ysorted     = sortOn year es
    -- Agrupo todos os anos da lista de eventos que são iguais
    yearGroups  = groupBy yearEq ysorted
    -- Cada lista esta agrupada por ano. Agora cada uma é organizada pelo mes
    msorted     = listsSort month yearGroups
    -- Agora aplico groupBy em cada uma das listas agrupadas por ano e tenho listas agrupadas por ano e mes
    monthGroups = map (groupBy monthEq) msorted
    dsorted     = sortByDay monthGroups

-- Reordena a lista para junter os eventos com as mesmas categorias.
-- Secundariamente os eventos de uma mesma categoria são organizados alfabeticamente.
sortByCategory :: [Event] -> [Event]
sortByCategory es = concat nsorted
  where
    csorted = sortOn category es
    catGroups = groupBy catEq csorted
    nsorted = listsSort category catGroups

-- FUNÇÃO NÃO FUNCIONOU MUITO BEM. Tive que recorrer para groupBy
-- Função auxiliar. Agrupa uma lista de eventos em várias listas de de eventos de acordo com algum dos campos do Evento.
-- Por exemplo, se quiser agrupar de tal forma que cada sublista tenha os anos iguais: listsOfDates year es
-- Para meses iguais: listsOfDates month es
-- E assim para qualquer campo instancia de Eq de Event
listsOfDates :: Eq b => (a -> b) -> [a] -> [[a]]
listsOfDates _ []  = []
listsOfDates _ [e] = [[e]]
listsOfDates getter (e:es)
 | getter e == getter (head es) = (e : [head es]) : listsOfDates getter (tail es)
 | otherwise                    = [e] : listsOfDates getter es

-- Compara se dois eventos tem anos iguais
yearEq :: Event -> Event -> Bool
yearEq e1 e2 = year e1 == year e2

-- Compara se dois eventos tem meses iguais
monthEq :: Event -> Event -> Bool
monthEq e1 e2 = month e1 == month e2

-- COmpara se dois eventos tem a mesma categoria
catEq :: Event -> Event -> Bool
catEq e1 e2 = category e1 == category e2

-- Função auxiliar. Recebe varias listas de algum ADT e organiza cada uma delas de acordo com o campo do adt informado
listsSort :: Ord field => (adt -> field) -> [[adt]] -> [[adt]]
listsSort getter = map $ sortOn getter

-- Função auxiliar. Ordena cada lista individualmente por dia.
sortByDay :: [[[Event]]] -> [[[Event]]]
sortByDay = map $ listsSort day

-- A partir do nome de uma categoria, procuro por ela em uma lista de categorias e retorno sua cor. Util para a GUI
findCatColor :: [Category] -> String -> String
findCatColor cs n = head color
  where
    color = [unpack $ catColor c | c <- cs, catName c == pack n]

-- Função para Quando o Evento deve receber uma cor automaticamente conforme a data se aproxima.
-- Recebe a quantidade de dias até a data e retorna uma cor em função dela
gradient :: Int -> Int -> Color
gradient x urgency = Color red green blue
  where
    -- Speed com que o gradiente muda depende da urgencia.
    -- Com urgencia 1, o gradiente alcança o máximo em   7 dias
    -- Com urgencia 2, o gradiente alcança o máximo em  31 dias
    -- Com urgencia 3, o gradiente alcança o máximo em 365 dias
    speed  = 10571
    speed2 =  2387
    speed3 =   203
    -- Max e min usados para ter um 'cap' nos valores e o gradiente não resetar
    red = case urgency of
      3 -> read $ show $ max 8000  (45000 - (x * (speed3 `div` 2)))
      2 -> read $ show $ max 8000  (45000 - (x * (speed2 `div` 2)))
      _ -> read $ show $ max 8000  (45000 - (x * (speed  `div` 2)))
    green = case urgency of
      3 -> read $ show $ min 30000 (8000  + (x * speed3))
      2 -> read $ show $ min 30000 (8000  + (x * speed2))
      _ -> read $ show $ min 30000 (8000  + (x * speed))

    blue = case urgency of
      3 -> read $ show $ min 17500 (8000  + (x * (speed3 `div` 4)))
      2 -> read $ show $ min 17500 (8000  + (x * (speed2 `div` 4)))
      _ -> read $ show $ min 17500 (8000  + (x * (speed  `div` 4)))
