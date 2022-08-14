-- {-# LANGUAGE OverloadedLabels  #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE PackageImports #-}

module Main where

import Lib
import Data.IORef
import Control.Monad

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Data.String
import Control.Monad.IO.Class (MonadIO(liftIO))
--import Data.Text.Read
--import Text.Printf (printf)

import qualified Data.HashTable.IO as H
import Data.Maybe


--A definição abaixo cria automaticamente as funções ayyyy byyyy e cyyyy. Assim, ao fazer Teste ayyyy ele vai pegar o int que tá em ayyyy...
data Teste = Teste {
                      ayyyy :: Int
                    , byyyy :: Int
                    , cyyyy :: Int
                    }


{-
createEvent :: Event -> IO Frame
createEvent n = do
    builder <- builderNew
    builderAddFromFile builder "./ui/UI_Evento.glade"
    eventTitle          <- getLabelObject builder  ""   (unpack  . name n)
    eventDescription    <- getLabelObject builder  ""   (unpack  . description n)
    eventCategory       <- getLabelObject builder  ""   (unpack  .  category n)
    eventDay            <- getLabelObject builder  ""   (showInt .  day n)
    eventMonth          <- getLabelObject builder  ""   (showInt .  month n)
    eventYear           <- getLabelObject builder  ""   (showInt .  year n)
    eventRecurrent      <- getLabelObject builder  ""   (if (recurrent n) then "Sim" else "Não")
    --button <- builderGetObject builder castToButton "button1"
    --onClicked button $ do putStrLn k

    builderGetObject builder castToFrame "frame1"
    where
        k = a ++ " test"
-}

--Hashtable verificado em github.com/gregorycollins/hashtables
type HashTable a b = H.BasicHashTable a b

makeHashTable :: IO (HashTable String Fixed)
makeHashTable = do H.new

{--

EXEMPLO DE PRINT

printHash :: HashTable String String -> String -> IO ()
printHash ht s = do
    x <- H.lookup ht s
    H.insert ht "Nani" "x"
    putStrLn $ if x == Nothing then "Nothing" else "teste"
--}

main :: IO ()
main = do
    initGUI
    builderMain <- makeBuilder "./ui/UI_main.glade"
    window <- builderGetObject builderMain castToWindow "mainWindow"

    -- #IDEIA: ao ler o arquivo de categorias, chamar aqui a função create category para cada linha da tabela.
    --                  n <- createCategory "Nome da Categoria" categoriesMap switcher
    --                  talvez adicionar o parâmetro de cor? Depende de como podemos controlá-lo.
    -- Após adicionar todas as categorias, pegar o arquivo que lê todos os eventos, 
    -- ir de categoria em categoria adicionando-os às suas correspondentes categorias.
    
    --A hashtable pode ser passada como uma espécie de ponteiro. "Modificar" em outras funções altera esta própria variável.
    categoriesMap <- makeHashTable

    --Switcher comporta todas as categorias que, por sua vez, contém os respectivos eventos.
    switcher <- builderGetObject builderMain castToNotebook "switcherMain"

    --Para criar e adicionar uma nova categoria, basta chamar a função createCategory.
    --Tal função espera como parâmetro o nome da categoria, a hashtable e o parent.
    n <- createCategory "Minha categoria 1" categoriesMap switcher
    n <- createCategory "Minha categoria 2" categoriesMap switcher
    n <- createCategory "Minha categoria 3" categoriesMap switcher

   -- window' <- createCompromisso "compromisso a" "data a" "tempo restante a" "dia da semana a" "descrição a"
   -- window'' <- createCompromisso "compromisso b" "data b" "tempo restante b" "dia da semana b" "descrição b"
    --hbox <- builderGetObject builder castToVScrollbar "vscrollbar"

    -- ADD EVENT ###################################################################################
    btnAddEvent <- getButton builderMain "btnAddEvent"
    onClicked btnAddEvent $ do bdAdd <- makeBuilder "./ui/UI_novoEvento.glade"
                               newEvent <- createEvent bdAdd
                               -- CONFIRM  ##################################################################
                               -- Adiciona o novo evento a uma categoria já existente.
                               -- Caso a categoria não exista, ela é criada e este novo evento é inserido.
                               confirm <- getButton bdAdd "btnConfirmar"
                               onClicked confirm $ do notValid <- checkFields bdAdd
                                                      if notValid
                                                        then generateWarningMessage "Preencha todos os campos."
                                                        else do
                                                            addEvent bdAdd builderMain
                                                            widgetDestroy newEvent
                                                            putStrLn "Novo evento adicionado."
                                                            endDo
                               endDo
                          -- ###########################################################################
    -- ##############################################################################################

    --Exemplo de como capturar a ação de um botão do submenu. Note que no glade tivemos que associar uma nova ação ao item.
    action <- builderGetObject builderMain castToAction "action1"
    onActionActivate action $ do (putStrLn "test")

    --Mostra todos os widgets presentes em window.
    widgetShowAll window
    on window deleteEvent $ liftIO mainQuit >> return False

    mainGUI

endDo :: IO ()
endDo = do putStr ""


--Cria uma categoria com um dado nome s. Adiciona-a na HashTable. Adiciona-o ao parent.
createCategory :: String -> HashTable String Fixed -> Notebook-> IO ()
createCategory s ht parent = do
    categoryBuilder <- makeBuilder "./ui/UI_Categoria.glade"
    fixedBox <- getFixed categoryBuilder "mainFixed"
    lblCategory <- getLabel categoryBuilder "lblCategoryTitle"
    labelSetText lblCategory s
    H.insert ht s fixedBox
    notebookAppendPage parent fixedBox s
    return ()



--Verifica se uma dada categoria existe.
categoryExists :: String -> HashTable String Fixed -> IO Bool
categoryExists s ht = do
    value <- H.lookup ht s
    return $ isJust value

--Gera uma dialog box com uma dada string. É do tipo warning.
generateWarningMessage :: String -> IO ()
generateWarningMessage s = do
    warningBuilder <- makeBuilder "./ui/UI_Aviso.glade"
    warningWindow  <- builderGetObject warningBuilder castToMessageDialog "window"
    warningMessage <- getLabel warningBuilder "lblMessage"
    labelSetText warningMessage s
    confirm <- getButton warningBuilder "btnConfirm"
    onClicked confirm $ widgetDestroy warningWindow
    widgetShowAll warningWindow
    putStrLn "Warning: há algum campo vazio."


--Verifica se todos os campos são válidos, isto é, se existem campos vazios.
--Retorna true se algum campo for vazio.
checkFields :: Builder -> IO Bool
checkFields bEvent = do
    nameBox        <- getTextBox bEvent "txtBoxEvent"
    descriptionBox <- getTextBox bEvent "txtBoxDescription"
    categoryBox    <- getTextBox bEvent "txtBoxCategory"
    name           <- entryGetText nameBox
    description    <- entryGetText descriptionBox
    category       <- entryGetText categoryBox
    return $ name == "" || description == "" || category == ""


addEvent :: Builder -> Builder -> IO ()
addEvent bMain bEvento = do
    putStrLn "TEste!"

getFixed :: Builder -> String -> IO Fixed
getFixed b s = do
    builderGetObject b castToFixed s

getLabel :: Builder -> String -> IO Label
getLabel b s = do
    builderGetObject b castToLabel s

getTextBox :: Builder -> String -> IO Entry
getTextBox b s = do
    builderGetObject b castToEntry s

--Realiza um cast para capturar um determinado botão de um certo builder.
getButton :: Builder -> String -> IO Button
getButton b s = do
    builderGetObject b castToButton s


--Cria um builder para um terminado .glade e retorna seu monad.
makeBuilder :: String -> IO Builder
makeBuilder s = do
    builder <- builderNew
    builderAddFromFile builder s
    return builder

--Cria uma janela para adicionar 
createEvent :: Builder -> IO Dialog
createEvent b = do

    --Cria um novo widget para adição de evento.
    novoEvento <- builderGetObject b castToDialog "newEventWindow"

    -- CANCEL   ##################################################################
    -- Remove a janela de adição do novo evento. Quit.
    cancel  <- builderGetObject b castToButton "btnCancelar"
    onClicked cancel $ do widgetDestroy novoEvento
                          putStrLn "Adição cancelada."
    -- ###########################################################################

    -- Mostra o widget na tela.
    widgetShow novoEvento
    return novoEvento
--addEvent :: Window -> Dialog -> IO ()
--addEvent mW tA = "K"