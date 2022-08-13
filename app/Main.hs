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

{- 
    Requer um builder, que tem a referência do .glade.
    A primeira string corresponde ao nome da label.
    A segunda string corresponde ao novo nome da label.
    Retorna uma IO Label
-}
getLabelObject :: Builder -> String -> String -> IO Label
getLabelObject b s n = do
    label <- builderGetObject b castToLabel s
    labelSetLabel label n
    return label

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

main :: IO ()
main = do
    initGUI
    builderMain <- makeBuilder "./ui/UI_main.glade"
    window <- builderGetObject builderMain castToWindow "mainWindow"

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
                               onClicked confirm $ do --addEvent bdAdd 
                                                      putStrLn "Novo evento adicionado."
                               putStr ""
                          -- ###########################################################################
    -- ##############################################################################################

    --Exemplo de como capturar a ação de um botão do submenu. Note que no glade tivemos que associar uma nova ação ao item.
    action <- builderGetObject builderMain castToAction "action1"
    onActionActivate action $ do (putStrLn "test")

    --Mostra todos os widgets presentes em window.
    widgetShowAll window
    on window deleteEvent $ liftIO mainQuit >> return False

    mainGUI

addEvent :: Builder -> IO ()
addEvent b = undefined


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