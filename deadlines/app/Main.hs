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


createCompromisso :: String -> String -> String -> String -> String -> IO Frame
createCompromisso a b c d e = do
    builder <- builderNew
    builderAddFromFile builder "./ui/ui_compromisso.glade"
    title           <- getLabelObject builder "titulo"          a
    date            <- getLabelObject builder "data"            b
    remainingTime   <- getLabelObject builder "temporestante"   c
    weekDay         <- getLabelObject builder "diadasemana"     d
    description     <- getLabelObject builder "descricao"       e
    --button <- builderGetObject builder castToButton "button1"
    --onClicked button $ do putStrLn k

    builderGetObject builder castToFrame "frame1"
    where
        k = a ++ " test"

main :: IO ()
main = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder "./ui/MainUI.glade"
    window <- builderGetObject builder castToWindow "mainWindow"


   -- window' <- createCompromisso "compromisso a" "data a" "tempo restante a" "dia da semana a" "descrição a"
   -- window'' <- createCompromisso "compromisso b" "data b" "tempo restante b" "dia da semana b" "descrição b"
    --hbox <- builderGetObject builder castToVScrollbar "vscrollbar"

    button <- builderGetObject builder castToButton "button1"

    onClicked button $ do addCompromisso

    widgetShowAll window

    --Exemplo de como capturar a ação de um botão do submenu. Note que no glade tivemos que associar uma nova ação ao item.
    action <- builderGetObject builder castToAction "action1"
    onActionActivate action $ do (putStrLn "test")


    on window deleteEvent $ liftIO mainQuit >> return False

    mainGUI

addCompromisso :: IO ()
addCompromisso = do
    builder <- builderNew
    builderAddFromFile builder "./ui/newCompromisso.glade"
    novoCompromisso <- builderGetObject builder castToDialog "newCompromissoWindow"
    widgetShow novoCompromisso