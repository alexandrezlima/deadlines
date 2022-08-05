{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import GI.Gtk as Gtk

main :: IO ()
main = do
    Gtk.init Nothing

    window <- Gtk.windowNew Gtk.WindowTypeToplevel
    Gtk.setContainerBorderWidth window 10

    Gtk.setWindowTitle window "deadlines"

    Gtk.setWindowDefaultWidth   window 750
    Gtk.setWindowDefaultHeight  window 300

    Gtk.onWidgetDestroy window Gtk.mainQuit



    image1 <- Gtk.imageNewFromFile $ "./images/discordqr.png"

    btn1 <- Gtk.buttonNew
    Gtk.buttonSetImage btn1  $ Just image1
    Gtk.buttonSetRelief btn1 Gtk.ReliefStyleNone
    Gtk.widgetSetHexpand btn1 False --evita que haja expansão vertical do botão (height)
    Gtk.on btn1 #clicked $ do
                        putStrLn "Teste!"
                        Gtk.widgetDestroy window


    grid <- Gtk.gridNew
    Gtk.gridSetColumnSpacing grid 10
    Gtk.gridSetRowSpacing grid 10
    Gtk.gridSetColumnHomogeneous grid True
                    --os primeiros 2 valores são as coordenadas x e y.
                    --os segundos 2 valores são quantas linhas e colunas o widget deve ocupar.
    #attach grid btn1 0 0 1 1

    #add window grid


    #showAll window
    Gtk.main

-- attacTo :: 
