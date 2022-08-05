{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GI.Gtk as Gtk

main :: IO ()
main = do
    Gtk.init Nothing

    window <- Gtk.windowNew Gtk.WindowTypeToplevel
    Gtk.setContainerBorderWidth window 10

    Gtk.setWindowTitle window "deadlines"

    Gtk.setWindowDefaultWidth   window 750
    Gtk.setWindowDefaultHeight  window 300

    Gtk.onWidgetDestroy window Gtk.mainQuit


    #showAll window
    Gtk.main

