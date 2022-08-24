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
import Data.Text

--Hashtable simples
type HashTable a b = H.BasicHashTable a b

makeHashTable :: IO (HashTable String Builder)
makeHashTable = do H.new

{--

EXEMPLO DE PRINT

printHash :: HashTable String String -> String -> IO ()
printHash ht s = do
    x <- H.lookup ht s
    H.insert ht "Nani" "x"
    putStrLn $ if x == Nothing then "Nothing" else "teste"
--}

--Devolve um monad de lista de Events do atual filtro salvo.
getCurrentFilteredList :: IO [Event]
getCurrentFilteredList = do
    prefs  <- getPrefs
    events <- getEvents
    filteredList events prefs

filteredList :: [Event] -> Prefs -> IO [Event]
filteredList events prefs = do
    let f      = defaultFilter      prefs
    let f_nome = pack $ defaultFilterName prefs
    let f_dia  = defaultFilterDay   prefs
    let f_mes  = defaultFilterMonth prefs
    let f_ano  = defaultFilterYear  prefs
    case f of
        1 -> return $ filterEventsByCat  f_nome events
        2 -> return $ filterEventName    f_nome events
        3 -> return $ filterCloseEvents  f_dia f_mes f_ano events
        4 -> return $ filterDistantEvets f_dia f_mes f_ano events
        5 -> return $ filterIsReg events
        6 -> getEvents
        _ -> getEvents

savePrefFilter :: Int -> String -> Int -> Int -> Int -> IO ()
savePrefFilter n s d m y = do
    prefs <- getPrefs
    let p_ad = autodelete prefs
    let p_ds = defaultSort prefs
    ok <- savePrefs (Prefs p_ad p_ds n s d m y)
    endDo

savePrefSort :: Int -> IO ()
savePrefSort n = do
    prefs <- getPrefs
    let p_ad = autodelete prefs
    let p_df = defaultFilter prefs
    let p_fn = defaultFilterName prefs
    let p_fd = defaultFilterDay prefs
    let p_fm = defaultFilterMonth prefs
    let p_fy = defaultFilterYear prefs
    ok <- savePrefs (Prefs p_ad n p_df p_fn p_fd p_fm p_fy)
    endDo

getSortedList :: [Event] -> IO [Event]
getSortedList es = do
    prefs <- getPrefs
    let n = defaultSort prefs
    case n of
        1 -> return $ sortEventsBy "name" es
        2 -> return $ sortEventsBy "date" es
        3 -> return $ sortEventsBy "description" es
        4 -> return $ sortEventsBy "recurrent" es
        5 -> return es
        _ -> return es

getProcessedList :: IO [Event]
getProcessedList = do
    l <- getCurrentFilteredList
    getSortedList l

setSortLabel :: Builder -> IO ()
setSortLabel builder = do
    prefs <- getPrefs
    let n = defaultSort prefs
    setLabelText builder "lbl_Sort" ("Ordenação: " ++ getSortByInt n)
    endDo

getSortByInt :: Int -> String
getSortByInt n =
    case n of
        1 -> "por nome."
        2 -> "por data."
        3 -> "por descrição."
        4 -> "por recorrência."
        5 -> "default."
        _ -> ""

setFilterLabel :: Builder -> IO ()
setFilterLabel builder = do
    prefs <- getPrefs
    let n = defaultFilter prefs
    let p_fn = defaultFilterName prefs
    let p_fd = defaultFilterDay prefs
    let p_fm = defaultFilterMonth prefs
    let p_fy = defaultFilterYear prefs
    setLabelText builder "lbl_Filter" ("Filtro: " ++ getFilterByInt n p_fn p_fd p_fm p_fy)
    endDo

getFilterByInt :: Int -> String -> Int -> Int -> Int -> String
getFilterByInt n s x y z=
    case n of
        1 -> "por categoria (" ++ s ++ ")."
        2 -> "por nome (" ++ s ++ ")."
        3 -> "por eventos anteriores à data " ++ show x ++ "/" ++ show y ++ "/" ++ show z ++ "."
        4 -> "por evento posteriores à data " ++ show x ++ "/" ++ show y ++ "/" ++ show z ++ "."
        5 -> "por eventos recorrentes."
        6 -> "padrão (sem filtro)."
        _ -> ""

main :: IO ()
main = do
    initGUI


    --Builder para janela principal.
    builderMain <- makeBuilder "./ui/UI_main.glade"

    --Janela principal, que comportará todos os outros elementos.
    window <- builderGetObject builderMain castToWindow "mainWindow"

    --Switcher comporta todas as categorias que, por sua vez, contém os respectivos eventos.
    switcher <- builderGetObject builderMain castToNotebook "switcherMain"
    logo <- builderGetObject builderMain castToImage "imgLogo"
    imageSetFromFile logo "./images/deadlines-RB.png"

    -- #IDEIA: ao ler o arquivo de categorias, chamar aqui a função create category para cada linha da tabela.
    --                  n <- createCategory "Nome da Categoria" categoriesMap switcher
    --                  talvez adicionar o parâmetro de cor? Depende de como podemos controlá-lo.
    -- Após adicionar todas as categorias, pegar o arquivo que lê todos os eventos, 
    -- ir de categoria em categoria adicionando-os às suas correspondentes categorias, usando insertEvent.
    
    --A hashtable pode ser passada como uma espécie de ponteiro. "Modificar" em outras funções altera esta própria variável.
    categoriesMap <- makeHashTable

    -- Posteriormente ler a tabela de categorias antes de criar os eventos.
    -- Criar função para refresh da data table.
    loadTable <- getProcessedList
    insertFromTable loadTable categoriesMap switcher
    setSortLabel builderMain
    setFilterLabel builderMain

    --Ideia para salvar e posteriormente carregar as configurações do usuário:
    --      Ter funções de sort por propriedade. Isto é, ter uma função que
    --      recebe uma lista, uma string que corresponde ao nome da coluna (ex: nome, data, etc)
    --      retorna a mesma lista só que organizada de acordo com a propriedade fornecida.
    --Na hora de popular a tabela, resta apenas passar o vetor organizado como parâmetro.
    

    --Para criar e adicionar uma nova categoria, basta chamar a função createCategory.
    --Tal função espera como parâmetro o nome da categoria, a hashtable e o parent.
    --Exemplo para criar a categoria:
    --n <- createCategory "Minha categoria 1" categoriesMap switcher

   -- window' <- createCompromisso "compromisso a" "data a" "tempo restante a" "dia da semana a" "descrição a"
   -- window'' <- createCompromisso "compromisso b" "data b" "tempo restante b" "dia da semana b" "descrição b"
    --hbox <- builderGetObject builder castToVScrollbar "vscrollbar"

    -- ADD EVENT ###################################################################################
    novo <- getAction builderMain "action_novoEvento"
    onActionActivate novo $ do bdAdd <- makeBuilder "./ui/UI_novoEvento.glade"
                               newEvent <- createEvent bdAdd
                               -- CONFIRM  ##################################################################
                               -- Adiciona o novo evento a uma categoria já existente.
                               -- Caso a categoria não exista, ela é criada e este novo evento é inserido.
                               confirm     <- getButton bdAdd "btnConfirmar"
                               bRecurrent  <- builderGetObject bdAdd castToToggleButton "recurrentCheckbox"
                               bRegularity <- builderGetObject bdAdd castToHPaned "regularityPanel"
                               onClicked confirm $ do notValid <- checkFields bdAdd
                                                      if notValid
                                                        then generateWarningMessage "Preencha todos os campos."
                                                        else do
                                                            addEvent builderMain bdAdd switcher categoriesMap
                                                            updatedList <- getProcessedList
                                                            refreshEvents updatedList categoriesMap switcher
                                                            widgetDestroy newEvent
                                                            endDo
                               onClicked bRecurrent $ do isVisible <- widgetGetVisible bRegularity 
                                                         if isVisible then widgetHide bRegularity  else widgetShow bRegularity
                                                         endDo
                               endDo
                          -- #########################################################################
    -- ###############################################################################################

    -- ORDENAÇÃO #####################################################################################

    -- Ordenar por nome
    action_sort_name <- getAction builderMain "ordenar_Nome"
    onActionActivate action_sort_name $ do updatedList <- getCurrentFilteredList
                                           refreshEvents (sortEventsBy "name" updatedList) categoriesMap switcher
                                           savePrefSort 1
                                           setSortLabel builderMain
    
    -- Ordenar por data
    action_sort_date <- getAction builderMain "ordenar_Data"
    onActionActivate action_sort_date $ do updatedList <- getCurrentFilteredList
                                           refreshEvents (sortEventsBy "date" updatedList) categoriesMap switcher
                                           savePrefSort 2
                                           setSortLabel builderMain

    -- Ordenar por descrição
    action_sort_desc <- getAction builderMain "ordenar_Descricao"
    onActionActivate action_sort_desc $ do updatedList <- getCurrentFilteredList
                                           refreshEvents (sortEventsBy "description" updatedList) categoriesMap switcher
                                           savePrefSort 3
                                           setSortLabel builderMain
    -- Ordenar por recorrência
    action_sort_rec <- getAction builderMain "ordenar_Recorrencia"
    onActionActivate action_sort_rec $ do updatedList <- getCurrentFilteredList
                                          refreshEvents (sortEventsBy "recurrent" updatedList) categoriesMap switcher
                                          savePrefSort 4
                                          setSortLabel builderMain

    -- Volta a lista para a ordenação padrão (ordem de inserção do usuário)
    action_sort_default <- getAction builderMain "ordenar_None"
    onActionActivate action_sort_default $ do updatedList <- getCurrentFilteredList
                                              refreshEvents updatedList categoriesMap switcher
                                              savePrefSort 5
                                              setSortLabel builderMain

    -- FILTROS ######################################################################################

    -- Filtrar por categoria
    action_filter_category <- getAction builderMain "Action_Filtro_Categoria"
    onActionActivate action_filter_category $ do createFilterDialogText 1 categoriesMap switcher
                                                 setFilterLabel builderMain

    -- Filtrar por nome do evento
    action_filter_name <- getAction builderMain "Action_Filtro_Nome"
    onActionActivate action_filter_name $ do createFilterDialogText 2 categoriesMap switcher
                                             setFilterLabel builderMain

    -- Filtrar por evento mais próximo
    action_filter_closer <- getAction builderMain "Action_Filtro_MaisProx"
    onActionActivate action_filter_closer $ do createFilterDialogDate 1 categoriesMap switcher
                                               setFilterLabel builderMain

    -- Filtrar por evento mais distante
    action_filter_further <- getAction builderMain "Action_Filtro_MaisDist"
    onActionActivate action_filter_further $ do createFilterDialogDate 2 categoriesMap switcher
                                                setFilterLabel builderMain

    -- Filtrar eventos que possuem descrição
    action_filter_description <- getAction builderMain "Action_Filtro_Desc"
    onActionActivate action_filter_description $ do updatedTable <- getEvents
                                                    refreshEvents (filterHasDesc updatedTable) categoriesMap switcher
                                                    setFilterLabel builderMain

    -- Filtrar eventos que são recorrentes
    action_filter_recurrency <- getAction builderMain "Action_Filtro_Recorrencia"
    onActionActivate action_filter_recurrency $ do updatedTable <- getEvents
                                                   refreshEvents (filterIsReg updatedTable) categoriesMap switcher
                                                   savePrefFilter 5 "" 0 0 0
                                                   setFilterLabel builderMain

    -- Remover filtros
    action_filter_remove <- getAction builderMain "Action_Filtro_Remover"
    onActionActivate action_filter_remove $ do updatedTable <- getEvents
                                               refreshEvents updatedTable categoriesMap switcher
                                               savePrefFilter 6 "" 0 0 0
                                               setFilterLabel builderMain

    -- ##############################################################################################

    -- OUTROS BOTÕES DO MENU SUPERIOR ###############################################################
    action_sair <- getAction builderMain "action_Sair"
    onActionActivate action_sair $ do liftIO mainQuit >> return False
                                      endDo
    -- ##############################################################################################

    --Mostra todos os widgets presentes em window.
    widgetShowAll window
    on window deleteEvent $ liftIO mainQuit >> return False

    mainGUI


endDo :: IO ()
endDo = do putStr ""

--Int corresponde ao tipo de filtro. 1 filtra eventos mais próximos, 2 filtra eventos mais distantes.
createFilterDialogDate :: Int -> HashTable String Builder -> Notebook -> IO ()
createFilterDialogDate n ht switcher = do
    builder <- makeBuilder "./ui/UI_FilterDialogDate.glade"
    window <- builderGetObject builder castToDialog "mainDialog"
    btnCancelar  <- getButton builder "btn_cancelar"
    btnConfirmar <- getButton builder "btn_confirmar"

    dia' <- getSpin builder "spin_dia"
    mes' <- getSpin builder "spin_mes"
    ano' <- getSpin builder "spin_ano"

    if n == 1 
        then setLabelText builder "lblTitulo" "Filtrar eventos anteriores a: "
        else setLabelText builder "lblTitulo" "Filtrar eventos posteriores a:"
    
    --Botão cancelar.
    onClicked btnCancelar $ do widgetDestroy window

    onClicked btnConfirmar $ do updatedTable <- getEvents
                                vDia <- spinButtonGetValue dia'
                                vMes <- spinButtonGetValue mes'
                                vAno <- spinButtonGetValue ano'
                                let vDia' = fromInteger $ truncate vDia
                                let vMes' = fromInteger $ truncate vMes
                                let vAno' = fromInteger $ truncate vAno
                                if n == 1
                                    then do
                                        refreshEvents (filterCloseEvents  vDia' vMes' vAno' updatedTable) ht switcher
                                        savePrefFilter 3 "" vDia' vMes' vAno'
                                    else do
                                        refreshEvents (filterDistantEvets vDia' vMes' vAno' updatedTable) ht switcher
                                        savePrefFilter 4 "" vDia' vMes' vAno'
                                widgetDestroy window


    widgetShow window
    endDo


--Int corresponde ao tipo de filtro. 1 filtra categoria, 2 filtra nome.
createFilterDialogText :: Int -> HashTable String Builder -> Notebook -> IO ()
createFilterDialogText n ht switcher = do
    builder <- makeBuilder "./ui/UI_FilterDialog.glade"
    window <- builderGetObject builder castToDialog "mainDialog"
    btnCancelar  <- getButton builder "btn_cancelar"
    btnConfirmar <- getButton builder "btn_confirmar"

    if n == 1 
        then setLabelText builder "lblTitulo" "Categoria:"
        else setLabelText builder "lblTitulo" "Eventos que contém o seguinte texto:"
    
    --Botão cancelar.
    onClicked btnCancelar $ do widgetDestroy window

    onClicked btnConfirmar $ do text <- getTextFromEntry builder "filterText"
                                updatedTable <- getEvents
                                if n == 1
                                    then do
                                        refreshEvents (filterEventsByCat (pack text) updatedTable) ht switcher
                                        savePrefFilter 1 text 0 0 0
                                    else do
                                        refreshEvents (filterEventName   (pack text) updatedTable) ht switcher
                                        savePrefFilter 2 text 0 0 0
                                widgetDestroy window


    widgetShow window
    endDo


refreshEvents :: [Event] -> HashTable String Builder -> Notebook -> IO ()
refreshEvents es ht switcher = do
    ht <- makeHashTable --Recria a hashtable
    children <- containerGetChildren switcher
    clearSwitcher children
    insertFromTable es ht switcher
    endDo

clearSwitcher :: [Widget] -> IO ()
clearSwitcher []     = do endDo
clearSwitcher (x:xs) = do
    widgetDestroy x
    clearSwitcher xs

--Cria uma categoria com um dado nome s. Adiciona-a na HashTable. Adiciona-o ao parent.
createCategory :: String -> HashTable String Builder -> Notebook-> IO Builder
createCategory s ht parent = do
    categoryBuilder <- makeBuilder "./ui/UI_Categoria.glade"
    fixedBox <- getFixed categoryBuilder "mainFixed"
    lblCategory <- getLabel categoryBuilder "lblCategoryTitle"
    colorButton <- builderGetObject categoryBuilder castToColorButton "btnColor"
    background <- builderGetObject categoryBuilder castToEventBox "eventbox1"
    background' <- builderGetObject categoryBuilder castToViewport "viewport1"
    labelSetText lblCategory s
    H.insert ht s categoryBuilder
    notebookAppendPage parent fixedBox s


    onColorSet colorButton $ do color <- colorButtonGetColor colorButton
                                widgetModifyBg background StateNormal color
                                widgetModifyBg background' StateNormal color
                                putStrLn $ "Cor da categoria alterada com sucesso. " ++ show color

    return categoryBuilder

--Retorna o builder de uma categoria. Assim, pode-se ter acesso a todos os seus elementos.
getCategory :: String -> HashTable String Builder -> IO Builder
getCategory s ht = do
    value <- H.lookup ht s
    case value of
        Just x -> return x
        Nothing -> error "Referência de categoria não encontrada."

--Verifica se uma dada categoria existe.
categoryExists :: String -> HashTable String Builder -> IO Bool
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
    category       <- entryGetText categoryBox
    return $ name == "" || category == ""


--Esta função deve receber:
--  O builder do createEvent, que é de onde serão capturados os textos inseridos pelo usuário nas text box.
--  O builder da main page. Caso a categoria não exista, ela precisará ser criada.
--  A hashTable para que seja possível pegar a referência da categoria, caso exista. 
addEvent :: Builder -> Builder -> Notebook -> HashTable String Builder -> IO ()
addEvent bMain bEvento switcher ht = do
    categoria    <- getTextFromEntry bEvento "txtBoxCategory"
    eventForm    <- toEventForm bEvento
    --Adicionar aqui função para salvar eventForm no csv.
    Lib.insertEvent eventForm
    isValid      <- categoryExists categoria ht
    if not isValid
        then do
            --Caso não exista a categoria, criamos uma nova categoria.
            bCategoria <- createCategory categoria ht switcher
            Main.insertEvent eventForm ht switcher
            putStrLn "Categoria criada."
            --Adicionamos dentro desta categoria o novo evento.
            endDo
        else do
            --A categoria já existe. Basta que se pegue a referência e adicionemos o evento à vertical box.
            Main.insertEvent eventForm ht switcher
            putStrLn ""

--Pega um builder e retorna os campos na forma de evento.
toEventForm :: Builder -> IO Event
toEventForm builder = do
    eName        <- getTextFromEntry builder "txtBoxEvent"
    eDescription <- getTextFromEntry builder "txtBoxDescription"
    eCategory    <- getTextFromEntry builder "txtBoxCategory"
    eRecurrent   <- builderGetObject builder castToToggleButton "recurrentCheckbox"
    isRecurrent  <- toggleButtonGetActive eRecurrent
    eRegularity  <- getSpin builder "regularity"
    vRegularity <- spinButtonGetValue eRegularity
    let valueRec = fromInteger $ truncate vRegularity

    -- Calendar settings.
    eCalendar    <- builderGetObject builder castToCalendar "calendar"
    cFormat      <- calendarGetDate eCalendar
    eDay         <- getDate cFormat 0
    eMonth       <- getDate cFormat 1
    eYear        <- getDate cFormat 2


    --Adicionar aqui as outras variáveis.
    --Month tem um retorno de 0 a 11. Logo foi ajustado para 1 a 12.
    return $ Item (pack eName) eDay (eMonth+1) eYear (pack eDescription) (pack eCategory) isRecurrent valueRec CatName

-- createDescFilterDialog :: [Event] -> IO ()
-- createFilterDialog es = do


--Retorna o dia, mês ou ano dependendo do parâmetro passado. 0 => dia, 1 => mês, 2 => ano.
getDate :: (Int, Int, Int) -> Int -> IO Int
getDate (_, _, d) 0 = return d
getDate (_, m, _) 1 = return m
getDate (y, _, _) 2 = return y
getDate (_, _, _) _ = return $ -1 --Esta ação nunca ocorrerá.

getYear :: (Integer, Int, Int) -> Int
getYear (a, _, _) = fromInteger a

getMonth :: (Integer, Int, Int) -> Int
getMonth (_, a, _) = a

getDay :: (Integer, Int, Int) -> Int
getDay (_, _, a) = a

getTextFromEntry :: Builder -> String -> IO String
getTextFromEntry builder s = do
    entryRef <- getTextBox builder s
    entryGetText entryRef

getMonthName :: Int -> String
getMonthName x = case x of
    1  -> "Janeiro"
    2  -> "Feveiro"
    3  -> "Março"
    4  -> "Abril"
    5  -> "Maio"
    6  -> "Junho"
    7  -> "Julho"
    8  -> "Agosto"
    9  -> "Setembro"
    10 -> "Outubro"
    11 -> "Novembro"
    12 -> "Dezembro"
    _  -> ""

insertFromTable :: [Event] -> HashTable String Builder -> Notebook -> IO ()
insertFromTable [] _ _            = endDo
insertFromTable (x:xs) ht switcher = do
    let nCategory = unpack $ category x
    isValid <- categoryExists nCategory ht
    if not isValid
        then do
            cat <- createCategory nCategory ht switcher
            Main.insertEvent x ht switcher
            endDo
        else Main.insertEvent x ht switcher
    insertFromTable xs ht switcher

--Pega um evento e, através do seu campo de categoria, pega a referência do builder do widget correspondente.
--Chame este evento para adicionar um Event a uma dada categoria.
insertEvent :: Event -> HashTable String Builder -> Notebook -> IO ()
insertEvent event ht switcher = do
    --Captura todas as informações relevantes para construir o widget.
    let nName        = unpack $ name event
    let nDay         = day event
    let nMonth       = month event
    let nCategory    = unpack $ category event
    let nYear        = year event
    let nDescription = unpack $ description event
    let nRecurrent   = recurrent event
    let nRegularity  = regularity  event
    let nColor       = Category --Alterar color depois.

    -------------------------------------------------------------------
    categoriaBuilderRef <- getCategory nCategory ht --Pega o builder da categoria.
    categoriesBox <- getVerticalBox categoriaBuilderRef "categoriesBox"

    --Cria uma nova linha de evento para visualização do usuário.
    bLinhaEvento <- makeBuilder "./ui/UI_Evento.glade"
    newEvento    <- getFixed bLinhaEvento "fixedMain"

    --Ajuste das labels: preenche nome do evento, descrição, dia da semana, data e tempo restante.
    setLabelText bLinhaEvento "lblEvent" (nName ++ (if nRecurrent then " (recorrente: a cada " ++ show nRegularity  ++ " dias)" else ""))
    setLabelText bLinhaEvento "lblDay" (show nDay ++ " de " ++ getMonthName nMonth ++ " de " ++ show nYear)
    setLabelText bLinhaEvento "lblDescription" nDescription
    setLabelText bLinhaEvento "lblWeekDay" (dateToWeekDay nDay nMonth nYear)
    hoje <- getToday
    setLabelText bLinhaEvento "lblTimeRemaining" (show (daysleft event hoje) ++ " dias")

    --BOTÕES ----------------------------------------------------
    btnEditar <- getButton bLinhaEvento "btnEditar"
    onClicked btnEditar $ do bEdit <- makeBuilder "./ui/UI_editarEvento.glade"
                             bEditWindow <- builderGetObject bEdit castToDialog "editEventWindow"
                             setTextBoxText bEdit "txtBoxEvent" nName
                             setTextBoxText bEdit "txtBoxDescription" nDescription
                             setTextBoxText bEdit "txtBoxCategory" nCategory
                             widgetShow bEditWindow

                             btnEdit <- getButton bEdit "btnEditar"
                             onClicked btnEdit $ do deleteEventFromFile nName
                                                    editedEvent <- toEventForm bEdit
                                                    Lib.insertEvent editedEvent
                                                    updatedList <- getProcessedList
                                                    refreshEvents updatedList ht switcher
                                                    widgetDestroy bEditWindow
                                                    b <- containerGetChildren categoriesBox

                                                    if Prelude.length b == 0
                                                        then do page <- getFixed categoriaBuilderRef "mainFixed"
                                                                widgetDestroy page
                                                                --Adicionar aqui o evento para remover a CATEGORIA do csv.
                                                                endDo
                                                        else endDo


                             btnCancelar <- getButton bEdit "btnCancelar"
                             onClicked btnCancelar $ do widgetDestroy bEditWindow

                             endDo

    btnExcluir <- getButton bLinhaEvento "btnExcluir"
    onClicked btnExcluir $ do widgetDestroy newEvento
                              --Adicionar aqui o evento para remover o EVENT do csv.
                              b <- containerGetChildren categoriesBox
                              _ <- deleteEventFromFile nName
                              if Prelude.length b == 0
                                then do page <- getFixed categoriaBuilderRef "mainFixed"
                                        widgetDestroy page
                                        putStrLn ("Evento " ++ nName ++ " e categoria "++ nCategory ++ " removidos com sucesso.")
                                        --Adicionar aqui o evento para remover a CATEGORIA do csv.
                                        endDo
                                else putStrLn ("Evento " ++ nName ++ " removido com sucesso.")
    -------------------------------------------------------------

    --Adiciona o evento ao vertical box.
    widgetReparent newEvento categoriesBox

    endDo


setTextBoxText :: Builder -> String -> String -> IO ()
setTextBoxText builder txtbox texto = do
    textBox <- builderGetObject builder castToEntry txtbox
    entrySetText textBox texto
    endDo

--A primeira string corresponde ao nome do label e a segunda, ao novo texto que o label terá.
setLabelText :: Builder -> String -> String -> IO ()
setLabelText builder lbl lblText = do
    label <- getLabel builder lbl
    labelSetText label lblText
    endDo

getSpin :: Builder -> String -> IO SpinButton
getSpin b s = do
    builderGetObject b castToSpinButton s

getAction :: Builder -> String -> IO Action
getAction b s = do
    builderGetObject b castToAction s

getVerticalBox :: Builder -> String -> IO VBox
getVerticalBox b s = do
    builderGetObject b castToVBox s

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
    novoEvento   <- builderGetObject b castToDialog "newEventWindow"
    calendario   <- builderGetObject b castToCalendar "calendar"
    hoje         <- getToday
    --O calendar possui meses de 0 a 11. Por isso o -1 do getMonth.
    calendarSelectMonth calendario (getMonth hoje - 1) (getYear hoje)
    calendarSelectDay calendario (getDay hoje)

    -- CANCEL   ##################################################################
    -- Remove a janela de adição do novo evento. Quit.
    cancel  <- builderGetObject b castToButton "btnCancelar"
    onClicked cancel $ do widgetDestroy novoEvento
                          putStrLn "Adição cancelada."
    -- ###########################################################################

    -- Mostra o widget na tela.
    widgetShow novoEvento
    return novoEvento