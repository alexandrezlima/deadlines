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
import qualified Data.HashTable.IO as H
import Data.Maybe
import Data.Text

--Estrutura de uma hashtable simples.
type HashTable a b = H.BasicHashTable a b

--Cria uma hashtable cuja key é uma string e o valor é um Builder.
--Durante este programa, este builder corresponde aos widgets de categorias.
makeHashTable :: IO (HashTable String Builder)
makeHashTable = do H.new

main :: IO ()
main = do
    --Inicializa a interface gráfica.
    initGUI


    --Cria um builder para janela principal.
    builderMain <- makeBuilder "./ui/UI_main.glade"

    --Referência da janela principal, que comportará todos os outros elementos.
    window <- builderGetObject builderMain castToWindow "mainWindow"

    --Referência do switcher, que comporta todas as categorias e que, por sua vez, contém os respectivos eventos.
    switcher <- builderGetObject builderMain castToNotebook "switcherMain"

    --Logo do canto inferior direito.
    logo <- builderGetObject builderMain castToImage "imgLogo"
    imageSetFromFile logo "./images/deadlines-RB.png"

    --Cria a hashtable.
    categoriesMap <- makeHashTable

    --Remove os eventos expirados (caso o usuário tenha habilitado a opção "autodelete").
    removeExpiredEvents

    --Pega a tabela processada, isto é, caso já tenha filtro e ordenação previamente selecionados.
    loadTable <- getProcessedList

    --Tendo a lista de eventos, insere cada evento em sua respectiva categoria.
    insertFromTable loadTable categoriesMap switcher

    --Atualiza os labels de ordenação, filtro e autodelete no canto inferior esquerdo.
    setSortLabel builderMain
    setFilterLabel builderMain
    setAutodeleteLabel builderMain


    -- ADD EVENT ###################################################################################
    -- O botão "Adicionar evento" cria um novo widget, neste caso uma nova janela, em que o usuário
    -- pode adicionar os detalhes de seu compromisso.
    novo <- getAction builderMain "action_novoEvento"
    onActionActivate novo $ do bdAdd <- makeBuilder "./ui/UI_novoEvento.glade"
                               newEvent <- createEvent bdAdd
                               -- CONFIRM  ##################################################################
                               -- Caso o usuário clique em "Adicionar", algumas verificações são realizadas.
                               confirm     <- getButton bdAdd "btnConfirmar"
                               bRecurrent  <- builderGetObject bdAdd castToToggleButton "recurrentCheckbox"
                               bRegularity <- builderGetObject bdAdd castToHPaned "regularityPanel"
                               onClicked confirm $ do notValid <- checkFields bdAdd
                                                      if notValid --Verifica-se se os campos necessários foram preenchidos.
                                                        --Caso "Evento" e "Categoria" estejam vazios, um warning é exibido.
                                                        then generateWarningMessage "Preencha os campos 'Evento' e 'Categoria' ."
                                                        else do
                                                            existsEv <- eventExists bdAdd
                                                            if not existsEv --Caso ambos os campos anteriormente mencionados tenham sido preenchidos.
                                                                --Verifica-se se o evento já existe. Em caso negativo, o evento é criado e adicionado.
                                                                then do
                                                                    addEvent builderMain bdAdd switcher categoriesMap
                                                                    updatedList <- getProcessedList
                                                                    refreshEvents updatedList categoriesMap switcher
                                                                    widgetDestroy newEvent --Remove a janela de criação de evento.
                                                                --Caso contrário, um warning é apresentado indicando que o evento já existe.
                                                                else generateWarningMessage "Evento já existente."
                               --Dentro da janela de criação de evento, o toggle "regularidade" exibe um spin box onde o usuário
                               --pode escolher a regularidade do evento que está criando.
                               onClicked bRecurrent $ do isVisible <- widgetGetVisible bRegularity
                                                         if isVisible then widgetHide bRegularity  else widgetShow bRegularity
                                                         endDo
                               endDo
                          -- #########################################################################
    -- ###############################################################################################

    -- ORDENAÇÃO #####################################################################################
    {-
    Cada um dos eventos de ordenação pega uma lista com os eventos com filtro aplicado (caso tenha filtro
    aplicado) e, sobre essa lista, aplica uma função de ordenação. Em seguida, salva no CSV a preferência
    de ordenação e atualiza o label correspondente no canto inferior esquerdo.
    -}
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

    -- Volta a lista para a ordenação padrão (ordem de inserção do usuário).
    action_sort_default <- getAction builderMain "ordenar_None"
    onActionActivate action_sort_default $ do updatedList <- getCurrentFilteredList
                                              refreshEvents updatedList categoriesMap switcher
                                              savePrefSort 5
                                              setSortLabel builderMain

    -- FILTROS ######################################################################################
    {-
    Cada um dos eventos de filtragem pega uma lista de todos os eventos, aplica a função de filtragem e,
    depois, a de ordenação (caso haja alguma ordenação selecionada). Salva a preferência de filtragem no
    CSV e atualiza o label correspondente no canto inferior esquerdo.
    -}
    -- Filtrar por categoria
    action_filter_category <- getAction builderMain "Action_Filtro_Categoria"
    onActionActivate action_filter_category $ do createFilterDialogText 1 categoriesMap switcher builderMain

    -- Filtrar por nome do evento
    action_filter_name <- getAction builderMain "Action_Filtro_Nome"
    onActionActivate action_filter_name $ do createFilterDialogText 2 categoriesMap switcher builderMain

    -- Filtrar por evento mais próximo
    action_filter_closer <- getAction builderMain "Action_Filtro_MaisProx"
    onActionActivate action_filter_closer $ do createFilterDialogDate 1 categoriesMap switcher builderMain

    -- Filtrar por evento mais distante
    action_filter_further <- getAction builderMain "Action_Filtro_MaisDist"
    onActionActivate action_filter_further $ do createFilterDialogDate 2 categoriesMap switcher builderMain

    -- Filtrar eventos que possuem descrição
    action_filter_description <- getAction builderMain "Action_Filtro_Desc"
    onActionActivate action_filter_description $ do updatedTable <- getEvents
                                                    refreshEvents (filterHasDesc updatedTable) categoriesMap switcher
                                                    savePrefFilter 7 "" 0 0 0
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
    -- Botão "Quit" no menu superior. Fecha o programa.
    action_sair <- getAction builderMain "action_Sair"
    onActionActivate action_sair $ do liftIO mainQuit >> return False
                                      endDo

    --Botão "autodelete". Faz com que o autodelete seja ativado ou desativado (exclui eventos cuja data está expirada).
    action_autodelete <- getAction builderMain "action_Autodelete"
    onActionActivate action_autodelete $ do autodel <- getAutodelete
                                            print autodel
                                            saveAutodelete (not autodel)
                                            updatedList <- getProcessedList
                                            refreshEvents updatedList categoriesMap switcher
                                            setAutodeleteLabel builderMain
                                            endDo
    -- ##############################################################################################

    --Mostra todos os widgets presentes em "window".
    widgetShowAll window
    on window deleteEvent $ liftIO mainQuit >> return False
    mainGUI

--Função para facilitar o IO () dos "do" blocks que precisam retornar IO ().
endDo :: IO ()
endDo = do putStr ""


--Devolve um monad de lista de Events com o atual filtro aplicado. Lê qual é o filtro salvo no CSV "Prefs".
getCurrentFilteredList :: IO [Event]
getCurrentFilteredList = do
    prefs  <- getPrefs
    print prefs
    events <- getEvents
    print events
    filteredList events prefs

--Função auxiliar que devolve uma lista filtrada dada uma preferência. Aplica diretamente a função de filtragem.
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

--Salva no CSV "Prefs" qual é o filtro atualmente selecionado.
-- Int, String, Int, Int, Int são, respectivamente:
-- Número do filtro selecionado, qual o nome utilizado para filtrar (nome do evento ou descrição),
-- Dia, mês e ano.
savePrefFilter :: Int -> String -> Int -> Int -> Int -> IO ()
savePrefFilter n s d m y = do
    prefs <- getPrefs
    print prefs
    let p_ad = autodelete prefs
    let p_lt = lastCategoryTab prefs
    let p_ds = defaultSort prefs
    savePrefs (Prefs p_ad p_lt p_ds n s d m y)
    endDo

--Salva no CSV "Prefs" qual é a ordenação atualmente selecionada.
-- "n" indica o número da respectiva ordenação.
savePrefSort :: Int -> IO ()
savePrefSort n = do
    prefs <- getPrefs
    let p_ad = autodelete prefs
    let p_lt = lastCategoryTab prefs
    let p_df = defaultFilter prefs
    let p_fn = defaultFilterName prefs
    let p_fd = defaultFilterDay prefs
    let p_fm = defaultFilterMonth prefs
    let p_fy = defaultFilterYear prefs
    print prefs
    savePrefs (Prefs p_ad p_lt n p_df p_fn p_fd p_fm p_fy)
    endDo

--Salva no CSV "Prefs" a opção "Autodelete", podendo ela ser true ou false (parâmetro "b").
saveAutodelete :: Bool -> IO ()
saveAutodelete b = do
    prefs <- getPrefs
    let p_df = defaultFilter prefs
    let p_lt = lastCategoryTab prefs
    let p_ds = defaultSort prefs
    let p_fn = defaultFilterName prefs
    let p_fd = defaultFilterDay prefs
    let p_fm = defaultFilterMonth prefs
    let p_fy = defaultFilterYear prefs
    print prefs
    savePrefs (Prefs b p_lt p_ds p_df p_fn p_fd p_fm p_fy)
    if b then removeExpiredEvents else endDo

--Pega a lista de eventos cuja data limite não foi expirada e sobrescreve no CSV de eventos.
removeExpiredEvents :: IO ()
removeExpiredEvents = do
    events <- getEvents
    hoje <- getToday
    let k = [x | x <- events, daysleft x hoje >= 0] --Cria uma lista com todos os eventos com data negativa.
    print k
    encodeToFile eventPath k
    endDo

--Lê do CSV qual é a preferência de autodelete e devolve o Bool correspondente.
getAutodelete :: IO Bool
getAutodelete = do
    autodelete <$> getPrefs

--Dada uma lista de eventos, aplica a função de ordenação e devolve a lista organizada
--de acordo com o tipo de ordenação salva no CSV "Prefs".
getSortedList :: [Event] -> IO [Event]
getSortedList es = do
    prefs <- getPrefs
    print prefs
    let n = defaultSort prefs
    case n of
        1 -> return $ sortEventsBy "name" es
        2 -> return $ sortEventsBy "date" es
        3 -> return $ sortEventsBy "description" es
        4 -> return $ sortEventsBy "recurrent" es
        5 -> return es
        _ -> return es

--Retorna um monad de uma lista de eventos com filtro e ordenação aplicados.
getProcessedList :: IO [Event]
getProcessedList = do
    l <- getCurrentFilteredList
    getSortedList l

--Modifica a label de autodelete no canto inferior esquerdo.
setAutodeleteLabel :: Builder -> IO()
setAutodeleteLabel builder = do
    prefs <- getPrefs
    print prefs
    let b = autodelete prefs
    setLabelText builder "lbl_AutoDelete" ("Autodeletar datas atingidas: " ++ show b)
    endDo

--Modifica a label de ordenação no canto inferior esquerdo.
setSortLabel :: Builder -> IO ()
setSortLabel builder = do
    prefs <- getPrefs
    print prefs
    let n = defaultSort prefs
    setLabelText builder "lbl_Sort" ("Ordenação: " ++ getSortByInt n)
    endDo

--Modifica a label de filtragem no canto inferior esquerdo.
setFilterLabel :: Builder -> IO ()
setFilterLabel builder = do
    prefs <- getPrefs
    print prefs
    let n = defaultFilter prefs
    let p_fn = defaultFilterName prefs
    let p_fd = defaultFilterDay prefs
    let p_fm = defaultFilterMonth prefs
    let p_fy = defaultFilterYear prefs
    setLabelText builder "lbl_Filter" ("Filtro: " ++ getFilterByInt n p_fn p_fd p_fm p_fy)
    endDo

--Retorna qual o tipo de ordenação baseado em um número.
--Esta ordem foi definida de forma arbitrária.
getSortByInt :: Int -> String
getSortByInt n =
    case n of
        1 -> "por nome."
        2 -> "por data."
        3 -> "por descrição."
        4 -> "por recorrência."
        5 -> "default."
        6 -> "por categoria."
        _ -> ""

--Retorna qual o tipo de filtragem baseado em um número.
--Esta ordem foi definida de forma arbitrária.
--n é o número do filtro, s corresponde ao nome do evento ou à descrição,
--x o dia, y o mês e z, o ano.
getFilterByInt :: Int -> String -> Int -> Int -> Int -> String
getFilterByInt n s x y z=
    case n of
        1 -> "por categoria (" ++ s ++ ")."
        2 -> "por nome (" ++ s ++ ")."
        3 -> "por eventos anteriores à data " ++ show x ++ "/" ++ show y ++ "/" ++ show z ++ "."
        4 -> "por evento posteriores à data " ++ show x ++ "/" ++ show y ++ "/" ++ show z ++ "."
        5 -> "por eventos recorrentes."
        6 -> "default (sem filtro)."
        7 -> "por eventos que possuem descrição."
        _ -> ""

--Cria uma janela para escolher a data do filtro.
--O Int corresponde ao tipo de filtro: 1 filtra eventos anteriores à data, 2 filtra eventos posteriores à data.
createFilterDialogDate :: Int -> HashTable String Builder -> Notebook -> Builder -> IO ()
createFilterDialogDate n ht switcher builderMain = do
    builder <- makeBuilder "./ui/UI_FilterDialogDate.glade"
    window <- builderGetObject builder castToDialog "mainDialog"
    btnCancelar  <- getButton builder "btn_cancelar"
    btnConfirmar <- getButton builder "btn_confirmar"

    --Cria as referências dos spins.
    dia' <- getSpin builder "spin_dia"
    mes' <- getSpin builder "spin_mes"
    ano' <- getSpin builder "spin_ano"

    prefs <-getPrefs
    print prefs

    --Define a label baseado no tipo de filtro.
    if n == 1
        then setLabelText builder "lblTitulo" "Filtrar eventos anteriores a: "
        else setLabelText builder "lblTitulo" "Filtrar eventos posteriores a:"

    --Botão cancelar, fecha a janela.
    onClicked btnCancelar $ do widgetDestroy window

    --Aplica o filtro baseado na data escolhida.
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
                                setFilterLabel builderMain
                                widgetDestroy window

    --Mostra a janela.
    widgetShow window
    endDo


--Salva o nome da última categoria aberta.
saveLastTabPref :: String -> IO ()
saveLastTabPref s = do
    prefs <- getPrefs
    let b    = autodelete prefs
    let p_df = defaultFilter prefs
    let p_lt = pack s
    let p_ds = defaultSort prefs
    let p_fn = defaultFilterName prefs
    let p_fd = defaultFilterDay prefs
    let p_fm = defaultFilterMonth prefs
    let p_fy = defaultFilterYear prefs
    print prefs
    savePrefs (Prefs b p_lt p_ds p_df p_fn p_fd p_fm p_fy)
    endDo

--Carrega o nome da última categoria aberta.
getLastTabPref :: IO String
getLastTabPref = do
    (unpack . lastCategoryTab) <$> getPrefs



--Cria uma janela para escolher o nome do evento ou da categoria.
--Int corresponde ao tipo de filtro. 1 filtra categoria, 2 filtra nome.
createFilterDialogText :: Int -> HashTable String Builder -> Notebook -> Builder -> IO ()
createFilterDialogText n ht switcher builderMain = do
    builder <- makeBuilder "./ui/UI_FilterDialog.glade"
    window <- builderGetObject builder castToDialog "mainDialog"
    btnCancelar  <- getButton builder "btn_cancelar"
    btnConfirmar <- getButton builder "btn_confirmar"

    --Atualiza o label com o tipo de filtro correspondente.
    if n == 1
        then setLabelText builder "lblTitulo" "Categoria:"
        else setLabelText builder "lblTitulo" "Eventos que contém o seguinte texto:"

    prefs <- getPrefs
    print prefs

    --Botão cancelar. Fecha a janela.
    onClicked btnCancelar $ do widgetDestroy window

    --Botão confirmar, pega o texto inserido no campo e aplica o filtro correspondente (por categoria ou por nome do evento).
    onClicked btnConfirmar $ do text <- getTextFromEntry builder "filterText"
                                updatedTable <- getEvents
                                if n == 1
                                    then do
                                        refreshEvents (filterEventsByCat (pack text) updatedTable) ht switcher
                                        savePrefFilter 1 text 0 0 0
                                    else do
                                        refreshEvents (filterEventName   (pack text) updatedTable) ht switcher
                                        savePrefFilter 2 text 0 0 0
                                setFilterLabel builderMain
                                widgetDestroy window

    --Mostra a janela.
    widgetShow window
    endDo

--Dada uma lista de eventos, refaz a hashtable, recria as categorias e insere os eventos correspondentes.
refreshEvents :: [Event] -> HashTable String Builder -> Notebook -> IO ()
refreshEvents es ht switcher = do
    --inserção de evento para salvar a aba.
    currentPage <- notebookGetCurrentPage switcher
    maybePage <- notebookGetNthPage switcher currentPage
    case maybePage of
        Nothing  -> saveLastTabPref ""
        (Just x) -> do
            ctg <- notebookGetTabLabelText switcher x
            case ctg:: Maybe String of
                Nothing -> saveLastTabPref ""
                Just k  -> saveLastTabPref k

    ht <- makeHashTable --Recria a hashtable
    children <- containerGetChildren switcher
    clearSwitcher children
    insertFromTable es ht switcher
    --inserção de evento para abrir a aba.
    endDo

--Dada uma lista de widgets, destrói cada um deles.
clearSwitcher :: [Widget] -> IO ()
clearSwitcher []     = do endDo
clearSwitcher (x:xs) = do
    widgetDestroy x
    clearSwitcher xs

--Cria uma categoria com um dado nome s. Adiciona-a na HashTable. Adiciona-o ao parent. Retorna o builder da categoria criada.
createCategory :: String -> HashTable String Builder -> Notebook-> IO Builder
createCategory s ht parent = do
    categoryBuilder <- makeBuilder "./ui/UI_Categoria.glade"
    fixedBox        <- getFixed categoryBuilder "mainFixed"
    lblCategory     <- getLabel categoryBuilder "lblCategoryTitle"
    colorButton     <- builderGetObject categoryBuilder castToColorButton "btnColor"
    background      <- builderGetObject categoryBuilder castToEventBox "eventbox1"
    background'     <- builderGetObject categoryBuilder castToViewport "viewport1"
    labelSetText lblCategory s

    H.insert ht s categoryBuilder
    notebookPrependPage parent fixedBox s

    --Verifica se uma dada categoria existe. Se sim, adiciona a cor salva.
    categoriesList <- getCategories
    if catExistsCSV s categoriesList
        then do color <- stringToColor (findCatColor categoriesList s)
                colorButtonSetColor colorButton color
                widgetModifyBg background StateNormal color
                widgetModifyBg background' StateNormal color
                endDo
        else do
            insertCategory (Category (pack s) (pack "0 0 0"))
            endDo

    --Ao clicar no botão de cor e selecionar uma nova cor, troca a cor de fundo
    --da categoria, bem como faz um refresh de modo que todos os eventos que possuem
    --o tipo de coloração como "Categoria" possam receber a atualização de cor.
    onColorSet colorButton $ do color <- colorButtonGetColor colorButton
                                deleteCategoryFromFile s
                                colorName <- colorToString color
                                insertCategory $ Category (pack s) (pack colorName)
                                widgetModifyBg background StateNormal color
                                widgetModifyBg background' StateNormal color
                                loadTable <- getProcessedList
                                refreshEvents loadTable ht parent
                                endDo

    --Captura qual o nome da última categoria salva.
    prefs <- getPrefs
    let savedPage = unpack $ lastCategoryTab prefs

    --Captura quantas páginas parent possui.
    childrenCount <- notebookGetNPages parent

    --Faz uma busca pela tab.
    findAndSetSwitcher parent (childrenCount-1) savedPage

    return categoryBuilder

findAndSetSwitcher :: Notebook -> Int -> String -> IO ()
findAndSetSwitcher n 0 _ = notebookSetCurrentPage n 0
findAndSetSwitcher n x s = do
    maybePage <- notebookGetNthPage n x
    case maybePage of
        Nothing  -> notebookSetCurrentPage n 0
        (Just t) -> do
            ctg <- notebookGetTabLabelText n t
            case ctg:: Maybe String of
                Nothing -> notebookSetCurrentPage n 0
                Just k  -> if k == s
                    then notebookSetCurrentPage n x
                    else findAndSetSwitcher n (x-1) s

--Dada uma string indicando o nome de uma categoria, devolve se essa categoria existe ou não.
catExistsCSV :: String -> [Category] -> Bool
catExistsCSV _ []      = False
catExistsCSV s (x: xs) = (catName x == pack s) || catExistsCSV s xs

--Converte os campos de Color para um monad de string separada por espaços.
colorToString :: Color -> IO String
colorToString (Color a b c) = do
    return $ show a ++ " " ++ show b ++ " " ++ show c

--Converte uma string do tipo "a b c", sendo a, b e c inteiros em um monad de Color.
stringToColor :: String -> IO Color
stringToColor s = do
    let [a, b, c] = Data.String.words s
    return $ Color (read a) (read b) (read c)

--Retorna o monad de builder de uma categoria. Assim, pode-se ter acesso a todos os seus elementos.
getCategory :: String -> HashTable String Builder -> IO Builder
getCategory s ht = do
    value <- H.lookup ht s
    case value of
        Just x -> return x
        Nothing -> error "Referência de categoria não encontrada."

--Verifica se uma dada categoria existe na hashtable.
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

--Verifica se o nome do evento e a categoria são válidos, isto é, se esses campos são vazios.
--Retorna true se algum campo for vazio.
checkFields :: Builder -> IO Bool
checkFields bEvent = do
    nameBox        <- getTextBox bEvent "txtBoxEvent"
    descriptionBox <- getTextBox bEvent "txtBoxDescription"
    categoryBox    <- getTextBox bEvent "txtBoxCategory"
    name           <- entryGetText nameBox
    category       <- entryGetText categoryBox
    return $ name == "" || category == ""

--Verifica se um dado evento existe na lista de eventos lida do CSV de eventos.
eventExists :: Builder -> IO Bool
eventExists bdAdd = do
    nameBox <- getTextBox bdAdd "txtBoxEvent"
    nome    <- entryGetText nameBox
    checkEvent nome <$> getEvents

--Função auxiliar. Dado o nome de um evento, verifica se esse evento está na lista de eventos.
checkEvent :: String -> [Event] -> Bool
checkEvent s []     = False
checkEvent s (x:xs) = b || checkEvent s xs
    where
        b = s == unpack (name x)

--Esta função deve receber:
--  O builder do createEvent, que é de onde serão capturados os textos inseridos pelo usuário nas entry boxes.
--  O builder da main page. Caso a categoria não exista, ela precisará ser criada.
--  O Notebook (switcher), o qual será parent do widget da categoria.
--  A hashTable para que seja possível pegar a referência da categoria, caso exista. 
addEvent :: Builder -> Builder -> Notebook -> HashTable String Builder -> IO ()
addEvent bMain bEvento switcher ht = do
    categoria    <- getTextFromEntry bEvento "txtBoxCategory"
    eventForm    <- toEventForm bEvento

    --Salva o evento no CSV de eventos.
    Lib.insertEvent eventForm
    isValid      <- categoryExists categoria ht

    --Verifica se a categoria existe.
    if not isValid
        then do
            --Caso não exista a categoria, cria-se uma nova categoria e esta é inserida no switcher.
            bCategoria <- createCategory categoria ht switcher
            Main.insertEvent eventForm ht switcher
            putStrLn "Categoria criada."
            endDo
        else do
            --A categoria já existe. Basta que se pegue a referência e adicionemos o evento à vertical box.
            Main.insertEvent eventForm ht switcher
            putStrLn ""

--Pega um builder e retorna os campos na forma de evento. Esse builder corresponde a um widget com campos
--pré-determinados.
toEventForm :: Builder -> IO Event
toEventForm builder = do
    eName        <- getTextFromEntry builder "txtBoxEvent"
    eDescription <- getTextFromEntry builder "txtBoxDescription"
    eCategory    <- getTextFromEntry builder "txtBoxCategory"
    eRecurrent   <- builderGetObject builder castToToggleButton "recurrentCheckbox"
    isRecurrent  <- toggleButtonGetActive eRecurrent
    eRegularity  <- getSpin builder "regularity"
    vRegularity  <- spinButtonGetValue eRegularity
    colorType    <- builderGetObject builder castToComboBox "combobox1"
    vColorType   <- comboBoxGetActive colorType
    colorUrg     <- getSpin builder "spin_urg"
    vColorUrg    <- spinButtonGetValue colorUrg
    btnColor     <- builderGetObject builder castToColorButton "colorbutton1"

    --Colors
    vBtnColor    <- colorButtonGetColor btnColor
    let urg      = fromInteger $ truncate vColorUrg
    let valueRec = fromInteger $ truncate vRegularity
    color'       <- colorToString vBtnColor
    let color    = switchOnColor vColorType color' urg
    print vColorType

    --Calendar settings.
    eCalendar    <- builderGetObject builder castToCalendar "calendar"
    cFormat      <- calendarGetDate eCalendar
    eDay         <- getDate cFormat 0
    eMonth       <- getDate cFormat 1
    eYear        <- getDate cFormat 2

    --Month tem um retorno de 0 a 11. Logo foi ajustado para 1 a 12.
    return $ Item (pack eName) eDay (eMonth+1) eYear (pack eDescription) (pack eCategory) isRecurrent valueRec color

--Dado um inteiro n, retorna o tipo de cor correspondente.
--A string s corresponde à string convertida de Color.
--O inteiro k corresponde à urgência do gradiente.
switchOnColor :: Int -> String -> Int -> ColorSrc
switchOnColor n s k = case n of
    0 -> Gradient k
    1 -> CatName
    2 -> Custom (pack s)
    _ -> Gradient k

--Retorna o dia, mês ou ano dependendo do parâmetro passado. 0 => dia, 1 => mês, 2 => ano.
getDate :: (Int, Int, Int) -> Int -> IO Int
getDate (_, _, d) 0 = return d
getDate (_, m, _) 1 = return m
getDate (y, _, _) 2 = return y
getDate (_, _, _) _ = return $ -1 --Esta ação nunca ocorrerá.

--Retorna o ano de uma data.
getYear :: (Integer, Int, Int) -> Int
getYear (a, _, _) = fromInteger a

--retorna o mês de uma data.
getMonth :: (Integer, Int, Int) -> Int
getMonth (_, a, _) = a

--Retorna o dia de uma data.
getDay :: (Integer, Int, Int) -> Int
getDay (_, _, a) = a

--Retorna o texto que está dentro de uma entry box (digitado pelo usuário).
getTextFromEntry :: Builder -> String -> IO String
getTextFromEntry builder s = do
    entryRef <- getTextBox builder s
    entryGetText entryRef

--Retorna qual é o nome do mês dado um inteiro que equivale ao mês.
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

--Dada uma lista de eventos, uma hastable e um Notebook (switcher), insere os eventos
--em duas correspondentes categorias.
insertFromTable :: [Event] -> HashTable String Builder -> Notebook -> IO ()
insertFromTable [] _ _             = endDo
insertFromTable (x:xs) ht switcher = do
    let nCategory = unpack $ category x
    isValid <- categoryExists nCategory ht
    autodel <- getAutodelete
    hoje <- getToday

    --Caso o autodelete esteja ativado e o total de dias restantes do evento seja negativo, este é excluído do CSV.
    if autodel && (daysleft x hoje) < 0
        then do
            insertFromTable xs ht switcher
            removeExpiredEvents
        else
            if not isValid
                then do
                    cat <- createCategory nCategory ht switcher
                    Main.insertEvent x ht switcher
                    insertFromTable xs ht switcher
                else do
                    Main.insertEvent x ht switcher
                    insertFromTable xs ht switcher

--Dada uma cor e a lista de categorias, retorna a cor correspondente.
getBackgroundColor :: Int -> ColorSrc -> String -> [Category] -> IO Color
getBackgroundColor _ CatName s cs       = stringToColor (findCatColor cs s)
getBackgroundColor x (Gradient k)   _ _ = return $ gradient x k
getBackgroundColor _ (Custom s) _ _     = stringToColor (unpack s)

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
    let nColor       = color event

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


    categoryList   <- getCategories
    let bCatExists = catExistsCSV nCategory categoryList

    if bCatExists
        then do
            --Ajusta a cor de fundo
            newColor <- getBackgroundColor (daysleft event hoje) nColor nCategory categoryList
            bg1 <- builderGetObject bLinhaEvento castToEventBox "eventbox1"
            bg2 <- builderGetObject bLinhaEvento castToViewport "viewport1"
            widgetModifyBg bg1 StateNormal newColor
            widgetModifyBg bg2 StateNormal newColor
        else do
            insertCategory (Category (pack nCategory) (pack "0 0 0"))
            let sColor = (Color 0 0 0)
            bg1 <- builderGetObject bLinhaEvento castToEventBox "eventbox1"
            bg2 <- builderGetObject bLinhaEvento castToViewport "viewport1"
            widgetModifyBg bg1 StateNormal sColor
            widgetModifyBg bg2 StateNormal sColor
            endDo


    --BOTÕES ----------------------------------------------------
    btnEditar <- getButton bLinhaEvento "btnEditar"
    --O botão de editar cria um novo widget em uma nova janela, semelhante ao widget "Adicionar evento",
    --mas neste caso todos os seus campos são automaticamente preenchidos com a informação do evento.
    --Neste widget o usuário pode editar as informações do evento correspondente.
    onClicked btnEditar $ do bEdit <- makeBuilder "./ui/UI_editarEvento.glade"
                             bEditWindow <- builderGetObject bEdit castToDialog "editEventWindow"
                             setTextBoxText bEdit "txtBoxEvent" nName
                             setTextBoxText bEdit "txtBoxDescription" nDescription
                             setTextBoxText bEdit "txtBoxCategory" nCategory
                             calendario   <- builderGetObject bEdit castToCalendar "calendar"
                             calendarSelectDay calendario nDay
                             calendarSelectMonth calendario (nMonth-1) nYear
                             widgetShow bEditWindow
                             colorButton  <- builderGetObject bEdit castToColorButton "colorbutton1"
                             colorType    <- builderGetObject bEdit castToComboBox "combobox1"
                             spinGradient <- getSpin bEdit "spin_urg"
                             spinRecurrency <- getSpin bEdit "regularity"
                             spinButtonSetValue spinRecurrency $ read (show nRegularity)
                             recurrentBox <- builderGetObject bEdit castToToggleButton "recurrentCheckbox"
                             regularityP  <- builderGetObject bEdit castToHPaned "regularityPanel"


                             if nRecurrent
                                then do
                                    toggleButtonSetActive recurrentBox True
                                    widgetShow regularityP
                                else do
                                    toggleButtonSetActive recurrentBox False
                                    widgetHide regularityP

                             onClicked recurrentBox $ do isVisible <- widgetGetVisible regularityP
                                                         if isVisible then widgetHide regularityP  else widgetShow regularityP
                                                         endDo




                             adjustComboBox nColor colorType colorButton spinGradient

                             on colorType changed $ do selectedColorType <- comboBoxGetActive colorType
                                                       if selectedColorType == 2
                                                        then widgetShow colorButton
                                                        else widgetHide colorButton
                                                       if selectedColorType == 0
                                                        then do
                                                            widgetShow spinGradient
                                                            spinButtonSetValue spinGradient 1
                                                        else widgetHide spinGradient

                             btnEdit <- getButton bEdit "btnEditar"
                             onClicked btnEdit $ do deleteEventFromFile nName
                                                    editedEvent <- toEventForm bEdit
                                                    Lib.insertEvent editedEvent
                                                    updatedList <- getProcessedList
                                                    print updatedList
                                                    refreshEvents updatedList ht switcher
                                                    widgetDestroy bEditWindow
                                                    b <- containerGetChildren categoriesBox

                                                    let categoriesevents = filterEventsByCat (pack nCategory) updatedList



                                                    if Prelude.null categoriesevents
                                                        then do page <- getFixed categoriaBuilderRef "mainFixed"
                                                                widgetDestroy page
                                                                deleteCategoryFromFile nCategory
                                                                endDo
                                                        else endDo


                             btnCancelar <- getButton bEdit "btnCancelar"
                             onClicked btnCancelar $ do widgetDestroy bEditWindow

                             endDo

    --Ao clicar no botão "X", o evento correspondente é excluído.
    btnExcluir <- getButton bLinhaEvento "btnExcluir"
    onClicked btnExcluir $ do widgetDestroy newEvento
                              b <- containerGetChildren categoriesBox
                              _ <- deleteEventFromFile nName
                              if Prelude.length b == 0
                                then do page <- getFixed categoriaBuilderRef "mainFixed"
                                        widgetDestroy page
                                        putStrLn ("Evento " ++ nName ++ " e categoria "++ nCategory ++ " removidos com sucesso.")
                                        deleteCategoryFromFile nCategory
                                        endDo
                                else putStrLn ("Evento " ++ nName ++ " removido com sucesso.")
    -------------------------------------------------------------

    --Adiciona o evento ao vertical box.
    widgetReparent newEvento categoriesBox

    endDo

--Ajusta o combobox de cores de acordo com o tipo selecionado (escondendo ou não o botão de cor e o spinbox).
adjustComboBox :: ColorSrc -> ComboBox -> ColorButton -> SpinButton -> IO ()
adjustComboBox (Gradient k) box cb sb = do
    comboBoxSetActive box 1
    comboBoxSetActive box 0
    spinButtonSetValue sb $ read (show k)
    widgetShow sb
    widgetHide cb
adjustComboBox CatName      box cb sb = do
    comboBoxSetActive box 0
    comboBoxSetActive box 1
    widgetHide sb
    widgetHide cb
adjustComboBox (Custom k)   box cb sb = do
    comboBoxSetActive box 0
    comboBoxSetActive box 2
    color <- stringToColor (unpack k)
    colorButtonSetColor cb color
    widgetHide sb
    widgetShow cb

--Dado um textBox, seu nome e uma string, esta string é inserida no textBox.
setTextBoxText :: Builder -> String -> String -> IO ()
setTextBoxText builder txtbox texto = do
    textBox <- builderGetObject builder castToEntry txtbox
    entrySetText textBox texto
    endDo

--A primeira string corresponde ao nome do label e a segunda, ao novo texto que o label terá.
--Logo, insere um dado texto em uma dada label.
setLabelText :: Builder -> String -> String -> IO ()
setLabelText builder lbl lblText = do
    label <- getLabel builder lbl
    labelSetText label lblText
    endDo

--Retorna a referência de um spin dado o builder que o contém e seu nome correspondente no widget.
getSpin :: Builder -> String -> IO SpinButton
getSpin b s = do
    builderGetObject b castToSpinButton s

--Retorna a referência de um action dado o builder que o contém e seu nome correspondente no widget.
getAction :: Builder -> String -> IO Action
getAction b s = do
    builderGetObject b castToAction s

--Retorna a referência de um vertical box dado o builder que o contém e seu nome correspondente no widget.
getVerticalBox :: Builder -> String -> IO VBox
getVerticalBox b s = do
    builderGetObject b castToVBox s

--Retorna a referência de um "fixed" (janela) dado o builder que o contém e seu nome correspondente no widget.
getFixed :: Builder -> String -> IO Fixed
getFixed b s = do
    builderGetObject b castToFixed s

--Retorna a referência de uma label dado o builder que a contém e seu nome correspondente no widget.
getLabel :: Builder -> String -> IO Label
getLabel b s = do
    builderGetObject b castToLabel s

--Retorna a referência de uma textBox dado o builder que a contém e seu nome correspondente no widget.
getTextBox :: Builder -> String -> IO Entry
getTextBox b s = do
    builderGetObject b castToEntry s

--Retorna a referência de um botão dado o builder que o contém e seu nome correspondente no widget.
getButton :: Builder -> String -> IO Button
getButton b s = do
    builderGetObject b castToButton s

--Cria um builder para um terminado .glade e retorna seu monad.
--O builder serve para que as referências dos objetos que constituem o widget possam ser capturadas.
makeBuilder :: String -> IO Builder
makeBuilder s = do
    builder <- builderNew
    builderAddFromFile builder s
    return builder

--Cria uma janela para adicionar um evento. 
createEvent :: Builder -> IO Dialog
createEvent b = do

    --Cria um novo widget para adição de evento.
    novoEvento   <- builderGetObject b castToDialog "newEventWindow"
    calendario   <- builderGetObject b castToCalendar "calendar"
    hoje         <- getToday

    --Inicializa o calendário com a data de hoje.
    --O calendar possui meses de 0 a 11. Por isso o -1 do getMonth.
    calendarSelectMonth calendario (getMonth hoje - 1) (getYear hoje)
    calendarSelectDay calendario (getDay hoje)

    --Inicializa as informações de cores.
    colorButton  <- builderGetObject b castToColorButton "colorbutton1"
    colorType    <- builderGetObject b castToComboBox "combobox1"
    spinGradient <- getSpin b "spin_urg"
    spinButtonSetValue spinGradient 1
    comboBoxSetActive colorType 1
    comboBoxSetActive colorType 0
    widgetHide colorButton

    --Caso o tipo de coloração do combobox seja alterado, exibe ou não as caixas extras pertinentes.
    on colorType changed $ do selectedColorType <- comboBoxGetActive colorType
                              if selectedColorType == 2
                                then widgetShow colorButton
                                else widgetHide colorButton
                              if selectedColorType == 0
                                then widgetShow spinGradient
                                else widgetHide spinGradient

    -- CANCEL   ##################################################################
    -- Remove a janela de adição do novo evento. Quit.
    cancel  <- builderGetObject b castToButton "btnCancelar"
    onClicked cancel $ do widgetDestroy novoEvento
                          putStrLn "Adição cancelada."
    -- ###########################################################################

    -- Mostra o widget na tela.
    widgetShow novoEvento
    return novoEvento