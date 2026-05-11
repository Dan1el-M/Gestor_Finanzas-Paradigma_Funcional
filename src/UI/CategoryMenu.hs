module UI.CategoryMenu where

import Text.Read (readMaybe)
import FileManager (cargarCategorias)
import qualified Services.CategoryService as Service
import Services.FinanceRegistryService (cargarRegistros)
import Models
import UI.UIHelpers (cerrar, err, menuOpciones, ok, opcion, prompt, promptOpcion, titulo)

menuCategoria :: IO ()
menuCategoria = do
    menuOpciones "Gestion de Categorias"
        [ opcion 1 "Crear categoria"
        , opcion 2 "Listar categorias"
        , opcion 3 "Buscar categoria por ID"
        , opcion 4 "Actualizar categoria"
        , opcion 5 "Eliminar categoria"
        , opcion 6 "Volver"
        ]
    seleccion <- promptOpcion
    case seleccion of
        "1" -> crearCategoriaMenu    >> menuCategoria
        "2" -> listarCategoriasMenu  >> menuCategoria
        "3" -> buscarCategoriaMenu   >> menuCategoria
        "4" -> actualizarCategoriaMenu >> menuCategoria
        "5" -> eliminarCategoriaMenu >> menuCategoria
        "6" -> ok "Volviendo..."
        _   -> err "Opcion invalida." >> menuCategoria

menuCategorias :: IO ()
menuCategorias = menuCategoria

cargarCategoriasMenu :: IO [Categoria]
cargarCategoriasMenu = cargarCategorias

crearCategoriaMenu :: IO ()
crearCategoriaMenu = do
    titulo "Crear Categoria"
    cerrar
    nombre <- prompt "Nombre"
    categorias <- cargarCategoriasMenu
    resultado <- Service.crearCategoriaService nombre categorias
    case resultado of
        Left mensaje -> err mensaje
        Right _      -> ok "Categoria creada correctamente."

listarCategoriasMenu :: IO ()
listarCategoriasMenu = do
    categorias <- cargarCategoriasMenu
    titulo "Categorias Registradas"
    cerrar
    mostrarCategorias categorias

buscarCategoriaMenu :: IO ()
buscarCategoriaMenu = do
    titulo "Buscar Categoria"
    cerrar
    categorias <- cargarCategoriasMenu
    mostrarCategorias categorias
    textoId <- prompt "ID"
    case readMaybe textoId :: Maybe Int of
        Nothing -> err "Debe ingresar un numero."
        Just idBuscado ->
            case Service.buscarCategoriaPorId idBuscado categorias of
                Nothing   -> err "No existe una categoria con ese ID."
                Just cat  -> ok ("Encontrada: " ++ nombreCategoria cat)

actualizarCategoriaMenu :: IO ()
actualizarCategoriaMenu = do
    titulo "Actualizar Categoria"
    cerrar
    categorias <- cargarCategoriasMenu
    mostrarCategorias categorias
    textoId <- prompt "ID a actualizar"
    case readMaybe textoId :: Maybe Int of
        Nothing -> err "Debe ingresar un numero."
        Just idBuscado -> do
            nuevoNombre <- prompt "Nuevo nombre"
            resultado <- Service.actualizarCategoriaService idBuscado nuevoNombre categorias
            case resultado of
                Left mensaje -> err mensaje
                Right _      -> ok "Categoria actualizada correctamente."

eliminarCategoriaMenu :: IO ()
eliminarCategoriaMenu = do
    titulo "Eliminar Categoria"
    cerrar
    categorias <- cargarCategoriasMenu
    mostrarCategorias categorias
    textoId <- prompt "ID a eliminar"
    case readMaybe textoId :: Maybe Int of
        Nothing -> err "Debe ingresar un numero."
        Just idBuscado -> do
            registros <- cargarRegistros
            resultado <- Service.eliminarCategoriaService idBuscado categorias registros
            case resultado of
                Left mensaje -> err mensaje
                Right _      -> ok "Categoria eliminada correctamente."

mostrarCategorias :: [Categoria] -> IO ()
mostrarCategorias [] = err "No hay categorias registradas."
mostrarCategorias cs = mapM_ mostrarCategoria cs

mostrarCategoria :: Categoria -> IO ()
mostrarCategoria cat =
    putStrLn $ "  │  ID: " ++ show (idCategoria cat)
            ++ "  │  Nombre: " ++ nombreCategoria cat

pedirIdCategoria :: IO Int
pedirIdCategoria = do
    categorias <- cargarCategoriasMenu
    titulo "Seleccionar Categoria"
    cerrar
    mostrarCategorias categorias
    textoId <- prompt "ID de categoria"
    case readMaybe textoId :: Maybe Int of
        Nothing -> err "Debe ingresar un numero." >> pedirIdCategoria
        Just idCat ->
            case Service.buscarCategoriaPorId idCat categorias of
                Nothing -> err "No existe esa categoria." >> pedirIdCategoria
                Just _  -> return idCat

