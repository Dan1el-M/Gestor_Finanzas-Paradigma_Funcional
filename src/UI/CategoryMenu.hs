module UI.CategoryMenu where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import FileManager (cargarCategorias)
import qualified Services.CategoryService as Service
import Services.FinanceRegistryService (cargarRegistros)
import Models
import UI.UIHelpers (titulo, cerrar, ok, err)

menuCategoria :: IO ()
menuCategoria = do
    titulo "Gestion de Categorias"
    putStrLn "  ║  1. Crear categoria                              ║"
    putStrLn "  ║  2. Listar categorias                            ║"
    putStrLn "  ║  3. Buscar categoria por ID                      ║"
    putStrLn "  ║  4. Actualizar categoria                         ║"
    putStrLn "  ║  5. Eliminar categoria                           ║"
    putStrLn "  ║  6. Volver                                       ║"
    cerrar
    putStr "  Opcion » "
    hFlush stdout
    opcion <- getLine
    case opcion of
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
    putStr "  Nombre » "
    hFlush stdout
    nombre <- getLine
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
    putStr "  ID » "
    hFlush stdout
    textoId <- getLine
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
    putStr "  ID a actualizar » "
    hFlush stdout
    textoId <- getLine
    case readMaybe textoId :: Maybe Int of
        Nothing -> err "Debe ingresar un numero."
        Just idBuscado -> do
            putStr "  Nuevo nombre » "
            hFlush stdout
            nuevoNombre <- getLine
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
    putStr "  ID a eliminar » "
    hFlush stdout
    textoId <- getLine
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
    putStr "  ID de categoria » "
    hFlush stdout
    textoId <- getLine
    case readMaybe textoId :: Maybe Int of
        Nothing -> err "Debe ingresar un numero." >> pedirIdCategoria
        Just idCat ->
            case Service.buscarCategoriaPorId idCat categorias of
                Nothing -> err "No existe esa categoria." >> pedirIdCategoria
                Just _  -> return idCat

