module UI.CategoryMenu where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import FileManager (cargarCategorias)
import qualified Services.CategoryService as Service
import Services.FinanceRegistryService (cargarRegistros)
import Models

-- Menu especifico para el CRUD de categorias.
menuCategoria :: IO ()
menuCategoria = do
    putStrLn ""
    putStrLn "===== Gestion de Categorias ====="
    putStrLn "1. Crear categoria"
    putStrLn "2. Listar categorias"
    putStrLn "3. Buscar categoria por ID"
    putStrLn "4. Actualizar categoria"
    putStrLn "5. Eliminar categoria"
    putStrLn "6. Volver al menu de registros financieros"
    putStr "Opcion: "
    hFlush stdout

    opcion <- getLine

    case opcion of
        "1" -> crearCategoriaMenu >> menuCategoria
        "2" -> listarCategoriasMenu >> menuCategoria
        "3" -> buscarCategoriaMenu >> menuCategoria
        "4" -> actualizarCategoriaMenu >> menuCategoria
        "5" -> eliminarCategoriaMenu >> menuCategoria
        "6" -> putStrLn "Volviendo al menu de registros financieros..."
        _ -> do
            putStrLn "Opcion invalida."
            menuCategoria

menuCategorias :: IO ()
menuCategorias = menuCategoria

cargarCategoriasMenu :: IO [Categoria]
cargarCategoriasMenu = cargarCategorias

-- Lee los datos necesarios para crear una categoria.
crearCategoriaMenu :: IO ()
crearCategoriaMenu = do
    putStr "Nombre de la nueva categoria: "
    hFlush stdout
    nombre <- getLine

    categorias <- cargarCategoriasMenu
    resultado <- Service.crearCategoriaService nombre categorias
    case resultado of
        Left mensaje ->
            putStrLn mensaje
        Right _ -> do
            putStrLn "Categoria creada correctamente."

mostrarTituloYCategorias :: [Categoria] -> IO ()
mostrarTituloYCategorias categorias = do
    putStrLn ""
    putStrLn "Categorias registradas:"
    mostrarCategorias categorias
    putStrLn ""

-- Muestra todas las categorias registradas.
listarCategoriasMenu :: IO ()
listarCategoriasMenu = do
    categorias <- cargarCategoriasMenu
    mostrarTituloYCategorias categorias

-- Lee un ID y muestra la categoria encontrada.
buscarCategoriaMenu :: IO ()
buscarCategoriaMenu = do
    categorias <- cargarCategoriasMenu
    mostrarTituloYCategorias categorias
    putStr "Ingrese el ID de la categoria: "
    hFlush stdout
    textoId <- getLine

    case readMaybe textoId :: Maybe Int of
        Nothing -> putStrLn "ID invalido. Debe ingresar un numero."
        Just idBuscado ->
            case Service.buscarCategoriaPorId idBuscado categorias of
                Nothing -> putStrLn "No existe una categoria con ese ID."
                Just categoria -> mostrarCategoria categoria

-- Lee los datos necesarios para actualizar una categoria existente.
actualizarCategoriaMenu :: IO ()
actualizarCategoriaMenu = do
    categorias <- cargarCategoriasMenu
    mostrarTituloYCategorias categorias
    putStr "Ingrese el ID de la categoria a actualizar: "
    hFlush stdout
    textoId <- getLine

    case readMaybe textoId :: Maybe Int of
        Nothing ->
            putStrLn "ID invalido. Debe ingresar un numero."

        Just idBuscado -> do
            putStr "Nuevo nombre de la categoria: "
            hFlush stdout
            nuevoNombre <- getLine

            resultado <- Service.actualizarCategoriaService idBuscado nuevoNombre categorias
            case resultado of
                Left mensaje ->
                    putStrLn mensaje

                Right _ ->
                    putStrLn "Categoria actualizada correctamente."

-- Lee el ID de la categoria que se desea eliminar.
eliminarCategoriaMenu :: IO ()
eliminarCategoriaMenu = do
    categorias <- cargarCategoriasMenu
    mostrarTituloYCategorias categorias
    putStr "Ingrese el ID de la categoria a eliminar: "
    hFlush stdout
    textoId <- getLine

    case readMaybe textoId :: Maybe Int of
        Nothing ->
            putStrLn "ID invalido. Debe ingresar un numero."

        Just idBuscado -> do
            registros <- cargarRegistros
            resultado <- Service.eliminarCategoriaService idBuscado categorias registros
            case resultado of
                Left mensaje ->
                    putStrLn mensaje

                Right _ ->
                    putStrLn "Categoria eliminada correctamente."

-- Muestra una lista de categorias.
mostrarCategorias :: [Categoria] -> IO ()
mostrarCategorias [] = putStrLn "No hay categorias registradas."
mostrarCategorias categorias = mostrarCategoriasRec categorias

mostrarCategoriasRec :: [Categoria] -> IO ()
mostrarCategoriasRec [] = return ()
mostrarCategoriasRec (categoria:resto) = do
    mostrarCategoria categoria
    mostrarCategoriasRec resto

-- Muestra una categoria individual.
mostrarCategoria :: Categoria -> IO ()
mostrarCategoria categoria = do
    putStrLn ("ID: " ++ show (idCategoria categoria)
        ++ " | Nombre: " ++ nombreCategoria categoria)
