module UI.CategoryMenu where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import qualified Services.CategoryService as Service
import Models

-- Menu especifico para el CRUD de categorias.
menuCategorias :: [Categoria] -> [RegistroFinanciero] -> IO [Categoria]
menuCategorias categorias registros = do
    putStrLn ""
    putStrLn "===== Gestion de Categorias ====="
    putStrLn "1. Crear categoria"
    putStrLn "2. Listar categorias"
    putStrLn "3. Buscar categoria por ID"
    putStrLn "4. Actualizar categoria"
    putStrLn "5. Eliminar categoria"
    putStrLn "6. Volver al menu principal"
    putStr "Opcion: "
    hFlush stdout

    opcion <- getLine

    case opcion of
        "1" -> crearCategoriaMenu categorias registros
        "2" -> listarCategoriasMenu categorias registros
        "3" -> buscarCategoriaMenu categorias registros
        "4" -> actualizarCategoriaMenu categorias registros
        "5" -> eliminarCategoriaMenu categorias registros
        "6" -> return categorias
        _ -> do
            putStrLn "Opcion invalida."
            menuCategorias categorias registros

-- Lee los datos necesarios para crear una categoria.
crearCategoriaMenu :: [Categoria] -> [RegistroFinanciero] -> IO [Categoria]
crearCategoriaMenu categorias registros = do
    putStr "Nombre de la nueva categoria: "
    hFlush stdout
    nombre <- getLine

    resultado <- Service.crearCategoriaService nombre categorias
    case resultado of
        Left mensaje -> do
            putStrLn mensaje
            menuCategorias categorias registros
        Right nuevasCategorias -> do
            putStrLn "Categoria creada correctamente."
            menuCategorias nuevasCategorias registros

mostrarTituloYCategorias :: [Categoria] -> IO ()
mostrarTituloYCategorias categorias = do
    putStrLn ""
    putStrLn "Categorias registradas:"
    mostrarCategorias categorias
    putStrLn ""

-- Muestra todas las categorias registradas.
listarCategoriasMenu :: [Categoria] -> [RegistroFinanciero] -> IO [Categoria]
listarCategoriasMenu categorias registros = do
    mostrarTituloYCategorias categorias
    menuCategorias categorias registros

-- Lee un ID y muestra la categoria encontrada.
buscarCategoriaMenu :: [Categoria] -> [RegistroFinanciero] -> IO [Categoria]
buscarCategoriaMenu categorias registros = do
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

    menuCategorias categorias registros

-- Lee los datos necesarios para actualizar una categoria existente.
actualizarCategoriaMenu :: [Categoria] -> [RegistroFinanciero] -> IO [Categoria]
actualizarCategoriaMenu categorias registros = do
    mostrarTituloYCategorias categorias
    putStr "Ingrese el ID de la categoria a actualizar: "
    hFlush stdout
    textoId <- getLine

    case readMaybe textoId :: Maybe Int of
        Nothing -> do
            putStrLn "ID invalido. Debe ingresar un numero."
            menuCategorias categorias registros

        Just idBuscado -> do
            putStr "Nuevo nombre de la categoria: "
            hFlush stdout
            nuevoNombre <- getLine

            resultado <- Service.actualizarCategoriaService idBuscado nuevoNombre categorias
            case resultado of
                Left mensaje -> do
                    putStrLn mensaje
                    menuCategorias categorias registros

                Right categoriasActualizadas -> do
                    putStrLn "Categoria actualizada correctamente."
                    menuCategorias categoriasActualizadas registros

-- Lee el ID de la categoria que se desea eliminar.
eliminarCategoriaMenu :: [Categoria] -> [RegistroFinanciero] -> IO [Categoria]
eliminarCategoriaMenu categorias registros = do
    mostrarTituloYCategorias categorias
    putStr "Ingrese el ID de la categoria a eliminar: "
    hFlush stdout
    textoId <- getLine

    case readMaybe textoId :: Maybe Int of
        Nothing -> do
            putStrLn "ID invalido. Debe ingresar un numero."
            menuCategorias categorias registros

        Just idBuscado -> do
            resultado <- Service.eliminarCategoriaService idBuscado categorias registros
            case resultado of
                Left mensaje -> do
                    putStrLn mensaje
                    menuCategorias categorias registros

                Right categoriasActualizadas -> do
                    putStrLn "Categoria eliminada correctamente."
                    menuCategorias categoriasActualizadas registros

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
