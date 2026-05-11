module Main where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Models
import CategoryManager
import FileManager
import Reports

-- Punto de entrada del programa.
-- Carga las categorías desde data/categorias.txt
-- y una lista vacía de registros financieros.
main :: IO ()
main = do
    putStrLn "===================================="
    putStrLn "        FinanTrack Haskell"
    putStrLn " Sistema de Finanzas Personales"
    putStrLn "===================================="

    -- Carga las categorías desde data/categorias.txt
    categoriasIniciales <- cargarCategorias

    -- Lista inicial de registros financieros.
    -- Más adelante esta lista se cargará desde data/registros.txt.
    let registrosIniciales = []

    menuPrincipal categoriasIniciales registrosIniciales

-- Menú principal del sistema.
-- Recibe la lista de categorías y la lista de registros para mantener
-- el estado del programa mientras está en ejecución.
menuPrincipal :: [Categoria] -> [RegistroFinanciero] -> IO ()
menuPrincipal categorias registros = do
    putStrLn ""
    putStrLn "Seleccione una opcion:"
    putStrLn "1. Registrar movimiento financiero"
    putStrLn "2. Ver registros financieros"
    putStrLn "3. Gestionar categorias"
    putStrLn "4. Gestionar presupuestos"
    putStrLn "5. Evaluar reglas y alertas"
    putStrLn "6. Ver analisis financiero"
    putStrLn "7. Simular escenario financiero"
    putStrLn "8. Generar reportes"
    putStrLn "9. Guardar datos"
    putStrLn "10. Salir"
    putStr "Opcion: "
    hFlush stdout

    opcion <- getLine
    ejecutarOpcion opcion categorias registros

-- Ejecuta la opción seleccionada por el usuario.
ejecutarOpcion :: String -> [Categoria] -> [RegistroFinanciero] -> IO ()
ejecutarOpcion opcion categorias registros =
    case opcion of
        "1" -> do
            putStrLn "Modulo de registro financiero pendiente de implementar."
            menuPrincipal categorias registros

        "2" -> do
            putStrLn "Modulo para ver registros pendiente de implementar."
            menuPrincipal categorias registros

        "3" -> do
            categoriasActualizadas <- menuCategorias categorias registros
            menuPrincipal categoriasActualizadas registros

        "4" -> do
            putStrLn "Modulo de presupuestos pendiente de implementar."
            menuPrincipal categorias registros

        "5" -> do
            putStrLn "Modulo de reglas y alertas pendiente de implementar."
            menuPrincipal categorias registros

        "6" -> do
            putStrLn "Modulo de analisis financiero pendiente de implementar."
            menuPrincipal categorias registros

        "7" -> do
            putStrLn "Generando reporte basico de prueba..."
            putStrLn (generarResumenBasico registros)
            menuPrincipal categorias registros

        "8" -> do
            putStrLn "Guardando datos de prueba..."
            guardarLineasArchivo rutaRegistros []
            putStrLn "Datos guardados correctamente."
            menuPrincipal categorias registros

        "10" -> do
            putStrLn "Saliendo del sistema..."

        _ -> do
            putStrLn "Opcion invalida. Intente de nuevo."
            menuPrincipal categorias registros

-- Menú específico para el CRUD de categorías.
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
        "1" -> crearCategoriaDesdeConsola categorias registros
        "2" -> listarCategoriasDesdeConsola categorias registros
        "3" -> buscarCategoriaDesdeConsola categorias registros
        "4" -> actualizarCategoriaDesdeConsola categorias registros
        "5" -> eliminarCategoriaDesdeConsola categorias registros
        "6" -> return categorias
        _ -> do
            putStrLn "Opcion invalida."
            menuCategorias categorias registros

-- Crea una categoría leyendo el nombre desde consola.
crearCategoriaDesdeConsola :: [Categoria] -> [RegistroFinanciero] -> IO [Categoria]
crearCategoriaDesdeConsola categorias registros = do
    putStr "Nombre de la nueva categoria: "
    hFlush stdout
    nombre <- getLine

    case buscarCategoriaPorNombre nombre categorias of
        Just _ -> do
            putStrLn "Ya existe una categoria con ese nombre."
            menuCategorias categorias registros

        Nothing -> do
            let nuevasCategorias = crearCategoria nombre categorias
            guardarCategorias nuevasCategorias
            putStrLn "Categoria creada correctamente."
            menuCategorias nuevasCategorias registros

-- Muestra todas las categorías registradas.
listarCategoriasDesdeConsola :: [Categoria] -> [RegistroFinanciero] -> IO [Categoria]
listarCategoriasDesdeConsola categorias registros = do
    putStrLn ""
    putStrLn "Categorias registradas:"
    mostrarCategorias categorias
    menuCategorias categorias registros

-- Busca una categoría por ID desde consola.
buscarCategoriaDesdeConsola :: [Categoria] -> [RegistroFinanciero] -> IO [Categoria]
buscarCategoriaDesdeConsola categorias registros = do
    putStr "Ingrese el ID de la categoria: "
    hFlush stdout
    textoId <- getLine

    case readMaybe textoId :: Maybe Int of
        Nothing -> putStrLn "ID invalido. Debe ingresar un numero."
        Just idBuscado ->
            case buscarCategoriaPorId idBuscado categorias of
                Nothing -> putStrLn "No existe una categoria con ese ID."
                Just categoria -> mostrarCategoria categoria

    menuCategorias categorias registros

-- Actualiza el nombre de una categoría existente.
actualizarCategoriaDesdeConsola :: [Categoria] -> [RegistroFinanciero] -> IO [Categoria]
actualizarCategoriaDesdeConsola categorias registros = do
    putStr "Ingrese el ID de la categoria a actualizar: "
    hFlush stdout
    textoId <- getLine

    case readMaybe textoId :: Maybe Int of
        Nothing -> do
            putStrLn "ID invalido. Debe ingresar un numero."
            menuCategorias categorias registros

        Just idBuscado ->
            case buscarCategoriaPorId idBuscado categorias of
                Nothing -> do
                    putStrLn "No existe una categoria con ese ID."
                    menuCategorias categorias registros

                Just _ -> do
                    putStr "Nuevo nombre de la categoria: "
                    hFlush stdout
                    nuevoNombre <- getLine

                    let categoriasActualizadas =
                            actualizarCategoria idBuscado nuevoNombre categorias

                    guardarCategorias categoriasActualizadas
                    putStrLn "Categoria actualizada correctamente."
                    menuCategorias categoriasActualizadas registros

-- Elimina una categoría si no está siendo usada por registros financieros.
eliminarCategoriaDesdeConsola :: [Categoria] -> [RegistroFinanciero] -> IO [Categoria]
eliminarCategoriaDesdeConsola categorias registros = do
    putStr "Ingrese el ID de la categoria a eliminar: "
    hFlush stdout
    textoId <- getLine

    case readMaybe textoId :: Maybe Int of
        Nothing -> do
            putStrLn "ID invalido. Debe ingresar un numero."
            menuCategorias categorias registros

        Just idBuscado ->
            case buscarCategoriaPorId idBuscado categorias of
                Nothing -> do
                    putStrLn "No existe una categoria con ese ID."
                    menuCategorias categorias registros

                Just _ ->
                    if puedeEliminarCategoria idBuscado registros
                        then do
                            let categoriasActualizadas =
                                    eliminarCategoria idBuscado categorias registros

                            guardarCategorias categoriasActualizadas
                            putStrLn "Categoria eliminada correctamente."
                            menuCategorias categoriasActualizadas registros
                        else do
                            putStrLn "No se puede eliminar la categoria porque esta siendo usada por registros financieros."
                            menuCategorias categorias registros

-- Muestra una lista de categorías.
mostrarCategorias :: [Categoria] -> IO ()
mostrarCategorias [] = putStrLn "No hay categorias registradas."
mostrarCategorias (categoria:resto) = do
    mostrarCategoria categoria
    mostrarCategorias resto

-- Muestra una categoría individual.
mostrarCategoria :: Categoria -> IO ()
mostrarCategoria categoria = do
    putStrLn ("ID: " ++ show (idCategoria categoria)
        ++ " | Nombre: " ++ nombreCategoria categoria)