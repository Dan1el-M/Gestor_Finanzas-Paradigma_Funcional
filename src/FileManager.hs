module FileManager where

import Control.DeepSeq (force)
import Control.Exception (catch, evaluate, throwIO)
import System.IO.Error (isDoesNotExistError)

import Models

-- Este modulo maneja la persistencia en archivos .txt.

rutaRegistros :: FilePath
rutaRegistros = "data/registros.txt"

rutaPresupuestos :: FilePath
rutaPresupuestos = "data/presupuestos.txt"

rutaReglas :: FilePath
rutaReglas = "data/reglas.txt"

rutaCategorias :: FilePath
rutaCategorias = "data/categorias.txt"

leerLineasArchivo :: FilePath -> IO [String]
leerLineasArchivo ruta = do
    contenido <- readFile ruta
    evaluate (force (lines contenido))

leerLineasArchivoSeguro :: FilePath -> IO [String]
leerLineasArchivoSeguro ruta =
    leerLineasArchivo ruta `catch` manejarError
  where
    manejarError e
        | isDoesNotExistError e = return []
        | otherwise             = throwIO e

guardarLineasArchivo :: FilePath -> [String] -> IO ()
guardarLineasArchivo ruta lineas =
    writeFile ruta (unlines lineas)

cargarCategorias :: IO [Categoria]
cargarCategorias = do
    lineas <- leerLineasArchivoSeguro rutaCategorias
    let lineasNoVacias = filter (not . null) lineas
    let categoriasParseadas = [c | Just c <- map lineaACategoria lineasNoVacias]
    if length categoriasParseadas == length lineasNoVacias
        then return categoriasParseadas
        else do
            -- Compatibilidad: formato viejo (1 nombre por línea, IDs auto-asignados).
            -- Importante: este formato no preserva IDs al eliminar/reordenar categorías.
            let categoriasConId = zip [1..] lineasNoVacias
            return [Categoria idCat nombre | (idCat, nombre) <- categoriasConId]
  where
    -- Convierte una línea del archivo a una Categoria; si falla el parseo retorna Nothing.
    lineaACategoria :: String -> Maybe Categoria
    lineaACategoria linea =
        case reads linea of
            [(c, "")] -> Just c
            _         -> Nothing

guardarCategorias :: [Categoria] -> IO ()
guardarCategorias categorias =
    -- Guardar con Read/Show para preservar IDs de categoría (evita desincronización con registros/presupuestos).
    guardarLineasArchivo rutaCategorias (map show categorias)

-- Carga la lista de presupuestos desde presupuestos.txt usando Read/Show (1 presupuesto por línea).
cargarPresupuestos :: IO [Presupuesto]
cargarPresupuestos = do
    lineas <- leerLineasArchivoSeguro rutaPresupuestos
    let lineasLimpias = map (filter (/= '\r')) lineas
    let presupuestos = [p | Just p <- map lineaAPresupuesto lineasLimpias]
    return presupuestos
  where
    -- Convierte una línea del archivo a un Presupuesto; si falla el parseo retorna Nothing.
    lineaAPresupuesto :: String -> Maybe Presupuesto
    lineaAPresupuesto linea =
        case reads linea of
            [(p, resto)] | all (`elem` " \t") resto -> Just p
            _         -> Nothing

-- Guarda la lista de presupuestos en presupuestos.txt usando Read/Show (1 presupuesto por línea).
guardarPresupuestos :: [Presupuesto] -> IO ()
guardarPresupuestos presupuestos =
    guardarLineasArchivo rutaPresupuestos (map show presupuestos)
