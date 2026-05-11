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
    let categoriasConId = zip [1..] (filter (not . null) lineas)
    return [Categoria idCat nombre | (idCat, nombre) <- categoriasConId]

guardarCategorias :: [Categoria] -> IO ()
guardarCategorias categorias =
    guardarLineasArchivo rutaCategorias (map nombreCategoria categorias)

-- Carga la lista de presupuestos desde presupuestos.txt usando Read/Show (1 presupuesto por línea).
cargarPresupuestos :: IO [Presupuesto]
cargarPresupuestos = do
    lineas <- leerLineasArchivoSeguro rutaPresupuestos
    let presupuestos = [p | Just p <- map lineaAPresupuesto lineas]
    return presupuestos
  where
    -- Convierte una línea del archivo a un Presupuesto; si falla el parseo retorna Nothing.
    lineaAPresupuesto :: String -> Maybe Presupuesto
    lineaAPresupuesto linea =
        case reads linea of
            [(p, "")] -> Just p
            _         -> Nothing

-- Guarda la lista de presupuestos en presupuestos.txt usando Read/Show (1 presupuesto por línea).
guardarPresupuestos :: [Presupuesto] -> IO ()
guardarPresupuestos presupuestos =
    guardarLineasArchivo rutaPresupuestos (map show presupuestos)

-- Carga la configuración de reglas desde reglas.txt usando Read/Show.
cargarReglas :: IO [ConfiguracionRegla]
cargarReglas = do
    lineas <- leerLineasArchivoSeguro rutaReglas
    let reglas = [r | Just r <- map lineaARegla lineas]
    return reglas
  where
    lineaARegla :: String -> Maybe ConfiguracionRegla
    lineaARegla linea =
        case reads linea of
            [(r, "")] -> Just r
            _         -> Nothing

-- Guarda la configuración de reglas en reglas.txt usando Read/Show.
guardarReglas :: [ConfiguracionRegla] -> IO ()
guardarReglas reglas =
    guardarLineasArchivo rutaReglas (map show reglas)
