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
