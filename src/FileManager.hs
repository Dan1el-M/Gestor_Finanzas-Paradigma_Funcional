--Aquí va la lectura y escritura de archivos.

module FileManager where

import Control.Exception (catch, throwIO)
import System.IO.Error (isDoesNotExistError)
import Control.DeepSeq (force)
import Control.Exception (evaluate)

{-

Guardar registros en data/registros.txt
Cargar registros al iniciar
Guardar presupuestos
Cargar categorías

-}

-- Este módulo manejará la persistencia en archivos .txt.
-- Aquí irán funciones para guardar y cargar registros,
-- presupuestos, reglas y categorías.

-- Rutas principales de los archivos de datos.
rutaRegistros :: FilePath
rutaRegistros = "data/registros.txt"

rutaPresupuestos :: FilePath
rutaPresupuestos = "data/presupuestos.txt"

rutaReglas :: FilePath
rutaReglas = "data/reglas.txt"

rutaCategorias :: FilePath
rutaCategorias = "data/categorias.txt"

-- Lee todas las líneas de un archivo.
-- Esta función será útil para cargar datos guardados.
leerLineasArchivo :: FilePath -> IO [String]
leerLineasArchivo ruta = do
    contenido <- readFile ruta
    evaluate (force (lines contenido))  -- fuerza lectura completa y cierra el archivo

leerLineasArchivoSeguro :: FilePath -> IO [String]
leerLineasArchivoSeguro ruta =
    leerLineasArchivo ruta `catch` manejarError
  where
    manejarError e
        | isDoesNotExistError e = return []
        | otherwise             = throwIO e

-- Guarda una lista de líneas en un archivo.
-- Cada elemento de la lista se guarda en una línea separada.
guardarLineasArchivo :: FilePath -> [String] -> IO ()
guardarLineasArchivo ruta lineas =
    writeFile ruta (unlines lineas)