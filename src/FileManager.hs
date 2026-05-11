--Aquí va la lectura y escritura de archivos.

{-

Guardar registros en data/registros.txt
Cargar registros al iniciar
Guardar presupuestos
Cargar categorías

-}

module FileManager where

import Models

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
    return (lines contenido)

-- Guarda una lista de líneas en un archivo.
-- Cada elemento de la lista se guarda en una línea separada.
guardarLineasArchivo :: FilePath -> [String] -> IO ()
guardarLineasArchivo ruta lineas =
    writeFile ruta (unlines lineas)

-- Carga categorías desde data/categorias.txt
-- Asigna IDs automáticamente comenzando desde 1
cargarCategorias :: IO [Categoria]
cargarCategorias = do
    lineas <- leerLineasArchivo rutaCategorias
    let categoriasConId = zip [1..] (filter (not . null) lineas)
    return [Categoria idCat nombre | (idCat, nombre) <- categoriasConId]

-- Guarda categorías en data/categorias.txt
guardarCategorias :: [Categoria] -> IO ()
guardarCategorias categorias =
    guardarLineasArchivo rutaCategorias (map nombreCategoria categorias)