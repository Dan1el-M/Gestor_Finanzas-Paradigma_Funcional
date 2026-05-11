module Main where

import FileManager
import UI.MainMenu (iniciarAplicacion)

-- Punto de entrada del programa.
-- Carga las categorías desde data/categorias.txt
-- y los registros financieros, luego inicia la aplicación.
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

    -- Inicia la aplicación con los datos cargados
    iniciarAplicacion categoriasIniciales registrosIniciales
