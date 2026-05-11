module UI.CategoryMenu where

import System.IO (hFlush, stdout)

menuCategoria :: IO ()
menuCategoria = do
    putStrLn "===================================="
    putStrLn "        FinanTrack Haskell"
    putStrLn " Sistema de Finanzas Personales"
    putStrLn "===================================="
    opcionesCategoria



opcionesCategoria :: IO ()
opcionesCategoria = do
    putStrLn ""
    putStrLn "Seleccione una opcion:"
    putStrLn "1. "
    putStrLn "2. Ver registros financieros"
    putStrLn "9. Salir"
    putStr "Opcion: "
    hFlush stdout

    opcion <- getLine
    ejecutarOpcion opcion

-- Ejecuta la acción correspondiente según la opción ingresada.
-- Por ahora algunas opciones son placeholders,
-- luego cada compañero conectará aquí sus funciones.
ejecutarOpcion :: String -> IO ()
ejecutarOpcion opcion =
    case opcion of
        "1" -> do
            putStrLn "Modulo de registro financiero pendiente de implementar."
            menuCategoria

        _ -> do
            putStrLn "Opcion invalida. Intente de nuevo."
            menuCategoria