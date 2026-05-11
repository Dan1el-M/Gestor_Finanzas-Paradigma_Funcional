module UI.MainMenu where

import System.IO (hFlush, stdout)
import UI.CategoryMenu (menuCategoria)
import UI.FinanceRegistryMenu (menuRegistroFinanciero)

iniciarAplicacion :: IO ()
iniciarAplicacion = do
    putStrLn "===================================="
    putStrLn "        FinanTrack Haskell"
    putStrLn " Sistema de Finanzas Personales"
    putStrLn "===================================="
    menuPrincipal

menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn ""
    putStrLn "Seleccione una opcion:"
    putStrLn "1. Gestionar registros financieros"
    putStrLn "3. Gestionar presupuestos"
    putStrLn "4. Evaluar reglas y alertas"
    putStrLn "5. Ver analisis financiero"
    putStrLn "6. Simular escenario financiero"
    putStrLn "7. Generar reportes"
    putStrLn "9. CRUD Categorias"
    putStrLn "10. Salir"
    putStr "Opcion: "
    hFlush stdout

    opcion <- getLine
    ejecutarOpcion opcion categorias registros

ejecutarOpcion :: String -> IO ()
ejecutarOpcion opcion =
    case opcion of
        "1" -> do
            menuRegistroFinanciero
            menuPrincipal

        "3" -> do
            putStrLn "Modulo de presupuestos pendiente."
            menuPrincipal

        "4" -> do
            putStrLn "Modulo de reglas pendiente."
            menuPrincipal

        "5" -> do
            putStrLn "Modulo de analisis pendiente."
            menuPrincipal

        "6" -> do
            putStrLn "Modulo de simulacion pendiente."
            menuPrincipal

        "7" -> do
            putStrLn "Modulo de reportes pendiente."
            menuPrincipal

        "9" -> do
            menuCategoria
            menuPrincipal

        "10" ->
            putStrLn "Saliendo del sistema..."

        _ -> do
            putStrLn "Opcion invalida. Intente de nuevo."
            menuPrincipal categorias registros