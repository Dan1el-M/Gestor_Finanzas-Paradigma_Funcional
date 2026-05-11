module UI.MainMenu where

import System.IO (hFlush, stdout)
import UI.FinanceRegistryMenu (menuRegistroFinanciero)
import UI.BudgetMenu (menuPresupuestos)

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
    putStrLn "2. Gestionar presupuestos"
    putStrLn "3. Evaluar reglas y alertas"
    putStrLn "4. Ver analisis financiero"
    putStrLn "5. Simular escenario financiero"
    putStrLn "6. Generar reportes"
    putStrLn "7. Salir"
    putStr "Opcion: "
    hFlush stdout

    opcion <- getLine
    ejecutarOpcion opcion

ejecutarOpcion :: String -> IO ()
-- Ejecuta la opción elegida del menú principal y regresa al menú cuando corresponde.
ejecutarOpcion opcion =
    case opcion of
        "1" -> do
            menuRegistroFinanciero
            menuPrincipal

        "2" -> do
            menuPresupuestos
            menuPrincipal

        "3" -> do
            putStrLn "Modulo de reglas pendiente."
            menuPrincipal

        "4" -> do
            putStrLn "Modulo de analisis pendiente."
            menuPrincipal

        "5" -> do
            putStrLn "Modulo de simulacion pendiente."
            menuPrincipal

        "6" -> do
            putStrLn "Modulo de reportes pendiente."
            menuPrincipal

        "7" ->
            putStrLn "Saliendo del sistema..."

        _ -> do
            putStrLn "Opcion invalida. Intente de nuevo."
            menuPrincipal
