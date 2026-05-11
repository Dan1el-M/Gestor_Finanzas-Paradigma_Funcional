module UI.MainMenu where

import UI.ReportMenu (menuReportes)
import System.IO (hFlush, stdout)
import UI.FinanceRegistryMenu (menuRegistroFinanciero)
import UI.BudgetMenu (menuPresupuestos)
import UI.FinanceAnalysisMenu (menuAnalisisFinanciero)
import UI.UIHelpers (titulo, cerrar, err)
import UI.SimulationMenu (menuSimulacion)
import UI.RuleMenu (menuReglas)

iniciarAplicacion :: IO ()
iniciarAplicacion = do
    putStrLn ""
    putStrLn "  ╔══════════════════════════════════════════════════╗"
    putStrLn "  ║           FinanTrack Haskell                     ║"
    putStrLn "  ║      Sistema de Finanzas Personales              ║"
    putStrLn "  ╚══════════════════════════════════════════════════╝"
    menuPrincipal

menuPrincipal :: IO ()
menuPrincipal = do
    titulo "Menu Principal"
    putStrLn "  ║  1. Gestionar registros financieros              ║"
    putStrLn "  ║  2. Gestionar presupuestos                       ║"
    putStrLn "  ║  3. Evaluar reglas y alertas                     ║"
    putStrLn "  ║  4. Analisis financiero avanzado                 ║"
    putStrLn "  ║  5. Simular escenario financiero                 ║"
    putStrLn "  ║  6. Generar reportes                             ║"
    putStrLn "  ║  7. Salir                                        ║"
    cerrar
    putStr "  Opcion » "
    hFlush stdout
    opcion <- getLine
    ejecutarOpcion opcion

ejecutarOpcion :: String -> IO ()
-- Ejecuta la opción elegida del menú principal y regresa al menú cuando corresponde.
ejecutarOpcion opcion =
    case opcion of
        "1" -> menuRegistroFinanciero >> menuPrincipal
        "2" -> menuPresupuestos >> menuPrincipal
        "3" -> menuReglas >> menuPrincipal
        "4" -> menuAnalisisFinanciero >> menuPrincipal
        "5" -> menuSimulacion >> menuPrincipal
        "6" -> menuReportes >> menuPrincipal
        "7" -> do
            putStrLn ""
            putStrLn "  ╔══════════════════════════════════════════════════╗"
            putStrLn "  ║         Hasta luego. Fin del programa.           ║"
            putStrLn "  ╚══════════════════════════════════════════════════╝"
        _   -> err "Opcion invalida." >> menuPrincipal
