module UI.MainMenu where

import UI.ReportMenu (menuReportes)
import System.IO (hFlush, stdout)
import UI.FinanceRegistryMenu (menuRegistroFinanciero)
import UI.FinanceAnalysisMenu (menuAnalisisFinanciero)
import UI.UIHelpers (titulo, cerrar, ok, err)
import UI.SimulationMenu (menuSimulacion)

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
ejecutarOpcion opcion =
    case opcion of
        "1" -> menuRegistroFinanciero >> menuPrincipal
        "2" -> ok "Modulo de presupuestos pendiente." >> menuPrincipal
        "3" -> ok "Modulo de reglas pendiente." >> menuPrincipal
        "4" -> menuAnalisisFinanciero >> menuPrincipal
        "5" -> menuSimulacion >> menuPrincipal
        "6" -> menuReportes >> menuPrincipal
        "7" -> do
            putStrLn ""
            putStrLn "  ╔══════════════════════════════════════════════════╗"
            putStrLn "  ║         Hasta luego. Fin del programa.           ║"
            putStrLn "  ╚══════════════════════════════════════════════════╝"
        _   -> err "Opcion invalida." >> menuPrincipal