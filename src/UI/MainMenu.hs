module UI.MainMenu where

import UI.ReportMenu (menuReportes)
import UI.FinanceRegistryMenu (menuRegistroFinanciero)
import UI.BudgetMenu (menuPresupuestos)
import UI.FinanceAnalysisMenu (menuAnalisisFinanciero)
import UI.UIHelpers (cerrar, enCaja, err, menuOpciones, opcion, promptOpcion, titulo)
import UI.SimulationMenu (menuSimulacion)
import UI.RuleMenu (menuReglas)

iniciarAplicacion :: IO ()
iniciarAplicacion = do
    titulo "FinanTrack Haskell"
    enCaja "Sistema de Finanzas Personales"
    cerrar
    menuPrincipal

menuPrincipal :: IO ()
menuPrincipal = do
    menuOpciones "Menu Principal"
        [ opcion 1 "Gestionar registros financieros"
        , opcion 2 "Gestionar presupuestos"
        , opcion 3 "Analisis financiero avanzado"
        , opcion 4 "Simular escenario financiero"
        , opcion 5 "Evaluar reglas y alertas"
        , opcion 6 "Generar reportes"
        , opcion 7 "Salir"
        ]
    seleccion <- promptOpcion
    ejecutarOpcion seleccion

ejecutarOpcion :: String -> IO ()
-- Ejecuta la opción elegida del menú principal y regresa al menú cuando corresponde.
ejecutarOpcion seleccion =
    case seleccion of
        "1" -> menuRegistroFinanciero >> menuPrincipal
        "2" -> menuPresupuestos >> menuPrincipal
        "3" -> menuAnalisisFinanciero >> menuPrincipal
        "4" -> menuSimulacion >> menuPrincipal
        "5" -> menuReglas >> menuPrincipal
        "6" -> menuReportes >> menuPrincipal
        "7" -> do
            titulo "Hasta luego"
            enCaja "Fin del programa."
            cerrar
        _   -> err "Opcion invalida." >> menuPrincipal
