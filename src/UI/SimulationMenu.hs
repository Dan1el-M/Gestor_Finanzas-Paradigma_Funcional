module UI.SimulationMenu where

import Text.Printf (printf)
import Text.Read (readMaybe)

import Services.FinanceRegistryService (cargarRegistros)
import qualified Services.SimulationService as Service
import UI.CategoryMenu (pedirIdCategoria)
import UI.UIHelpers
    ( cerrar, enCaja, err, menuOpciones, mostrarMonto, ok, opcion, prompt
    , promptOpcion, titulo
    )

menuSimulacion :: IO ()
menuSimulacion = do
    menuOpciones "Simulacion Financiera"
        [ opcion 1 "Simular reduccion de gastos"
        , opcion 2 "Proyectar ahorro en el tiempo"
        , opcion 3 "Volver al menu principal"
        ]
    seleccion <- promptOpcion
    case seleccion of
        "1" -> simularReduccionMenu >> menuSimulacion
        "2" -> proyectarAhorroMenu >> menuSimulacion
        "3" -> ok "Volviendo al menu principal..."
        _ -> err "Opcion invalida. Intente de nuevo." >> menuSimulacion

simularReduccionMenu :: IO ()
simularReduccionMenu = do
    menuOpciones "Simular Reduccion de Gastos"
        [ opcion 1 "Aplicar a todos los gastos"
        , opcion 2 "Aplicar a gastos de una categoria"
        , opcion 3 "Volver al menu de simulacion"
        ]
    seleccion <- promptOpcion

    case seleccion of
        "1" -> simularReduccionTodosLosGastos
        "2" -> simularReduccionPorCategoria
        "3" -> ok "Volviendo al menu de simulacion..."
        _ -> err "Opcion invalida. Regresando al menu de simulacion."

simularReduccionTodosLosGastos :: IO ()
simularReduccionTodosLosGastos = do
    textoPorcentaje <- prompt "Porcentaje de reduccion"

    case readMaybe textoPorcentaje :: Maybe Double of
        Nothing ->
            err "Porcentaje invalido. Debe ingresar un numero positivo."

        Just porcentaje -> do
            registros <- cargarRegistros
            case Service.simularReduccionGastos porcentaje registros of
                Left mensaje ->
                    err mensaje

                Right resultado -> do
                    titulo "Resultado de la Simulacion"
                    enCaja $ "Gastos actuales: " ++ mostrarMonto (Service.gastoActualReduccion resultado)
                    enCaja $ "Porcentaje de reduccion: " ++ formatoPorcentaje (Service.porcentajeReduccion resultado)
                    enCaja $ "Nuevo gasto estimado: " ++ mostrarMonto (Service.nuevoGastoEstimado resultado)
                    enCaja $ "Ahorro generado: " ++ mostrarMonto (Service.ahorroGeneradoReduccion resultado)
                    enCaja "Esta simulacion no modifica los registros guardados."
                    cerrar

simularReduccionPorCategoria :: IO ()
simularReduccionPorCategoria = do
    titulo "Categoria a Simular"
    cerrar
    idCategoria <- pedirIdCategoria
    textoPorcentaje <- prompt "Porcentaje de reduccion"

    case readMaybe textoPorcentaje :: Maybe Double of
        Nothing ->
            err "Porcentaje invalido. Debe ingresar un numero positivo."

        Just porcentaje -> do
            registros <- cargarRegistros
            case Service.simularReduccionGastosPorCategoria idCategoria porcentaje registros of
                Left mensaje ->
                    err mensaje

                Right resultado -> do
                    titulo "Resultado por Categoria"
                    enCaja $ "ID de categoria: " ++ show idCategoria
                    enCaja $ "Gastos actuales: " ++ mostrarMonto (Service.gastoActualReduccion resultado)
                    enCaja $ "Porcentaje de reduccion: " ++ formatoPorcentaje (Service.porcentajeReduccion resultado)
                    enCaja $ "Nuevo gasto estimado: " ++ mostrarMonto (Service.nuevoGastoEstimado resultado)
                    enCaja $ "Ahorro generado: " ++ mostrarMonto (Service.ahorroGeneradoReduccion resultado)
                    enCaja "Esta simulacion no modifica los registros guardados."
                    cerrar

proyectarAhorroMenu :: IO ()
proyectarAhorroMenu = do
    menuOpciones "Proyectar Ahorro"
        [ opcion 1 "Proyectar con todos los ahorros"
        , opcion 2 "Proyectar con ahorros de una categoria"
        , opcion 3 "Volver al menu de simulacion"
        ]
    seleccion <- promptOpcion

    case seleccion of
        "1" -> proyectarAhorroGeneral
        "2" -> proyectarAhorroPorCategoria
        "3" -> ok "Volviendo al menu de simulacion..."
        _ -> err "Opcion invalida. Regresando al menu de simulacion."

proyectarAhorroGeneral :: IO ()
proyectarAhorroGeneral = do
    textoMeses <- prompt "Cantidad de meses"

    case readMaybe textoMeses :: Maybe Int of
        Nothing ->
            err "Cantidad de meses invalida. Debe ingresar un numero positivo."

        Just cantidadMeses -> do
            registros <- cargarRegistros
            case Service.proyectarAhorroDesdePromedio cantidadMeses registros of
                Left mensaje ->
                    err mensaje

                Right resultado -> do
                    titulo "Resultado de la Proyeccion"
                    enCaja $ "Promedio de ahorros: " ++ mostrarMonto (Service.promedioAhorro resultado)
                    enCaja $ "Cantidad de meses: " ++ show (Service.mesesProyeccion resultado)
                    enCaja $ "Ahorro proyectado: " ++ mostrarMonto (Service.ahorroProyectado resultado)
                    enCaja "Esta proyeccion no modifica los registros guardados."
                    cerrar

proyectarAhorroPorCategoria :: IO ()
proyectarAhorroPorCategoria = do
    titulo "Categoria a Proyectar"
    cerrar
    idCategoria <- pedirIdCategoria
    textoMeses <- prompt "Cantidad de meses"

    case readMaybe textoMeses :: Maybe Int of
        Nothing ->
            err "Cantidad de meses invalida. Debe ingresar un numero positivo."

        Just cantidadMeses -> do
            registros <- cargarRegistros
            case Service.proyectarAhorroDesdePromedioPorCategoria idCategoria cantidadMeses registros of
                Left mensaje ->
                    err mensaje

                Right resultado -> do
                    titulo "Resultado por Categoria"
                    enCaja $ "ID de categoria: " ++ show idCategoria
                    enCaja $ "Promedio de ahorros: " ++ mostrarMonto (Service.promedioAhorro resultado)
                    enCaja $ "Cantidad de meses: " ++ show (Service.mesesProyeccion resultado)
                    enCaja $ "Ahorro proyectado: " ++ mostrarMonto (Service.ahorroProyectado resultado)
                    enCaja "Esta proyeccion no modifica los registros guardados."
                    cerrar

formatoPorcentaje :: Double -> String
formatoPorcentaje porcentaje = printf "%.2f%%" porcentaje
