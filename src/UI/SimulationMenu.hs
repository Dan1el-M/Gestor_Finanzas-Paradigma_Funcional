module UI.SimulationMenu where

import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Services.FinanceRegistryService (cargarRegistros)
import qualified Services.SimulationService as Service
import UI.CategoryMenu (pedirIdCategoria)

menuSimulacion :: IO ()
menuSimulacion = do
    putStrLn ""
    putStrLn "===== Simulacion financiera ====="
    putStrLn "1. Simular reduccion de gastos"
    putStrLn "2. Proyectar ahorro en el tiempo"
    putStrLn "3. Volver al menu principal"
    putStr "Opcion: "
    hFlush stdout

    opcion <- getLine
    case opcion of
        "1" -> simularReduccionMenu >> menuSimulacion
        "2" -> proyectarAhorroMenu >> menuSimulacion
        "3" -> putStrLn "Volviendo al menu principal..."
        _ -> do
            putStrLn "Opcion invalida. Intente de nuevo."
            menuSimulacion

simularReduccionMenu :: IO ()
simularReduccionMenu = do
    putStrLn ""
    putStrLn "===== Simular reduccion de gastos ====="
    putStrLn "1. Aplicar a todos los gastos"
    putStrLn "2. Aplicar a gastos de una categoria"
    putStrLn "3. Volver al menu de simulacion"
    putStr "Opcion: "
    hFlush stdout
    opcion <- getLine

    case opcion of
        "1" -> simularReduccionTodosLosGastos
        "2" -> simularReduccionPorCategoria
        "3" -> putStrLn "Volviendo al menu de simulacion..."
        _ -> putStrLn "Opcion invalida. Regresando al menu de simulacion."

simularReduccionTodosLosGastos :: IO ()
simularReduccionTodosLosGastos = do
    putStr "Porcentaje de reduccion: "
    hFlush stdout
    textoPorcentaje <- getLine

    case readMaybe textoPorcentaje :: Maybe Double of
        Nothing ->
            putStrLn "Porcentaje invalido. Debe ingresar un numero positivo."

        Just porcentaje -> do
            registros <- cargarRegistros
            case Service.simularReduccionGastos porcentaje registros of
                Left mensaje ->
                    putStrLn mensaje

                Right resultado -> do
                    putStrLn ""
                    putStrLn "Resultado de la simulacion:"
                    putStrLn $ "Gastos actuales: " ++ formatoMonto (Service.gastoActualReduccion resultado)
                    putStrLn $ "Porcentaje de reduccion: " ++ formatoPorcentaje (Service.porcentajeReduccion resultado)
                    putStrLn $ "Nuevo gasto estimado: " ++ formatoMonto (Service.nuevoGastoEstimado resultado)
                    putStrLn $ "Ahorro generado por la reduccion: " ++ formatoMonto (Service.ahorroGeneradoReduccion resultado)
                    putStrLn "Esta simulacion no modifica los registros guardados."

simularReduccionPorCategoria :: IO ()
simularReduccionPorCategoria = do
    putStrLn ""
    putStrLn "Seleccione la categoria de gastos a simular:"
    idCategoria <- pedirIdCategoria
    putStr "Porcentaje de reduccion: "
    hFlush stdout
    textoPorcentaje <- getLine

    case readMaybe textoPorcentaje :: Maybe Double of
        Nothing ->
            putStrLn "Porcentaje invalido. Debe ingresar un numero positivo."

        Just porcentaje -> do
            registros <- cargarRegistros
            case Service.simularReduccionGastosPorCategoria idCategoria porcentaje registros of
                Left mensaje ->
                    putStrLn mensaje

                Right resultado -> do
                    putStrLn ""
                    putStrLn "Resultado de la simulacion por categoria:"
                    putStrLn $ "ID de categoria: " ++ show idCategoria
                    putStrLn $ "Gastos actuales de la categoria: " ++ formatoMonto (Service.gastoActualReduccion resultado)
                    putStrLn $ "Porcentaje de reduccion: " ++ formatoPorcentaje (Service.porcentajeReduccion resultado)
                    putStrLn $ "Nuevo gasto estimado de la categoria: " ++ formatoMonto (Service.nuevoGastoEstimado resultado)
                    putStrLn $ "Ahorro generado por la reduccion: " ++ formatoMonto (Service.ahorroGeneradoReduccion resultado)
                    putStrLn "Esta simulacion no modifica los registros guardados."

proyectarAhorroMenu :: IO ()
proyectarAhorroMenu = do
    putStrLn ""
    putStrLn "===== Proyectar ahorro en el tiempo ====="
    putStrLn "1. Proyectar con todos los ahorros"
    putStrLn "2. Proyectar con ahorros de una categoria"
    putStrLn "3. Volver al menu de simulacion"
    putStr "Opcion: "
    hFlush stdout
    opcion <- getLine

    case opcion of
        "1" -> proyectarAhorroGeneral
        "2" -> proyectarAhorroPorCategoria
        "3" -> putStrLn "Volviendo al menu de simulacion..."
        _ -> putStrLn "Opcion invalida. Regresando al menu de simulacion."

proyectarAhorroGeneral :: IO ()
proyectarAhorroGeneral = do
    putStr "Cantidad de meses: "
    hFlush stdout
    textoMeses <- getLine

    case readMaybe textoMeses :: Maybe Int of
        Nothing ->
            putStrLn "Cantidad de meses invalida. Debe ingresar un numero positivo."

        Just cantidadMeses -> do
            registros <- cargarRegistros
            case Service.proyectarAhorroDesdePromedio cantidadMeses registros of
                Left mensaje ->
                    putStrLn mensaje

                Right resultado -> do
                    putStrLn ""
                    putStrLn "Resultado de la proyeccion general:"
                    putStrLn $ "Promedio de ahorros registrados: " ++ formatoMonto (Service.promedioAhorro resultado)
                    putStrLn $ "Cantidad de meses: " ++ show (Service.mesesProyeccion resultado)
                    putStrLn $ "Ahorro proyectado: " ++ formatoMonto (Service.ahorroProyectado resultado)
                    putStrLn "Esta proyeccion no modifica los registros guardados."

proyectarAhorroPorCategoria :: IO ()
proyectarAhorroPorCategoria = do
    putStrLn ""
    putStrLn "Seleccione la categoria de ahorros a proyectar:"
    idCategoria <- pedirIdCategoria
    putStr "Cantidad de meses: "
    hFlush stdout
    textoMeses <- getLine

    case readMaybe textoMeses :: Maybe Int of
        Nothing ->
            putStrLn "Cantidad de meses invalida. Debe ingresar un numero positivo."

        Just cantidadMeses -> do
            registros <- cargarRegistros
            case Service.proyectarAhorroDesdePromedioPorCategoria idCategoria cantidadMeses registros of
                Left mensaje ->
                    putStrLn mensaje

                Right resultado -> do
                    putStrLn ""
                    putStrLn "Resultado de la proyeccion por categoria:"
                    putStrLn $ "ID de categoria: " ++ show idCategoria
                    putStrLn $ "Promedio de ahorros de la categoria: " ++ formatoMonto (Service.promedioAhorro resultado)
                    putStrLn $ "Cantidad de meses: " ++ show (Service.mesesProyeccion resultado)
                    putStrLn $ "Ahorro proyectado: " ++ formatoMonto (Service.ahorroProyectado resultado)
                    putStrLn "Esta proyeccion no modifica los registros guardados."

formatoMonto :: Double -> String
formatoMonto monto = printf "CRC %.2f" monto

formatoPorcentaje :: Double -> String
formatoPorcentaje porcentaje = printf "%.2f%%" porcentaje
