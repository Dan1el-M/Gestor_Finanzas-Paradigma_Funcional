module Services.SimulationService where

import Models

data ResultadoReduccion = ResultadoReduccion
    { gastoActualReduccion :: Double
    , porcentajeReduccion :: Double
    , nuevoGastoEstimado :: Double
    , ahorroGeneradoReduccion :: Double
    } deriving (Show, Eq)

data ResultadoProyeccionAhorro = ResultadoProyeccionAhorro
    { promedioAhorro :: Double
    , mesesProyeccion :: Int
    , ahorroProyectado :: Double
    } deriving (Show, Eq)

totalGastos :: [RegistroFinanciero] -> Double
totalGastos registros =
    sum (map montoRegistro (filter esGasto registros))
  where
    esGasto registro = tipoRegistro registro == Gasto

totalGastosPorCategoria :: Int -> [RegistroFinanciero] -> Double
totalGastosPorCategoria idCategoriaBuscada registros =
    totalGastos (filter perteneceACategoria registros)
  where
    perteneceACategoria registro = idCategoriaRegistro registro == idCategoriaBuscada

simularReduccionGastos :: Double -> [RegistroFinanciero] -> Either String ResultadoReduccion
simularReduccionGastos porcentaje registros
    | porcentaje <= 0 = Left "El porcentaje de reduccion debe ser mayor que cero."
    | otherwise =
        let gastoActual = totalGastos registros
            ahorroGenerado = gastoActual * porcentaje / 100
            nuevoGasto = gastoActual - ahorroGenerado
        in Right ResultadoReduccion
            { gastoActualReduccion = gastoActual
            , porcentajeReduccion = porcentaje
            , nuevoGastoEstimado = nuevoGasto
            , ahorroGeneradoReduccion = ahorroGenerado
            }

simularReduccionGastosPorCategoria :: Int -> Double -> [RegistroFinanciero] -> Either String ResultadoReduccion
simularReduccionGastosPorCategoria idCategoriaBuscada porcentaje registros
    | porcentaje <= 0 = Left "El porcentaje de reduccion debe ser mayor que cero."
    | otherwise =
        let gastoActual = totalGastosPorCategoria idCategoriaBuscada registros
            ahorroGenerado = gastoActual * porcentaje / 100
            nuevoGasto = gastoActual - ahorroGenerado
        in Right ResultadoReduccion
            { gastoActualReduccion = gastoActual
            , porcentajeReduccion = porcentaje
            , nuevoGastoEstimado = nuevoGasto
            , ahorroGeneradoReduccion = ahorroGenerado
            }

promedioAhorros :: [RegistroFinanciero] -> Double
promedioAhorros registros =
    promedioMontos (filter esAhorro registros)
  where
    esAhorro registro = tipoRegistro registro == Ahorro

promedioAhorrosPorCategoria :: Int -> [RegistroFinanciero] -> Double
promedioAhorrosPorCategoria idCategoriaBuscada registros =
    promedioAhorros (filter perteneceACategoria registros)
  where
    perteneceACategoria registro = idCategoriaRegistro registro == idCategoriaBuscada

promedioMontos :: [RegistroFinanciero] -> Double
promedioMontos [] = 0
promedioMontos registros =
    sum (map montoRegistro registros) / fromIntegral (length registros)

proyectarAhorroDesdePromedio :: Int -> [RegistroFinanciero] -> Either String ResultadoProyeccionAhorro
proyectarAhorroDesdePromedio cantidadMeses registros =
    construirProyeccion cantidadMeses (promedioAhorros registros)

proyectarAhorroDesdePromedioPorCategoria :: Int -> Int -> [RegistroFinanciero] -> Either String ResultadoProyeccionAhorro
proyectarAhorroDesdePromedioPorCategoria idCategoriaBuscada cantidadMeses registros =
    construirProyeccion cantidadMeses (promedioAhorrosPorCategoria idCategoriaBuscada registros)

construirProyeccion :: Int -> Double -> Either String ResultadoProyeccionAhorro
construirProyeccion cantidadMeses promedio
    | cantidadMeses <= 0 = Left "La cantidad de meses debe ser mayor que cero."
    | promedio <= 0 = Left "No hay registros de ahorro para calcular un promedio valido."
    | otherwise =
        Right ResultadoProyeccionAhorro
            { promedioAhorro = promedio
            , mesesProyeccion = cantidadMeses
            , ahorroProyectado = promedio * fromIntegral cantidadMeses
            }

proyectarAhorro :: Double -> Int -> Either String Double
proyectarAhorro ahorroMensual cantidadMeses
    | ahorroMensual <= 0 = Left "El ahorro mensual debe ser mayor que cero."
    | cantidadMeses <= 0 = Left "La cantidad de meses debe ser mayor que cero."
    | otherwise = Right (ahorroMensual * fromIntegral cantidadMeses)
