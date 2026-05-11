module Services.BudgetService where

import Models

-- Convierte un presupuesto a una línea de texto para persistirlo en presupuestos.txt.
presupuestoALinea :: Presupuesto -> String
presupuestoALinea = show

-- Convierte una línea de presupuestos.txt a un Presupuesto; si el parseo falla retorna Nothing.
lineaAPresupuesto :: String -> Maybe Presupuesto
lineaAPresupuesto linea =
    case reads linea of
        [(p, "")] -> Just p
        _         -> Nothing

-- Calcula el siguiente ID de presupuesto en base al máximo existente.
siguienteIdPresupuesto :: [Presupuesto] -> Int
siguienteIdPresupuesto [] = 1
siguienteIdPresupuesto presupuestos = maximum (map idPresupuesto presupuestos) + 1

-- Garantiza que exista un presupuesto (por defecto 0) para cada categoría.
-- Si una categoría no tiene presupuesto, se crea con tipo LimiteMaximo y monto 0.
asegurarPresupuestosPorDefecto :: [Categoria] -> [Presupuesto] -> [Presupuesto]
asegurarPresupuestosPorDefecto categorias presupuestos =
    foldl agregarSiFalta presupuestos categorias
  where
    agregarSiFalta :: [Presupuesto] -> Categoria -> [Presupuesto]
    agregarSiFalta acc categoria =
        case buscarPresupuestoPorCategoria (idCategoria categoria) acc of
            Just _  -> acc
            Nothing ->
                let nuevo = Presupuesto
                        { idPresupuesto = siguienteIdPresupuesto acc
                        , idCategoriaPresupuesto = idCategoria categoria
                        , montoPresupuesto = 0
                        , tipoPresupuesto = LimiteMaximo
                        }
                in acc ++ [nuevo]

-- Busca un presupuesto usando el ID de la categoría.
buscarPresupuestoPorCategoria :: Int -> [Presupuesto] -> Maybe Presupuesto
buscarPresupuestoPorCategoria _ [] = Nothing
buscarPresupuestoPorCategoria idCategoriaBuscada (p:ps)
    | idCategoriaPresupuesto p == idCategoriaBuscada = Just p
    | otherwise = buscarPresupuestoPorCategoria idCategoriaBuscada ps

-- Actualiza (o crea) el presupuesto de una categoría con un nuevo monto.
-- Mantiene el tipo de presupuesto existente; si no existía, lo crea como LimiteMaximo.
asignarPresupuestoCategoria :: Int -> Double -> [Presupuesto] -> [Presupuesto]
asignarPresupuestoCategoria idCat montoNuevo presupuestos =
    case buscarPresupuestoPorCategoria idCat presupuestos of
        Nothing ->
            presupuestos ++
                [ Presupuesto
                    { idPresupuesto = siguienteIdPresupuesto presupuestos
                    , idCategoriaPresupuesto = idCat
                    , montoPresupuesto = montoNuevo
                    , tipoPresupuesto = LimiteMaximo
                    }
                ]
        Just existente ->
            map actualizar presupuestos
          where
            actualizar p
                | idPresupuesto p == idPresupuesto existente =
                    p { montoPresupuesto = montoNuevo }
                | otherwise = p

-- Suma los montos de los registros de tipo Gasto por categoría.
acumuladoGastosPorCategoria :: [RegistroFinanciero] -> [(Int, Double)]
acumuladoGastosPorCategoria registros =
    foldl acumular [] (filter (\r -> tipoRegistro r == Gasto) registros)
  where
    acumular :: [(Int, Double)] -> RegistroFinanciero -> [(Int, Double)]
    acumular acc r = sumarMonto (idCategoriaRegistro r) (montoRegistro r) acc

    sumarMonto :: Int -> Double -> [(Int, Double)] -> [(Int, Double)]
    sumarMonto idCat monto [] = [(idCat, monto)]
    sumarMonto idCat monto ((c, total):xs)
        | c == idCat = (c, total + monto) : xs
        | otherwise  = (c, total) : sumarMonto idCat monto xs

-- Obtiene el gasto real acumulado de una categoría (0 si no hay registros).
gastoRealCategoria :: Int -> [RegistroFinanciero] -> Double
gastoRealCategoria idCat registros =
    case lookup idCat (acumuladoGastosPorCategoria registros) of
        Nothing -> 0
        Just x  -> x

-- Obtiene el monto presupuestado de una categoría (0 si no existe).
presupuestoCategoria :: Int -> [Presupuesto] -> Double
presupuestoCategoria idCat presupuestos =
    case buscarPresupuestoPorCategoria idCat presupuestos of
        Nothing -> 0
        Just p  -> montoPresupuesto p

-- Cruza categorías + registros + presupuestos y genera una comparación por categoría.
-- Retorna: (Categoría, gastoReal, montoPresupuestado, diferencia = presupuesto - real).
compararRealVsPresupuesto :: [Categoria] -> [Presupuesto] -> [RegistroFinanciero] -> [(Categoria, Double, Double, Double)]
compararRealVsPresupuesto categorias presupuestos registros =
    map construir categorias
  where
    construir categoria =
        let idCat = idCategoria categoria
            real = gastoRealCategoria idCat registros
            presupuesto = presupuestoCategoria idCat presupuestos
            diferencia = presupuesto - real
        in (categoria, real, presupuesto, diferencia)

-- Determina si al agregar un nuevo registro (solo si es Gasto) se excede el presupuesto de la categoría.
-- Nota: si el presupuesto está en 0, se interpreta como "no configurado" y no genera alerta.
excedePresupuestoConNuevoRegistro :: [Presupuesto] -> [RegistroFinanciero] -> RegistroFinanciero -> Bool
excedePresupuestoConNuevoRegistro presupuestos existentes nuevo =
    case tipoRegistro nuevo of
        Gasto ->
            case buscarPresupuestoPorCategoria (idCategoriaRegistro nuevo) presupuestos of
                Nothing -> False
                Just p ->
                    case tipoPresupuesto p of
                        MetaMinima   -> False
                        LimiteMaximo ->
                            let limite = montoPresupuesto p
                                realActual = gastoRealCategoria (idCategoriaRegistro nuevo) existentes
                                realNuevo = realActual + montoRegistro nuevo
                            in limite > 0 && realNuevo > limite
        _ -> False
