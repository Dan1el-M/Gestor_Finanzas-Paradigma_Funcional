module Services.RuleService where

import Models

nombreReglaGastoCategoria :: String
nombreReglaGastoCategoria = "Gasto por categoria"

nombreReglaAhorroMinimo :: String
nombreReglaAhorroMinimo = "Ahorro minimo"

reglaGastoPorDefecto :: ConfiguracionRegla
reglaGastoPorDefecto = ConfiguracionRegla
    { nombreRegla = nombreReglaGastoCategoria
    , idCategoriaRegla = 1
    , montoRegla = 0
    , tipoPresupuestoRegla = LimiteMaximo
    }

reglaAhorroPorDefecto :: ConfiguracionRegla
reglaAhorroPorDefecto = ConfiguracionRegla
    { nombreRegla = nombreReglaAhorroMinimo
    , idCategoriaRegla = 0
    , montoRegla = 0
    , tipoPresupuestoRegla = MetaMinima
    }

asegurarReglasPorDefecto :: [ConfiguracionRegla] -> [ConfiguracionRegla]
asegurarReglasPorDefecto reglas =
    [ obtenerRegla nombreReglaGastoCategoria reglaGastoPorDefecto reglas
    , obtenerRegla nombreReglaAhorroMinimo reglaAhorroPorDefecto reglas
    ]

obtenerRegla :: String -> ConfiguracionRegla -> [ConfiguracionRegla] -> ConfiguracionRegla
obtenerRegla nombre defecto reglas =
    case filter (\r -> nombreRegla r == nombre) reglas of
        (r:_) -> r
        []    -> defecto

configurarReglaGasto :: Int -> Double -> [ConfiguracionRegla] -> Either String [ConfiguracionRegla]
configurarReglaGasto idCategoriaBuscada montoLimite reglas
    | montoLimite <= 0 = Left "El monto limite debe ser mayor que cero."
    | otherwise =
        Right (reemplazarRegla nombreReglaGastoCategoria nuevaRegla (asegurarReglasPorDefecto reglas))
  where
    nuevaRegla = ConfiguracionRegla
        { nombreRegla = nombreReglaGastoCategoria
        , idCategoriaRegla = idCategoriaBuscada
        , montoRegla = montoLimite
        , tipoPresupuestoRegla = LimiteMaximo
        }

configurarReglaAhorro :: Double -> [ConfiguracionRegla] -> Either String [ConfiguracionRegla]
configurarReglaAhorro montoMinimo reglas
    | montoMinimo <= 0 = Left "El monto minimo de ahorro debe ser mayor que cero."
    | otherwise =
        Right (reemplazarRegla nombreReglaAhorroMinimo nuevaRegla (asegurarReglasPorDefecto reglas))
  where
    reglaActual = obtenerRegla nombreReglaAhorroMinimo reglaAhorroPorDefecto reglas
    nuevaRegla = reglaActual { montoRegla = montoMinimo, tipoPresupuestoRegla = MetaMinima }

reemplazarRegla :: String -> ConfiguracionRegla -> [ConfiguracionRegla] -> [ConfiguracionRegla]
reemplazarRegla nombre nuevaRegla reglas =
    map reemplazar reglas
  where
    reemplazar regla
        | nombreRegla regla == nombre = nuevaRegla
        | otherwise = regla

evaluarReglasAlRegistrar :: [ConfiguracionRegla] -> [RegistroFinanciero] -> RegistroFinanciero -> [Alerta]
evaluarReglasAlRegistrar reglas registrosExistentes nuevoRegistro =
    evaluarReglaGasto reglasCompletas registrosExistentes nuevoRegistro
        ++ evaluarReglaAhorro reglasCompletas nuevoRegistro
  where
    reglasCompletas = asegurarReglasPorDefecto reglas

evaluarReglaGasto :: [ConfiguracionRegla] -> [RegistroFinanciero] -> RegistroFinanciero -> [Alerta]
evaluarReglaGasto reglas registrosExistentes nuevoRegistro =
    case tipoRegistro nuevoRegistro of
        Gasto ->
            if idCategoriaRegistro nuevoRegistro == idCategoriaRegla regla
                && montoRegla regla > 0
                && gastoTotalConNuevo > montoRegla regla
                then [Alerta mensaje "Alerta"]
                else []
        _ -> []
  where
    regla = obtenerRegla nombreReglaGastoCategoria reglaGastoPorDefecto reglas
    gastoTotalConNuevo =
        totalGastosCategoria (idCategoriaRegla regla) registrosExistentes
            + montoRegistro nuevoRegistro
    mensaje =
        "Los gastos de la categoria "
            ++ show (idCategoriaRegla regla)
            ++ " superan el limite configurado de "
            ++ show (montoRegla regla)
            ++ ". Total estimado: "
            ++ show gastoTotalConNuevo
            ++ "."

evaluarReglaAhorro :: [ConfiguracionRegla] -> RegistroFinanciero -> [Alerta]
evaluarReglaAhorro reglas nuevoRegistro =
    case tipoRegistro nuevoRegistro of
        Ahorro ->
            if montoRegla regla > 0 && montoRegistro nuevoRegistro < montoRegla regla
                then [Alerta mensaje "Advertencia"]
                else []
        _ -> []
  where
    regla = obtenerRegla nombreReglaAhorroMinimo reglaAhorroPorDefecto reglas
    mensaje =
        "El ahorro registrado es menor al minimo configurado de "
            ++ show (montoRegla regla)
            ++ ". Monto registrado: "
            ++ show (montoRegistro nuevoRegistro)
            ++ "."

totalGastosCategoria :: Int -> [RegistroFinanciero] -> Double
totalGastosCategoria idCategoriaBuscada registros =
    sum (map montoRegistro gastosDeCategoria)
  where
    gastosDeCategoria =
        filter
            (\r -> tipoRegistro r == Gasto && idCategoriaRegistro r == idCategoriaBuscada)
            registros
