module UI.FinanceAnalysisMenu where

import Models
import Services.FinanceRegistryService  (cargarRegistros)
import Services.CategoryService         (buscarCategoriaPorId)
import Services.FinanceAnalysisService
import FileManager                      (cargarCategorias)
import UI.UIHelpers
    ( cerrar, enCaja, err, menuOpciones, mostrarMonto, ok, opcion, padL, padR
    , prompt, promptOpcion, separador, titulo
    )

-- ════════════════════════════════════════════════════════════
--  MENÚ PRINCIPAL DE ANÁLISIS
-- ════════════════════════════════════════════════════════════

menuAnalisisFinanciero :: IO ()
menuAnalisisFinanciero = do
    menuOpciones "Analisis Financiero"
        [ opcion 1 "Flujo de caja mensual"
        , opcion 2 "Tendencias de gasto"
        , opcion 3 "Proyeccion de gastos historicos"
        , opcion 4 "Categorias con mayor impacto financiero"
        , opcion 5 "Volver al menu principal"
        ]
    seleccion <- promptOpcion
    ejecutarOpcionAnalisis seleccion


ejecutarOpcionAnalisis :: String -> IO ()
ejecutarOpcionAnalisis seleccion =
    case seleccion of
        "1" -> subMenuFlujoCajaMensual    >> menuAnalisisFinanciero
        "2" -> subMenuTendenciasGasto     >> menuAnalisisFinanciero
        "3" -> subMenuProyeccionGastos    >> menuAnalisisFinanciero
        "4" -> subMenuCategoriasImpacto   >> menuAnalisisFinanciero
        "5" -> ok "Volviendo al menu principal..."
        _   -> err "Opcion invalida. Intente nuevamente." >> menuAnalisisFinanciero

-- ════════════════════════════════════════════════════════════
--  UTILIDAD: pedir mes y año al usuario
-- ════════════════════════════════════════════════════════════

pedirMesAnio :: IO (Int, Int)
pedirMesAnio = do
    anioStr <- prompt "Ingrese el año (ej: 2026)"
    mesStr  <- prompt "Ingrese el mes (1-12)"
    case (reads anioStr, reads mesStr) of
        ([(a, "")], [(m, "")]) | m >= 1 && m <= 12 ->
            return (m, a)
        _ -> do
            err "Valores invalidos, intente nuevamente."
            pedirMesAnio

-- ════════════════════════════════════════════════════════════
--  1. FLUJO DE CAJA MENSUAL
-- ════════════════════════════════════════════════════════════

subMenuFlujoCajaMensual :: IO ()
subMenuFlujoCajaMensual = do
    titulo "Flujo de Caja Mensual"
    cerrar
    (m, a) <- pedirMesAnio
    registros <- cargarRegistros
    let flujo = flujoCajaMensual m a registros
    putStrLn ""
    putStrLn $ "  Periodo              : " ++ show (mes flujo) ++ "/" ++ show (anio flujo)
    separador
    putStrLn $ "  (+) Ingresos         : " ++ formatMonto (totalIngresos    flujo)
    putStrLn $ "  (+) Ahorros          : " ++ formatMonto (totalAhorros     flujo)
    putStrLn $ "  (-) Gastos           : " ++ formatMonto (totalGastos      flujo)
    putStrLn $ "  (-) Inversiones      : " ++ formatMonto (totalInversiones flujo)
    separador
    let neto' = neto flujo
        signo = if neto' >= 0 then "  SUPERAVIT" else "  DEFICIT  "
    putStrLn $ signo ++ "          : " ++ formatMonto neto'
    putStrLn ""

-- ════════════════════════════════════════════════════════════
--  2. TENDENCIAS DE GASTO
-- ════════════════════════════════════════════════════════════

subMenuTendenciasGasto :: IO ()
subMenuTendenciasGasto = do
    titulo "Tendencias de Gasto"
    cerrar
    anioStr <- prompt "Ingrese el año (ej: 2026)"
    mesStr <- prompt "Ingrese el mes (1-12)"
    case (reads anioStr, reads mesStr) of
        ([(a, "")], [(m, "")]) | m >= 1 && m <= 12 -> do
            registros  <- cargarRegistros
            categorias <- cargarCategorias
            let cats = categoriasMayorFrecuencia m a registros
            putStrLn ""
            putStrLn $ "  Periodo: " ++ show m ++ "/" ++ show a
            if null cats
                then putStrLn "  No hay registros de gastos para este periodo."
                else do
                    putStrLn "  Categoria(s) con mayor frecuencia de gasto:"
                    mapM_ (imprimirCatFrecuencia categorias) cats
            putStrLn ""
        _ -> do
            err "Valores invalidos, intente nuevamente."
            subMenuTendenciasGasto

imprimirCatFrecuencia :: [Categoria] -> Int -> IO ()
imprimirCatFrecuencia categorias idCat =
    let nombre = case buscarCategoriaPorId idCat categorias of
                    Just cat -> nombreCategoria cat
                    Nothing  -> "(categoria no encontrada)"
    in  putStrLn $ "    - [ID: " ++ show idCat ++ "] " ++ nombre

-- ════════════════════════════════════════════════════════════
--  3. PROYECCION DE GASTOS
-- ════════════════════════════════════════════════════════════

subMenuProyeccionGastos :: IO ()
subMenuProyeccionGastos = do
    menuOpciones "Proyeccion de Gastos"
        [ opcion 1 "Proyeccion gastos totales"
        , opcion 2 "Proyeccion por categoria"
        , opcion 3 "Volver"
        ]
    seleccion <- promptOpcion
    case seleccion of
        "1" -> subMenuProyeccionTotal      >> subMenuProyeccionGastos
        "2" -> subMenuProyeccionCategoria  >> subMenuProyeccionGastos
        "3" -> return ()
        _   -> err "Opcion invalida. Intente nuevamente." >> subMenuProyeccionGastos

subMenuProyeccionTotal :: IO ()
subMenuProyeccionTotal = do
    titulo "Proyeccion de Gastos Totales"
    enCaja "Promedio mensual historico de todos los gastos."
    cerrar
    registros <- cargarRegistros
    putStrLn ""
    case proyeccionGastosTotales registros of
        Nothing ->
            putStrLn "  No hay gastos historicos registrados para proyectar."
        Just promedio -> do
            putStrLn $ "  Promedio mensual historico de gastos: " ++ formatMonto promedio
            putStrLn   "  (Total gastos historicos / meses distintos con gastos)"
    putStrLn ""

subMenuProyeccionCategoria :: IO ()
subMenuProyeccionCategoria = do
    titulo "Proyeccion por Categoria"
    enCaja "Promedio mensual historico por categoria."
    cerrar
    categorias <- cargarCategorias
    if null categorias
    then err "No hay categorias registradas."
    else do
        putStrLn "  Categorias disponibles:"
        mapM_ (\cat -> putStrLn $ "    [" ++ show (idCategoria cat) ++ "] " ++ nombreCategoria cat) categorias
        putStrLn ""
        idStr <- prompt "Ingrese el ID de la categoria"
        case reads idStr of
            [(idCat, "")] ->
                case buscarCategoriaPorId idCat categorias of
                    Nothing -> do
                        err "Categoria no encontrada."
                        putStrLn ""
                    Just cat -> do
                        registros <- cargarRegistros
                        putStrLn ""
                        case proyeccionGastosPorCategoria idCat registros of
                            Nothing ->
                                putStrLn $ "  No hay gastos historicos de \"" ++ nombreCategoria cat ++ "\" para proyectar."
                            Just promedio -> do
                                putStrLn $ "  Categoria        : " ++ nombreCategoria cat
                                putStrLn $ "  Promedio mensual : " ++ formatMonto promedio
                                putStrLn   "  (Total gastos de la categoria / meses distintos con gastos)"
                        putStrLn ""
            _ -> do
                err "ID invalido."
                putStrLn ""
    putStrLn ""


-- ════════════════════════════════════════════════════════════
--  4. RANKING DE CATEGORÍAS POR IMPACTO FINANCIERO
-- ════════════════════════════════════════════════════════════

subMenuCategoriasImpacto :: IO ()
subMenuCategoriasImpacto = do
    menuOpciones "Categorias con Mayor Impacto"
        [ opcion 1 "Filtro por año"
        , opcion 2 "Filtro por mes"
        , opcion 3 "Volver"
        ]
    seleccion <- promptOpcion
    case seleccion of
        "1" -> subMenuImpactoPorAnio   >> subMenuCategoriasImpacto
        "2" -> subMenuImpactoPorMes    >> subMenuCategoriasImpacto
        "3" -> return ()
        _   -> err "Opcion invalida. Intente nuevamente." >> subMenuCategoriasImpacto

subMenuImpactoPorAnio :: IO ()
subMenuImpactoPorAnio = do
    titulo "Ranking por Año"
    cerrar
    anioStr <- prompt "Ingrese el año (ej: 2026)"
    case reads anioStr of
        [(a, "")] -> do
            registros  <- cargarRegistros
            categorias <- cargarCategorias
            let ranking = rankingCategoriasPorAnio a registros
            putStrLn ""
            putStrLn $ "  Año: " ++ show a
            imprimirRanking categorias ranking
        _ -> do
            err "Año invalido. Intente nuevamente."
            subMenuImpactoPorAnio

subMenuImpactoPorMes :: IO ()
subMenuImpactoPorMes = do
    titulo "Ranking por Mes"
    cerrar
    anioStr <- prompt "Ingrese el año (ej: 2026)"
    mesStr <- prompt "Ingrese el mes (1-12)"
    case (reads anioStr, reads mesStr) of
        ([(a, "")], [(m, "")]) | m >= 1 && m <= 12 -> do
            registros  <- cargarRegistros
            categorias <- cargarCategorias
            let ranking = rankingCategoriasPorMes m a registros
            putStrLn ""
            putStrLn $ "  Periodo: " ++ show m ++ "/" ++ show a
            imprimirRanking categorias ranking
        _ -> do
            err "Valores invalidos. Intente nuevamente."
            subMenuImpactoPorMes

imprimirRanking :: [Categoria] -> [RankingCategoria] -> IO ()
imprimirRanking categorias ranking = do
    if null ranking
    then putStrLn "  No hay registros de gastos para este periodo."
    else do
        putStrLn $ "  " ++ padR 5 "Pos"
                ++ "│ " ++ padR 5 "ID"
                ++ "│ " ++ padR 24 "Nombre"
                ++ "│ " ++ padR 14 "Total"
        putStrLn $ "  " ++ replicate 54 '─'
        mapM_ (imprimirFilaRanking categorias) (zip [1 :: Int ..] ranking)
    putStrLn ""

imprimirFilaRanking :: [Categoria] -> (Int, RankingCategoria) -> IO ()
imprimirFilaRanking categorias (pos, (idCat, total)) =
    let nombre = case buscarCategoriaPorId idCat categorias of
                    Just cat -> nombreCategoria cat
                    Nothing  -> "(no encontrada)"
    in  putStrLn $ "  " ++ padLeft 3 (show pos)
                ++ "  │ " ++ padLeft 3 (show idCat)
                ++ " │ " ++ padRight 24 nombre
                ++ " │ " ++ formatMonto total

-- ════════════════════════════════════════════════════════════
--  UTILIDADES DE FORMATO
-- ════════════════════════════════════════════════════════════

-- | Formatea un Double como monto monetario con 2 decimales
formatMonto :: Double -> String
formatMonto = mostrarMonto

-- | Rellena a la izquierda con espacios hasta el ancho dado
padLeft :: Int -> String -> String
padLeft = padL

-- | Rellena a la derecha con espacios hasta el ancho dado
padRight :: Int -> String -> String
padRight = padR
