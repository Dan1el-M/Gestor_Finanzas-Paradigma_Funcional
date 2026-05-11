module UI.FinanceAnalysisMenu where

import System.IO (hFlush, stdout)
import Data.List (intercalate)

import Models
import Services.FinanceRegistryService  (cargarRegistros)
import Services.CategoryService         (buscarCategoriaPorId)
import Services.FinanceAnalysisService
import FileManager                      (cargarCategorias)

-- ════════════════════════════════════════════════════════════
--  MENÚ PRINCIPAL DE ANÁLISIS
-- ════════════════════════════════════════════════════════════

menuAnalisisFinanciero :: IO ()
menuAnalisisFinanciero = do
    putStrLn "===================================="
    putStrLn "      Analisis Financiero"
    putStrLn "===================================="
    putStrLn ""
    putStrLn "Seleccione una opcion:"
    putStrLn "1. Flujo de caja mensual"
    putStrLn "2. Tendencias de gasto"
    putStrLn "3. Proyeccion de gastos basada en datos historicos"
    putStrLn "4. Identificacion de categorias con mayor impacto financiero"
    putStrLn "5. Volver al menu principal"
    putStr "Opcion: "
    hFlush stdout

    opcion <- getLine
    ejecutarOpcionAnalisis opcion


ejecutarOpcionAnalisis :: String -> IO ()
ejecutarOpcionAnalisis opcion =
    case opcion of
        "1" -> subMenuFlujoCajaMensual    >> menuAnalisisFinanciero
        "2" -> subMenuTendenciasGasto     >> menuAnalisisFinanciero
        "3" -> subMenuProyeccionGastos    >> menuAnalisisFinanciero
        "4" -> subMenuCategoriasImpacto   >> menuAnalisisFinanciero
        "5" -> putStrLn "Volviendo al menu principal..."
        _   -> do
                   putStrLn "Opcion invalida. Intente nuevamente."
                   menuAnalisisFinanciero

-- ════════════════════════════════════════════════════════════
--  UTILIDAD: pedir mes y año al usuario
-- ════════════════════════════════════════════════════════════

pedirMesAnio :: IO (Int, Int)
pedirMesAnio = do
    putStr "Ingrese el año (ej: 2026): "
    hFlush stdout
    anioStr <- getLine
    putStr "Ingrese el mes (1-12): "
    hFlush stdout
    mesStr  <- getLine
    case (reads anioStr, reads mesStr) of
        ([(a, "")], [(m, "")]) | m >= 1 && m <= 12 ->
            return (m, a)
        _ -> do
            putStrLn "Valores invalidos, intente nuevamente."
            pedirMesAnio

-- ════════════════════════════════════════════════════════════
--  1. FLUJO DE CAJA MENSUAL
-- ════════════════════════════════════════════════════════════

subMenuFlujoCajaMensual :: IO ()
subMenuFlujoCajaMensual = do
    putStrLn "===================================="
    putStrLn "      Flujo de Caja Mensual"
    putStrLn "===================================="
    (m, a) <- pedirMesAnio
    registros <- cargarRegistros
    let flujo = flujoCajaMensual m a registros
    putStrLn ""
    putStrLn $ "  Periodo              : " ++ show (mes flujo) ++ "/" ++ show (anio flujo)
    putStrLn   "  ------------------------------------"
    putStrLn $ "  (+) Ingresos         : " ++ formatMonto (totalIngresos    flujo)
    putStrLn $ "  (+) Ahorros          : " ++ formatMonto (totalAhorros     flujo)
    putStrLn $ "  (-) Gastos           : " ++ formatMonto (totalGastos      flujo)
    putStrLn $ "  (-) Inversiones      : " ++ formatMonto (totalInversiones flujo)
    putStrLn   "  ------------------------------------"
    let neto' = neto flujo
        signo = if neto' >= 0 then "  SUPERAVIT" else "  DEFICIT  "
    putStrLn $ signo ++ "          : " ++ formatMonto neto'
    putStrLn ""

-- ════════════════════════════════════════════════════════════
--  2. TENDENCIAS DE GASTO
-- ════════════════════════════════════════════════════════════

subMenuTendenciasGasto :: IO ()
subMenuTendenciasGasto = do
    putStrLn "===================================="
    putStrLn "       Tendencias de Gasto"
    putStrLn "===================================="
    putStr "Ingrese el año (ej: 2026): "
    hFlush stdout
    anioStr <- getLine
    putStr "Ingrese el mes (1-12): "
    hFlush stdout
    mesStr <- getLine
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
            putStrLn "Valores invalidos, intente nuevamente."
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
    putStrLn "===================================="
    putStrLn "    Proyeccion de Gastos"
    putStrLn "===================================="
    putStrLn ""
    putStrLn "Seleccione una opcion:"
    putStrLn "1. Proyeccion gastos totales"
    putStrLn "2. Proyeccion por categoria"
    putStrLn "3. Volver"
    putStr "Opcion: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> subMenuProyeccionTotal      >> subMenuProyeccionGastos
        "2" -> subMenuProyeccionCategoria  >> subMenuProyeccionGastos
        "3" -> return ()
        _   -> do
                   putStrLn "Opcion invalida. Intente nuevamente."
                   subMenuProyeccionGastos

subMenuProyeccionTotal :: IO ()
subMenuProyeccionTotal = do
    putStrLn "===================================="
    putStrLn "   Proyeccion de Gastos Totales"
    putStrLn "===================================="
    putStrLn "  Calcula el promedio mensual historico de todos los gastos."
    putStrLn ""
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
    putStrLn "===================================="
    putStrLn "   Proyeccion por Categoria"
    putStrLn "===================================="
    putStrLn "  Calcula el promedio mensual historico de gastos por categoria."
    putStrLn ""
    categorias <- cargarCategorias
    if null categorias
    then putStrLn "  No hay categorias registradas."
    else do
        putStrLn "  Categorias disponibles:"
        mapM_ (\cat -> putStrLn $ "    [" ++ show (idCategoria cat) ++ "] " ++ nombreCategoria cat) categorias
        putStrLn ""
        putStr "Ingrese el ID de la categoria: "
        hFlush stdout
        idStr <- getLine
        case reads idStr of
            [(idCat, "")] ->
                case buscarCategoriaPorId idCat categorias of
                    Nothing -> do
                        putStrLn "  Categoria no encontrada."
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
                putStrLn "  ID invalido."
                putStrLn ""
    putStrLn ""


-- ════════════════════════════════════════════════════════════
--  4. RANKING DE CATEGORÍAS POR IMPACTO FINANCIERO
-- ════════════════════════════════════════════════════════════

subMenuCategoriasImpacto :: IO ()
subMenuCategoriasImpacto = do
    putStrLn "===================================="
    putStrLn " Categorias con Mayor Impacto"
    putStrLn "===================================="
    putStrLn ""
    putStrLn "Seleccione una opcion:"
    putStrLn "1. Filtro por año"
    putStrLn "2. Filtro por mes"
    putStrLn "3. Volver"
    putStr "Opcion: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> subMenuImpactoPorAnio   >> subMenuCategoriasImpacto
        "2" -> subMenuImpactoPorMes    >> subMenuCategoriasImpacto
        "3" -> return ()
        _   -> do
                   putStrLn "Opcion invalida. Intente nuevamente."
                   subMenuCategoriasImpacto

subMenuImpactoPorAnio :: IO ()
subMenuImpactoPorAnio = do
    putStrLn "===================================="
    putStrLn "  Ranking por año"
    putStrLn "===================================="
    putStr "Ingrese el año (ej: 2026): "
    hFlush stdout
    anioStr <- getLine
    case reads anioStr of
        [(a, "")] -> do
            registros  <- cargarRegistros
            categorias <- cargarCategorias
            let ranking = rankingCategoriasPorAnio a registros
            putStrLn ""
            putStrLn $ "  Año: " ++ show a
            imprimirRanking categorias ranking
        _ -> do
            putStrLn "Año invalido. Intente nuevamente."
            subMenuImpactoPorAnio

subMenuImpactoPorMes :: IO ()
subMenuImpactoPorMes = do
    putStrLn "===================================="
    putStrLn "  Ranking por Mes"
    putStrLn "===================================="
    putStr "Ingrese el año (ej: 2026): "
    hFlush stdout
    anioStr <- getLine
    putStr "Ingrese el mes (1-12): "
    hFlush stdout
    mesStr <- getLine
    case (reads anioStr, reads mesStr) of
        ([(a, "")], [(m, "")]) | m >= 1 && m <= 12 -> do
            registros  <- cargarRegistros
            categorias <- cargarCategorias
            let ranking = rankingCategoriasPorMes m a registros
            putStrLn ""
            putStrLn $ "  Periodo: " ++ show m ++ "/" ++ show a
            imprimirRanking categorias ranking
        _ -> do
            putStrLn "Valores invalidos. Intente nuevamente."
            subMenuImpactoPorMes

imprimirRanking :: [Categoria] -> [RankingCategoria] -> IO ()
imprimirRanking categorias ranking = do
    if null ranking
    then putStrLn "  No hay registros de gastos para este periodo."
    else do
        putStrLn "  Pos  | ID  | Nombre                 | Total Gastado"
        putStrLn "  -----|-----|------------------------|---------------"
        mapM_ (imprimirFilaRanking categorias) (zip [1 :: Int ..] ranking)
    putStrLn ""

imprimirFilaRanking :: [Categoria] -> (Int, RankingCategoria) -> IO ()
imprimirFilaRanking categorias (pos, (idCat, total)) =
    let nombre = case buscarCategoriaPorId idCat categorias of
                    Just cat -> nombreCategoria cat
                    Nothing  -> "(no encontrada)"
    in  putStrLn $ "  " ++ padLeft 3 (show pos)
                ++ "  | " ++ padLeft 3 (show idCat)
                ++ " | " ++ padRight 22 nombre
                ++ " | " ++ formatMonto total

-- ════════════════════════════════════════════════════════════
--  UTILIDADES DE FORMATO
-- ════════════════════════════════════════════════════════════

-- | Formatea un Double como monto monetario con 2 decimales
formatMonto :: Double -> String
formatMonto x =
    let (entero, dec) = break (== '.') (show (fromIntegral (round (x * 100) :: Int) / 100.0 :: Double))
        decimales     = take 3 (dec ++ ".00")   -- asegura ".XX"
    in  entero ++ decimales

-- | Rellena a la izquierda con espacios hasta el ancho dado
padLeft :: Int -> String -> String
padLeft n s = replicate (max 0 (n - length s)) ' ' ++ s

-- | Rellena a la derecha con espacios hasta el ancho dado
padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '