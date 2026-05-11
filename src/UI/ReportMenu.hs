module UI.ReportMenu where

import Text.Read (readMaybe)
import Models
import Services.ReportService
import Services.FinanceRegistryService (cargarRegistros,filtrarPorMes)
import Services.DateService (pedirAnio, pedirMes)
import FileManager (cargarCategorias)
import UI.UIHelpers
    ( cerrar, err, menuOpciones, mostrarMonto, ok, opcion, padR
    , prompt, promptOpcion, separador, titulo
    )
import UI.FinanceRegistryMenu (mostrarTablaRegistros)

menuReportes :: IO ()
menuReportes = do
    menuOpciones "Reportes Financieros"
        [ opcion 1 "Resumen mensual"
        , opcion 2 "Comparar dos periodos"
        , opcion 3 "Categorias con mayor gasto"
        , opcion 4 "Volver al menu principal"
        ]
    seleccion <- promptOpcion
    case seleccion of
        "1" -> menuResumenMensual    >> menuReportes
        "2" -> menuCompararPeriodos  >> menuReportes
        "3" -> menuTopCategorias     >> menuReportes
        "4" -> ok "Volviendo..."
        _   -> err "Opcion invalida." >> menuReportes

-- ─── Resumen mensual ───────────────────────────────────────

menuResumenMensual :: IO ()
menuResumenMensual = do
    titulo "Resumen Mensual"
    cerrar
    anio <- pedirAnio
    mes  <- pedirMes
    registros <- cargarRegistros
    let filtrados = filtrarPorMes anio mes registros  -- filtrá antes
    mostrarTablaRegistros filtrados                      
    let resumen = generarResumenMensual anio mes registros
    mostrarResumen resumen

mostrarResumen :: ResumenMensual -> IO ()
mostrarResumen r = do
    let (anio, mes) = periodoResumen r
    putStrLn ""
    putStrLn $ "  Periodo       : " ++ show anio ++ "/" ++ show mes
    separador
    putStrLn $ "  Ingresos      : " ++ mostrarMonto (totalIngresos r)
    putStrLn $ "  Gastos        : " ++ mostrarMonto (totalGastos r)
    putStrLn $ "  Ahorros       : " ++ mostrarMonto (totalAhorros r)
    putStrLn $ "  Inversiones   : " ++ mostrarMonto (totalInversiones r)
    separador
    putStrLn $ "  Balance       : " ++ mostrarMonto (balanceMensual r)
    putStrLn $ "  Registros     : " ++ show (cantidadRegistros r)

-- ─── Comparacion de periodos ───────────────────────────────

menuCompararPeriodos :: IO ()
menuCompararPeriodos = do
    titulo "Comparar Periodos"
    cerrar

    putStrLn "  -- Periodo A --"
    anioA <- pedirAnio
    mesA  <- pedirMes

    putStrLn "  -- Periodo B --"
    anioB <- pedirAnio
    mesB  <- pedirMes

    registros <- cargarRegistros

    let registrosA = filtrarPorMes anioA mesA registros
        registrosB = filtrarPorMes anioB mesB registros

        resA = generarResumenMensual anioA mesA registrosA
        resB = generarResumenMensual anioB mesB registrosB

        comp = compararPeriodos resA resB

    titulo $ "Periodo A (" ++ show anioA ++ "/" ++ show mesA ++ ")"
    cerrar
    mostrarTablaRegistros registrosA

    titulo $ "Periodo B (" ++ show anioB ++ "/" ++ show mesB ++ ")"
    cerrar
    mostrarTablaRegistros registrosB

    titulo "Resumen Comparativo"
    cerrar
    mostrarComparacion comp

mostrarComparacion :: ComparacionPeriodos -> IO ()
mostrarComparacion c = do
    putStrLn ""
    putStrLn $ "  " ++ padR 16 "Concepto"
              ++ padR 14 "Periodo A"
              ++ padR 14 "Periodo B"
              ++ padR 14 "Diferencia"
    putStrLn $ "  " ++ replicate 58 '─'
    mostrarFila "Ingresos"
        (totalIngresos  (periodoA c))
        (totalIngresos  (periodoB c))
        (diffIngresos c)
    mostrarFila "Gastos"
        (totalGastos    (periodoA c))
        (totalGastos    (periodoB c))
        (diffGastos c)
    mostrarFila "Ahorros"
        (totalAhorros   (periodoA c))
        (totalAhorros   (periodoB c))
        (diffAhorros c)
    mostrarFila "Inversiones"
        (totalInversiones (periodoA c))
        (totalInversiones (periodoB c))
        (diffInversiones c)
    mostrarFila "Balance"
        (balanceMensual (periodoA c))
        (balanceMensual (periodoB c))
        (diffBalance c)
  where
    mostrarFila label va vb diff =
        putStrLn $ "  " ++ padR 16 label
                ++ padR 14 (mostrarMonto va)
                ++ padR 14 (mostrarMonto vb)
                ++ signo diff ++ mostrarMonto (abs diff)

    signo d = if d >= 0 then "+" else "-"

-- ─── Top categorias ────────────────────────────────────────

menuTopCategorias :: IO ()
menuTopCategorias = do
    titulo "Categorias con Mayor Gasto"
    cerrar
    input <- prompt "Cuantas categorias mostrar (ej: 5)"
    case readMaybe input :: Maybe Int of
        Nothing -> err "Debe ingresar un numero."
        Just n -> do
            registros  <- cargarRegistros
            categorias <- cargarCategorias
            let top = topCategoriasGasto n registros
            mostrarTopCategorias top categorias

mostrarTopCategorias :: [GastoCategoria] -> [Categoria] -> IO ()
mostrarTopCategorias [] _ = err "No hay gastos registrados."
mostrarTopCategorias top cats = do
    putStrLn ""
    putStrLn $ "  " ++ padR 5 "Pos"
              ++ padR 6 "ID"
              ++ padR 20 "Categoria"
              ++ padR 14 "Total Gastado"
    putStrLn $ "  " ++ replicate 45 '-'
    mapM_ (mostrarFilaCategoria cats) (zip [1 :: Int ..] top)
  where
    mostrarFilaCategoria cs (pos, g) =
        let nombre = filter (/= '\r') $ case filter (\c -> idCategoria c == idCatGasto g) cs of
                        (c:_) -> nombreCategoria c
                        []    -> "ID " ++ show (idCatGasto g)
        in putStrLn $ "  " ++ padR 5 (show pos)
                    ++ padR 6 (show (idCatGasto g))
                    ++ padR 20 nombre
                    ++ padR 14 (mostrarMonto (totalGastado g))
