module Services.ReportService where

import Models
import Data.List (sortBy, groupBy, maximumBy)
import Data.Ord (comparing, Down(..))
import Data.Time (Day, toGregorian)

-- ─── Utilidades de fecha ───────────────────────────────────

anioMes :: Day -> (Integer, Int)
anioMes fecha =
    let (anio, mes, _) = toGregorian fecha
    in (anio, mes)

mismoMes :: RegistroFinanciero -> RegistroFinanciero -> Bool
mismoMes a b = anioMes (fechaRegistro a) == anioMes (fechaRegistro b)

-- ─── Filtros ───────────────────────────────────────────────

registrosDelMes :: Integer -> Int -> [RegistroFinanciero] -> [RegistroFinanciero]
registrosDelMes anio mes = filter (\r -> anioMes (fechaRegistro r) == (anio, mes))

registrosPorTipo :: TipoRegistro -> [RegistroFinanciero] -> [RegistroFinanciero]
registrosPorTipo tipo = filter (\r -> tipoRegistro r == tipo)

-- ─── Totales ───────────────────────────────────────────────

totalMonto :: [RegistroFinanciero] -> Double
totalMonto = foldr (\r acc -> montoRegistro r + acc) 0.0

-- ─── Resumen mensual ───────────────────────────────────────

data ResumenMensual = ResumenMensual
    { periodoResumen    :: (Integer, Int)
    , totalIngresos     :: Double
    , totalGastos       :: Double
    , totalAhorros      :: Double
    , totalInversiones  :: Double
    , balanceMensual    :: Double
    , cantidadRegistros :: Int
    } deriving (Show)

generarResumenMensual :: Integer -> Int -> [RegistroFinanciero] -> ResumenMensual
generarResumenMensual anio mes registros =
    let del     = registrosDelMes anio mes registros
        ingresos   = totalMonto (registrosPorTipo Ingreso del)
        gastos     = totalMonto (registrosPorTipo Gasto del)
        ahorros    = totalMonto (registrosPorTipo Ahorro del)
        inversiones = totalMonto (registrosPorTipo Inversion del)
        balance    = ingresos - gastos + ahorros - inversiones
    in ResumenMensual
        { periodoResumen    = (anio, mes)
        , totalIngresos     = ingresos
        , totalGastos       = gastos
        , totalAhorros      = ahorros
        , totalInversiones  = inversiones
        , balanceMensual    = balance
        , cantidadRegistros = length del
        }

-- ─── Comparacion entre periodos ────────────────────────────

data ComparacionPeriodos = ComparacionPeriodos
    { periodoA         :: ResumenMensual
    , periodoB         :: ResumenMensual
    , diffIngresos     :: Double
    , diffGastos       :: Double
    , diffAhorros      :: Double
    , diffInversiones  :: Double
    , diffBalance      :: Double
    } deriving (Show)

compararPeriodos :: ResumenMensual -> ResumenMensual -> ComparacionPeriodos
compararPeriodos a b =
    ComparacionPeriodos
        a
        b
        (totalIngresos b - totalIngresos a)
        (totalGastos b - totalGastos a)
        (totalAhorros b - totalAhorros a)
        (totalInversiones b - totalInversiones a)
        (balanceMensual b - balanceMensual a)

-- ─── Categorias con mayor gasto ────────────────────────────

data GastoCategoria = GastoCategoria
    { idCatGasto    :: Int
    , totalGastado  :: Double
    } deriving (Show)

gastosPorCategoria :: [RegistroFinanciero] -> [GastoCategoria]
gastosPorCategoria registros =
    let soloGastos  = registrosPorTipo Gasto registros
        agrupados   = groupBy (\a b -> idCategoriaRegistro a == idCategoriaRegistro b)
                    $ sortBy (comparing idCategoriaRegistro) soloGastos
        resumir g   = GastoCategoria
                        { idCatGasto   = idCategoriaRegistro (head g)
                        , totalGastado = totalMonto g
                        }
    in sortBy (comparing (Down . totalGastado)) (map resumir agrupados)

-- Top N categorias con mayor gasto
topCategoriasGasto :: Int -> [RegistroFinanciero] -> [GastoCategoria]
topCategoriasGasto n = take n . gastosPorCategoria