module Services.FinanceAnalysisService where

import Models
import Data.List        (sortBy, groupBy, nub)
import Data.Ord         (comparing, Down(..))
import Data.Time        (toGregorian)

-- ════════════════════════════════════════════════════════════
--  TIPOS AUXILIARES
-- ════════════════════════════════════════════════════════════

-- | Resultado del flujo de caja de un mes dado
data FlujoCaja = FlujoCaja
    { mes         :: Int     -- 1-12
    , anio        :: Int
    , totalIngresos  :: Double
    , totalAhorros   :: Double
    , totalGastos    :: Double
    , totalInversiones :: Double
    , neto        :: Double  -- (ingresos+ahorros) - (gastos+inversiones)
    } deriving (Show)

-- | Par (idCategoria, monto total acumulado)
type RankingCategoria = (Int, Double)

-- ════════════════════════════════════════════════════════════
--  UTILIDADES INTERNAS
-- ════════════════════════════════════════════════════════════

-- | Extrae (año, mes) del Day almacenado en el registro.
--   toGregorian :: Day -> (Integer, Int, Int)  ->  (año, mes, día)
parMesAnio :: RegistroFinanciero -> (Int, Int)
parMesAnio r =
    let (a, m, _) = toGregorian (fechaRegistro r)
    in  (fromIntegral a, m)

-- | Suma los montos de una lista de registros
sumarMontos :: [RegistroFinanciero] -> Double
sumarMontos = foldr (\r acc -> montoRegistro r + acc) 0.0

-- ════════════════════════════════════════════════════════════
--  1. FLUJO DE CAJA MENSUAL
-- ════════════════════════════════════════════════════════════

-- | Calcula el flujo de caja para un mes/año concretos.
--   Recibe TODOS los registros y devuelve el resumen del periodo.
flujoCajaMensual :: Int -> Int -> [RegistroFinanciero] -> FlujoCaja
flujoCajaMensual mesObj anioObj todos =
    let delMes     = filter (\r -> parMesAnio r == (anioObj, mesObj)) todos
        ing        = sumarMontos $ filter (\r -> tipoRegistro r == Ingreso)    delMes
        aho        = sumarMontos $ filter (\r -> tipoRegistro r == Ahorro)     delMes
        gas        = sumarMontos $ filter (\r -> tipoRegistro r == Gasto)      delMes
        inv        = sumarMontos $ filter (\r -> tipoRegistro r == Inversion)  delMes
    in  FlujoCaja
            { mes              = mesObj
            , anio             = anioObj
            , totalIngresos    = ing
            , totalAhorros     = aho
            , totalGastos      = gas
            , totalInversiones = inv
            , neto             = (ing + aho) - (gas + inv)
            }

-- ════════════════════════════════════════════════════════════
--  2. TENDENCIAS DE GASTO  (categorías con más frecuencia)
-- ════════════════════════════════════════════════════════════

-- | Cuenta cuántas veces aparece cada categoría en los gastos de un mes/año.
--   Retorna la(s) categoría(s) con la MAYOR frecuencia.
--   En caso de empate, devuelve todas las empatadas.
categoriasMayorFrecuencia :: Int -> Int -> [RegistroFinanciero] -> [Int]
categoriasMayorFrecuencia mesObj anioObj registros =
    let delMes      = filter (\r -> parMesAnio r == (anioObj, mesObj)) registros
        gastos'     = filter (\r -> tipoRegistro r == Gasto) delMes
        categorias  = map idCategoriaRegistro gastos'
        -- agrupar y contar
        frecuencias = map (\g -> (head g, length g))
                    . groupBy (==)
                    . sort
                    $ categorias
    in  if null frecuencias
        then []
        else let maxFreq = maximum (map snd frecuencias)
             in  [idCat | (idCat, freq) <- frecuencias, freq == maxFreq]
  where
    sort = sortBy compare   -- ordena para poder agrupar con groupBy

-- ════════════════════════════════════════════════════════════
--  3. PROYECCIÓN DE GASTOS (promedio histórico)
-- ════════════════════════════════════════════════════════════

-- | Devuelve el mes y año anteriores al dado.
mesAnterior :: Int -> Int -> (Int, Int)
mesAnterior 1  a = (12, a - 1)
mesAnterior m  a = (m - 1, a)

-- | Promedio mensual simple de gastos históricos totales.
--   Total de todos los gastos / cantidad de meses distintos con gastos.
--   Retorna Nothing si no hay ningún gasto registrado.
proyeccionGastosTotales :: [RegistroFinanciero] -> Maybe Double
proyeccionGastosTotales todos =
    let gastos'    = filter (\r -> tipoRegistro r == Gasto) todos
        totalGasto = sumarMontos gastos'
        mesesConGastos = length . nub $ map parMesAnio gastos'
    in  if null gastos'
        then Nothing
        else Just (totalGasto / fromIntegral mesesConGastos)

-- | Promedio mensual simple de gastos históricos para una categoría concreta.
--   Total de gastos de la categoría / meses distintos con gastos en esa categoría.
--   Retorna Nothing si no hay gastos de esa categoría.
proyeccionGastosPorCategoria :: Int -> [RegistroFinanciero] -> Maybe Double
proyeccionGastosPorCategoria idCat todos =
    let gastos'    = filter (\r -> tipoRegistro r == Gasto
                                && idCategoriaRegistro r == idCat) todos
        totalGasto = sumarMontos gastos'
        mesesConGastos = length . nub $ map parMesAnio gastos'
    in  if null gastos'
        then Nothing
        else Just (totalGasto / fromIntegral mesesConGastos)

-- ════════════════════════════════════════════════════════════
--  4. RANKING DE CATEGORÍAS POR IMPACTO FINANCIERO
-- ════════════════════════════════════════════════════════════

-- | Núcleo del ranking: recibe gastos ya filtrados y devuelve ranking DESC.
rankingDesdeGastos :: [RegistroFinanciero] -> [RankingCategoria]
rankingDesdeGastos gastos' =
    let idsCats    = nub (map idCategoriaRegistro gastos')
        totalesCat = map (\idCat ->
                            let subLista = filter (\r -> idCategoriaRegistro r == idCat) gastos'
                            in  (idCat, sumarMontos subLista)
                         ) idsCats
    in  sortBy (comparing (Down . snd)) totalesCat

-- | Ranking de mayor a menor gasto por categoría en un año completo.
rankingCategoriasPorAnio :: Int -> [RegistroFinanciero] -> [RankingCategoria]
rankingCategoriasPorAnio anioObj registros =
    let gastos' = filter (\r -> tipoRegistro r == Gasto
                              && fst (parMesAnio r) == anioObj) registros
    in  rankingDesdeGastos gastos'

-- | Ranking de mayor a menor gasto por categoría en un mes/año concreto.
rankingCategoriasPorMes :: Int -> Int -> [RegistroFinanciero] -> [RankingCategoria]
rankingCategoriasPorMes mesObj anioObj registros =
    let gastos' = filter (\r -> tipoRegistro r == Gasto
                              && parMesAnio r == (anioObj, mesObj)) registros
    in  rankingDesdeGastos gastos'