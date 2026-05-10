--Aquí va la lógica para manejar los registros financieros.

{- ejemplo
Agregar ingresos
Agregar gastos
Filtrar por categoría
Filtrar por tipo
Calcular totales
-}

module FinanceManager where

import Models

-- Este módulo gestionará los registros financieros.
-- Aquí irán funciones para agregar, filtrar y calcular totales
-- de ingresos, gastos, ahorros e inversiones.

-- es para qye haya algo, version beta
cantidadRegistros :: [RegistroFinanciero] -> Int
cantidadRegistros registros = length registros 