--Aquí va el análisis financiero 

module Analysis where

import Models

-- Este módulo realizará análisis financiero.
-- Aquí irán funciones como flujo de caja mensual,
-- tendencias de gasto y categorías con mayor impacto.

-- Calcula el total de montos de una lista de registros.
totalRegistros :: [RegistroFinanciero] -> Double
totalRegistros registros =
    sum (map montoRegistro registros)