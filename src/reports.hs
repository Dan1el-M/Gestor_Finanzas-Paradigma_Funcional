--Aquí se generan los reportes.

module Reports where

import Models

-- Este módulo generará reportes financieros.
-- Los reportes podrán mostrarse en consola y luego guardarse en .txt.

-- version beta, solo muestra la cantidad de registros financieros.
generarResumenBasico :: [RegistroFinanciero] -> String
generarResumenBasico registros =
    "Cantidad de registros financieros: " ++ show (length registros)