--Aquí van las simulaciones.

module Simulations where

-- Este módulo permitirá simular escenarios financieros.
-- Ejemplo: reducir gastos en cierto porcentaje
-- o proyectar ahorro en el tiempo.

-- Calcula un nuevo monto después de aplicar una reducción porcentual.
-- Ejemplo:
-- simularReduccion 100000 10 = 90000
simularReduccion :: Double -> Double -> Double
simularReduccion monto porcentaje =
    monto - (monto * porcentaje / 100)