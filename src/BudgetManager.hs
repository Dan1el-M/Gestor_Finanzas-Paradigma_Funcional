--Aquí va la lógica de presupuestos.

{-
Crear presupuesto por categoría
Comparar gasto real contra presupuesto
Saber si una categoría se pasó del límite
-}

module BudgetManager where

import Models

-- Este módulo manejará los presupuestos por categoría.
-- Aquí se compararán los gastos reales contra los límites definidos.

-- Busca un presupuesto por categoría.
-- version beta
buscarPresupuesto :: String -> [Presupuesto] -> Maybe Presupuesto
buscarPresupuesto _ [] = Nothing
buscarPresupuesto categoria (p:ps)
    | categoriaPresupuesto p == categoria = Just p
    | otherwise = buscarPresupuesto categoria ps