module BudgetManager where

import Models

-- Este módulo manejará los presupuestos por categoría.
-- Ahora los presupuestos se relacionan con categorías mediante ID.

-- Busca un presupuesto usando el ID de la categoría.
buscarPresupuestoPorCategoria :: Int -> [Presupuesto] -> Maybe Presupuesto
buscarPresupuestoPorCategoria _ [] = Nothing
buscarPresupuestoPorCategoria idCategoriaBuscada (p:ps)
    | idCategoriaPresupuesto p == idCategoriaBuscada = Just p
    | otherwise = buscarPresupuestoPorCategoria idCategoriaBuscada ps