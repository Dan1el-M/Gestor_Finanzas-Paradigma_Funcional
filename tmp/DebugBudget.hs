module Main where

import FileManager (cargarCategorias, cargarPresupuestos)
import Services.BudgetService (asegurarPresupuestosPorDefecto, excedePresupuestoConNuevoMonto, buscarPresupuestoPorCategoria)
import Services.FinanceRegistryService (cargarRegistros)

main :: IO ()
main = do
  categorias <- cargarCategorias
  presupuestos <- cargarPresupuestos
  registros <- cargarRegistros
  let presupuestosCompletos = asegurarPresupuestosPorDefecto categorias presupuestos
  putStrLn ("presupuestos=" ++ show presupuestosCompletos)
  let idCat=1; monto=2000
  putStrLn ("lookup presupuesto cat1=" ++ show (buscarPresupuestoPorCategoria idCat presupuestosCompletos))
  putStrLn ("excede cat1 2000=" ++ show (excedePresupuestoConNuevoMonto presupuestosCompletos registros idCat monto))
