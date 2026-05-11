module UI.BudgetMenu where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import FileManager (cargarCategorias, cargarPresupuestos, guardarPresupuestos)
import Models
import Services.BudgetService
    ( asegurarPresupuestosPorDefecto
    , asignarPresupuestoCategoria
    , compararRealVsPresupuesto
    )
import Services.FinanceRegistryService (cargarRegistros)
import UI.CategoryMenu (pedirIdCategoria)

-- Muestra el menú principal de presupuestos y ejecuta la opción seleccionada.
menuPresupuestos :: IO ()
menuPresupuestos = do
    putStrLn "===================================="
    putStrLn "        Gestion de Presupuestos"
    putStrLn "===================================="
    putStrLn ""
    putStrLn "Seleccione una opcion:"
    putStrLn "1. Definir presupuesto por categoria"
    putStrLn "2. Comparar gasto real vs presupuesto"
    putStrLn "3. Volver al menu principal"
    putStr "Opcion: "
    hFlush stdout
    opcion <- getLine
    ejecutarOpcionPresupuesto opcion

-- Ejecuta una opción del menú de presupuestos.
ejecutarOpcionPresupuesto :: String -> IO ()
ejecutarOpcionPresupuesto opcion =
    case opcion of
        "1" -> subMenuDefinirPresupuesto >> menuPresupuestos
        "2" -> subMenuVerComparacionRealVsPresupuesto >> menuPresupuestos
        "3" -> putStrLn "Volviendo al menu principal..."
        _   -> putStrLn "Opcion invalida. Intente de nuevo." >> menuPresupuestos

-- Permite seleccionar una categoría y asignarle un monto de presupuesto (por defecto 0).
subMenuDefinirPresupuesto :: IO ()
subMenuDefinirPresupuesto = do
    categorias <- cargarCategorias
    presupuestos <- cargarPresupuestos
    let presupuestosCompletos = asegurarPresupuestosPorDefecto categorias presupuestos

    putStrLn ""
    putStrLn "Seleccione la categoria a presupuestar:"
    idCat <- pedirIdCategoria

    putStr "Monto de presupuesto: "
    hFlush stdout
    textoMonto <- getLine

    case readMaybe textoMonto :: Maybe Double of
        Nothing -> putStrLn "Monto invalido. Debe ingresar un numero."
        Just montoNuevo -> do
            let actualizados = asignarPresupuestoCategoria idCat montoNuevo presupuestosCompletos
            guardarPresupuestos actualizados
            putStrLn "Presupuesto actualizado y guardado correctamente."

-- Recorre registros y presupuestos y muestra la comparación de gasto real vs presupuesto por categoría.
subMenuVerComparacionRealVsPresupuesto :: IO ()
subMenuVerComparacionRealVsPresupuesto = do
    categorias <- cargarCategorias
    presupuestos <- cargarPresupuestos
    registros <- cargarRegistros
    let presupuestosCompletos = asegurarPresupuestosPorDefecto categorias presupuestos
    let comparacion = compararRealVsPresupuesto categorias presupuestosCompletos registros

    putStrLn ""
    putStrLn "Categoria | Real (Gasto) | Presupuesto | Diferencia (Pres - Real)"
    putStrLn "---------------------------------------------------------------"
    mapM_ imprimirFila comparacion
  where
    -- Imprime una fila de comparación para una categoría.
    imprimirFila :: (Categoria, Double, Double, Double) -> IO ()
    imprimirFila (cat, real, presup, dif) =
        putStrLn $
            nombreCategoria cat
                ++ " | "
                ++ show real
                ++ " | "
                ++ show presup
                ++ " | "
                ++ show dif

