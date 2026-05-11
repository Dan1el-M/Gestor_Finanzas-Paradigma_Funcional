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
import UI.UIHelpers (titulo, cerrar, enCaja, ok, err, padR, mostrarMonto)

menuPresupuestos :: IO ()
menuPresupuestos = do
    titulo "Gestion de Presupuestos"
    enCaja "1. Definir presupuesto por categoria"
    enCaja "2. Comparar gasto real vs presupuesto"
    enCaja "3. Volver al menu principal"
    cerrar
    putStr "  Opcion: "
    hFlush stdout
    opcion <- getLine
    ejecutarOpcionPresupuesto opcion

ejecutarOpcionPresupuesto :: String -> IO ()
ejecutarOpcionPresupuesto opcion =
    case opcion of
        "1" -> subMenuDefinirPresupuesto >> menuPresupuestos
        "2" -> subMenuVerComparacionRealVsPresupuesto >> menuPresupuestos
        "3" -> ok "Volviendo al menu principal..."
        _   -> err "Opcion invalida." >> menuPresupuestos

subMenuDefinirPresupuesto :: IO ()
subMenuDefinirPresupuesto = do
    categorias <- cargarCategorias
    presupuestos <- cargarPresupuestos
    let presupuestosCompletos = asegurarPresupuestosPorDefecto categorias presupuestos

    titulo "Definir Presupuesto"
    enCaja "Seleccione la categoria a presupuestar"
    cerrar
    idCat <- pedirIdCategoria

    putStr "  Monto de presupuesto: "
    hFlush stdout
    textoMonto <- getLine

    case readMaybe textoMonto :: Maybe Double of
        Nothing -> err "Monto invalido. Debe ingresar un numero."
        Just montoNuevo -> do
            let actualizados = asignarPresupuestoCategoria idCat montoNuevo presupuestosCompletos
            guardarPresupuestos actualizados
            ok "Presupuesto actualizado y guardado correctamente."

subMenuVerComparacionRealVsPresupuesto :: IO ()
subMenuVerComparacionRealVsPresupuesto = do
    categorias <- cargarCategorias
    presupuestos <- cargarPresupuestos
    registros <- cargarRegistros
    let presupuestosCompletos = asegurarPresupuestosPorDefecto categorias presupuestos
    let comparacion = compararRealVsPresupuesto categorias presupuestosCompletos registros
    mostrarTablaComparacion comparacion

mostrarTablaComparacion :: [(Categoria, Double, Double, Double)] -> IO ()
mostrarTablaComparacion filas = do
    titulo "Comparacion Real vs Presupuesto"
    enCaja encabezado
    enCaja separador
    mapM_ (enCaja . fila) filas
    enCaja separador
    cerrar
  where
    encabezado =
        padR 16 "Categoria"
        ++ "| " ++ padR 10 "Real"
        ++ "| " ++ padR 10 "Presup."
        ++ "| " ++ padR 10 "Dif."

    separador = replicate 48 '-'

    fila (cat, real, presup, dif) =
        padR 16 (nombreCategoria cat)
        ++ "| " ++ padR 10 (mostrarMonto real)
        ++ "| " ++ padR 10 (mostrarMonto presup)
        ++ "| " ++ padR 10 (mostrarMonto dif)

