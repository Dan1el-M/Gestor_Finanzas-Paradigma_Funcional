module UI.BudgetMenu where

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
import UI.UIHelpers
    ( cerrar, enCaja, err, menuOpciones, mostrarMonto, ok, opcion, padR
    , prompt, promptOpcion, titulo
    )

menuPresupuestos :: IO ()
menuPresupuestos = do
    menuOpciones "Gestion de Presupuestos"
        [ opcion 1 "Definir presupuesto por categoria"
        , opcion 2 "Comparar gasto real vs presupuesto"
        , opcion 3 "Volver al menu principal"
        ]
    seleccion <- promptOpcion
    ejecutarOpcionPresupuesto seleccion

ejecutarOpcionPresupuesto :: String -> IO ()
ejecutarOpcionPresupuesto seleccion =
    case seleccion of
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

    textoMonto <- prompt "Monto de presupuesto"

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
    cerrar
    putStrLn $ "  " ++ encabezado
    putStrLn $ "  " ++ separador
    mapM_ (putStrLn . ("  " ++) . fila) filas
    putStrLn $ "  " ++ separador
  where
    encabezado =
        padR 24 "Categoria"
        ++ "│ " ++ padR 14 "Real"
        ++ "│ " ++ padR 14 "Presup."
        ++ "│ " ++ padR 14 "Dif."

    separador = replicate (length encabezado) '─'

    fila (cat, real, presup, dif) =
        padR 24 (limpiarNombre (nombreCategoria cat))
        ++ "│ " ++ padR 14 (mostrarMonto real)
        ++ "│ " ++ padR 14 (mostrarMonto presup)
        ++ "│ " ++ padR 14 (mostrarMonto dif)

    limpiarNombre = filter (/= '\r')

