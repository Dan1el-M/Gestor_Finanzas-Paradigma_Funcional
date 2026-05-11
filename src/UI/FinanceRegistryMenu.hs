module UI.FinanceRegistryMenu where

import Data.Char (toLower)
import Text.Read (readMaybe)

import Models
import FileManager (cargarCategorias, cargarPresupuestos, cargarReglas)
import Services.BudgetService
import Services.FinanceRegistryService
import Services.RuleService (evaluarReglasAlRegistrar)
import Services.DateService
import UI.CategoryMenu (menuCategoria, pedirIdCategoria)
import Utils (splitOn)
import UI.UIHelpers
    ( cerrar, enCaja, err, menuOpciones, mostrarMonto, ok, opcion, padR
    , prompt, promptOpcion, titulo
    )

menuRegistroFinanciero :: IO ()
menuRegistroFinanciero = do
    menuOpciones "Registros Financieros"
        [ opcion 1 "Agregar registro"
        , opcion 2 "Ver registros"
        , opcion 3 "Editar registro"
        , opcion 4 "Eliminar registro"
        , opcion 5 "Gestionar categorias"
        , opcion 6 "Volver al menu principal"
        ]
    seleccion <- promptOpcion
    ejecutarOpcion seleccion

ejecutarOpcion :: String -> IO ()
ejecutarOpcion seleccion =
    case seleccion of
        "1" -> subMenuAgregarRegistroFinanciero >> menuRegistroFinanciero
        "2" -> cargarRegistros >>= mostrarTablaRegistros >> menuRegistroFinanciero
        "3" -> subMenuEditarRegistroFinanciero >> menuRegistroFinanciero
        "4" -> subMenuEliminarRegistroFinanciero >> menuRegistroFinanciero
        "5" -> menuCategoria >> menuRegistroFinanciero
        "6" -> ok "Volviendo al menu principal..."
        _   -> err "Opcion invalida." >> menuRegistroFinanciero

-- ─── Agregar ───────────────────────────────────────────────

subMenuAgregarRegistroFinanciero :: IO ()
subMenuAgregarRegistroFinanciero = do
    titulo "Agregar Nuevo Registro Financiero"
    cerrar
    existentes <- cargarRegistros
    resultado <- solicitarDatosRegistro existentes (siguienteIdRegistro existentes)

    case resultado of
        Nothing -> err "Operacion cancelada. No se guardo el registro."
        Just nuevo -> do
            categorias <- cargarCategorias
            presupuestos <- cargarPresupuestos
            let presupuestosCompletos = asegurarPresupuestosPorDefecto categorias presupuestos

            continuar <-
                if excedePresupuestoConNuevoRegistro presupuestosCompletos existentes nuevo
                    then confirmarExcesoPresupuesto
                    else return True

            if continuar
                then do
                    let actualizados = agregarRegistro existentes nuevo
                    guardarRegistros actualizados
                    ok "Registro agregado y guardado correctamente."
                    reglas <- cargarReglas
                    mostrarAlertas (evaluarReglasAlRegistrar reglas existentes nuevo)
                else
                    err "Operacion cancelada. No se guardo el registro."

mostrarAlertas :: [Alerta] -> IO ()
mostrarAlertas [] = return ()
mostrarAlertas alertas = do
    putStrLn ""
    putStrLn "  Alertas generadas por reglas:"
    mapM_ mostrarAlerta alertas

mostrarAlerta :: Alerta -> IO ()
mostrarAlerta alerta =
    putStrLn $ "  [" ++ nivelAlerta alerta ++ "] " ++ mensajeAlerta alerta

-- Pide confirmación al usuario cuando un gasto excede el presupuesto configurado para su categoría.
confirmarExcesoPresupuesto :: IO Bool
confirmarExcesoPresupuesto = do
    err "ADVERTENCIA: este gasto excede el presupuesto de la categoria."
    resp <- prompt "Desea continuar de todos modos? (s/n)"
    let respNorm = map toLower resp
    return (respNorm == "s" || respNorm == "si" || respNorm == "sí")

solicitarDatosRegistro :: [RegistroFinanciero] -> Int -> IO (Maybe RegistroFinanciero)
solicitarDatosRegistro existentes idNuevo = do
    tipo  <- pedirTipo
    monto <- pedirMonto
    idCat <- pedirIdCategoria

    -- Warning temprano: con monto + categoria ya se puede validar (solo para Gasto).
    continuar <-
        case tipo of
            Gasto -> do
                categorias <- cargarCategorias
                presupuestos <- cargarPresupuestos
                let presupuestosCompletos = asegurarPresupuestosPorDefecto categorias presupuestos
                if excedePresupuestoConNuevoMonto presupuestosCompletos existentes idCat monto
                    then confirmarExcesoPresupuesto
                    else return True
            _ -> return True

    if not continuar
        then return Nothing
        else do
            fecha <- pedirFecha
            desc <- prompt "Descripcion"
            etiquetas <- pedirEtiquetas

            return $ Just RegistroFinanciero
                { idRegistro          = idNuevo
                , tipoRegistro        = tipo
                , montoRegistro       = monto
                , idCategoriaRegistro = idCat
                , fechaRegistro       = fecha
                , descripcionRegistro = desc
                , etiquetasRegistro   = etiquetas
                }

pedirMonto :: IO Double
pedirMonto = do
    input <- prompt "Monto"
    case readMaybe input :: Maybe Double of
        Nothing -> err "Debe ingresar un numero." >> pedirMonto
        Just m | m <= 0 -> err "El monto debe ser mayor a 0." >> pedirMonto
        Just m  -> return m

pedirTipo :: IO TipoRegistro
pedirTipo = do
    menuOpciones "Tipo de Registro"
        [ opcion 1 "Ingreso"
        , opcion 2 "Gasto"
        , opcion 3 "Ahorro"
        , opcion 4 "Inversion"
        ]
    op <- promptOpcion
    case op of
        "1" -> return Ingreso
        "2" -> return Gasto
        "3" -> return Ahorro
        "4" -> return Inversion
        _   -> err "Opcion invalida." >> pedirTipo

pedirEtiquetas :: IO [String]
pedirEtiquetas = do
    input <- prompt "Etiquetas (fijo,variable)"
    let etiquetas = filter (not . null) (splitOn ',' input)
    if null etiquetas
        then err "Debe ingresar al menos una etiqueta." >> pedirEtiquetas
        else return etiquetas

siguienteIdRegistro :: [RegistroFinanciero] -> Int
siguienteIdRegistro [] = 1
siguienteIdRegistro rs = maximum (map idRegistro rs) + 1

-- ─── Tabla de registros ────────────────────────────────────

mostrarTablaRegistros :: [RegistroFinanciero] -> IO ()
mostrarTablaRegistros [] = do
    titulo "Registros Financieros"
    enCaja "No hay registros registrados."
    cerrar
mostrarTablaRegistros rs = do
    titulo "Registros Financieros"
    cerrar
    categorias <- cargarCategorias
    putStrLn $ "  " ++ encabezado
    putStrLn $ "  " ++ separador
    mapM_ (\(n, r) -> putStrLn $ "  " ++ filaConNumero categorias n r) (zip [1 :: Int ..] (reverse rs))
    putStrLn $ "  " ++ separador
    putStrLn $ "  Total registros: " ++ show (length rs)
  where
    encabezado = padR 5 "N"
              ++ "│ " ++ padR 6  "ID"
              ++ "│ " ++ padR 11 "Tipo"
              ++ "│ " ++ padR 10 "Monto"
              ++ "│ " ++ padR 12 "Fecha"
              ++ "│ " ++ padR 15 "Descripcion"
              ++ "│ " ++ padR 20 "Categoria"
    separador  = replicate (length encabezado) '-'

filaConNumero :: [Categoria] -> Int -> RegistroFinanciero -> String
filaConNumero categorias n r =
    padR 5 (show n)
    ++ "│ " ++ padR 6  (show (idRegistro r))
    ++ "│ " ++ padR 11 (show (tipoRegistro r))
    ++ "│ " ++ padR 10 (mostrarMonto (montoRegistro r))
    ++ "│ " ++ padR 12 (show (fechaRegistro r))
    ++ "│ " ++ padR 15 (descripcionRegistro r)
    ++ "| " ++ padR 20 nombreCat
  where
    nombreCat = case filter (\c -> idCategoria c == idCategoriaRegistro r) categorias of
        (c:_) -> filter (/= '\r') (nombreCategoria c)
        []    -> "Sin categoria"

-- ─── Editar ────────────────────────────────────────────────

subMenuEditarRegistroFinanciero :: IO ()
subMenuEditarRegistroFinanciero = do
    titulo "Editar Registro Financiero"
    cerrar
    registros <- cargarRegistros
    if null registros
        then err "No hay registros para editar."
        else do
            mostrarTablaRegistros registros
            putStrLn ""
            input <- prompt "Numero de la tabla a editar"
            case readMaybe input :: Maybe Int of
                Nothing -> err "Debe ingresar un numero."
                Just n ->
                    if n < 1 || n > length registros
                        then err "Numero fuera de rango."
                        else do
                            let listaVisible = reverse registros
                                rOriginal    = listaVisible !! (n - 1)
                                indiceReal   = length registros - n
                                existentesSinOriginal = filter (\r -> idRegistro r /= idRegistro rOriginal) registros
                            mNuevo <- solicitarDatosRegistro existentesSinOriginal (idRegistro rOriginal)
                            case mNuevo of
                                Nothing -> err "Edicion cancelada. No se guardo el cambio."
                                Just nuevo -> do
                                    guardarRegistros (reemplazarEn indiceReal nuevo registros)
                                    ok "Registro editado y guardado correctamente."

-- ─── Eliminar ──────────────────────────────────────────────

subMenuEliminarRegistroFinanciero :: IO ()
subMenuEliminarRegistroFinanciero = do
    titulo "Eliminar Registro Financiero"
    cerrar
    registros <- cargarRegistros
    if null registros
        then err "No hay registros para eliminar."
        else do
            mostrarTablaRegistros registros
            putStrLn ""
            input <- prompt "Numero de la tabla a eliminar"
            case readMaybe input :: Maybe Int of
                Nothing -> err "Debe ingresar un numero."
                Just n ->
                    if n < 1 || n > length registros
                        then err "Numero fuera de rango."
                        else do
                            let indiceReal = length registros - n
                            guardarRegistros (eliminarEn indiceReal registros)
                            ok "Registro eliminado correctamente."

-- ─── Utilidades de lista ───────────────────────────────────

reemplazarEn :: Int -> a -> [a] -> [a]
reemplazarEn _ _ []         = []
reemplazarEn 0 nuevo (_:xs) = nuevo : xs
reemplazarEn n nuevo (x:xs) = x : reemplazarEn (n - 1) nuevo xs

eliminarEn :: Int -> [a] -> [a]
eliminarEn _ []     = []
eliminarEn 0 (_:xs) = xs
eliminarEn n (x:xs) = x : eliminarEn (n - 1) xs

