module UI.FinanceRegistryMenu where

import System.IO (hFlush, stdout)
import Models
import Services.FinanceRegistryService
import Services.DateService
import UI.CategoryMenu (menuCategoria, pedirIdCategoria, mostrarCategorias)
import Utils (splitOn)
import Text.Read (readMaybe)
import FileManager (cargarCategorias)
import UI.UIHelpers (titulo, cerrar, ok, err, padR)

-- ─── Menu principal ────────────────────────────────────────

menuRegistroFinanciero :: IO ()
menuRegistroFinanciero = do
    titulo "Registros Financieros"
    putStrLn "  ║  1. Agregar registro                             ║"
    putStrLn "  ║  2. Ver registros                                ║"
    putStrLn "  ║  3. Editar registro                              ║"
    putStrLn "  ║  4. Eliminar registro                            ║"
    putStrLn "  ║  5. Gestionar categorias                         ║"
    putStrLn "  ║  6. Volver al menu principal                     ║"
    cerrar
    putStr "  Opcion » "
    hFlush stdout
    opcion <- getLine
    ejecutarOpcion opcion

ejecutarOpcion :: String -> IO ()
ejecutarOpcion opcion =
    case opcion of
        "1" -> subMenuAgregarRegistroFinanciero >> menuRegistroFinanciero
        "2" -> do
            registros <- cargarRegistros
            mostrarTablaRegistros registros
            menuRegistroFinanciero
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
    nuevo <- solicitarDatosRegistro (siguienteIdRegistro existentes)
    let actualizados = agregarRegistro existentes nuevo
    guardarRegistros actualizados
    ok "Registro agregado y guardado correctamente."

solicitarDatosRegistro :: Int -> IO RegistroFinanciero
solicitarDatosRegistro idNuevo = do
    tipo  <- pedirTipo
    monto <- pedirMonto
    idCat <- pedirIdCategoria
    fecha <- pedirFecha
    putStr "  Descripcion     » "
    hFlush stdout
    desc <- getLine
    etiquetas <- pedirEtiquetas   -- ← reemplaza las dos líneas anteriores

    return $ RegistroFinanciero
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
    putStr "  Monto           » "
    hFlush stdout
    input <- getLine
    case readMaybe input :: Maybe Double of
        Nothing -> err "Debe ingresar un numero." >> pedirMonto
        Just m | m <= 0 -> err "El monto debe ser mayor a 0." >> pedirMonto
        Just m  -> return m

pedirTipo :: IO TipoRegistro
pedirTipo = do
    putStrLn ""
    putStrLn "  Tipo de registro:"
    putStrLn "    1. Ingreso    2. Gasto"
    putStrLn "    3. Ahorro     4. Inversion"
    putStr "  Opcion » "
    hFlush stdout
    op <- getLine
    case op of
        "1" -> return Ingreso
        "2" -> return Gasto
        "3" -> return Ahorro
        "4" -> return Inversion
        _   -> err "Opcion invalida." >> pedirTipo

pedirEtiquetas :: IO [String]
pedirEtiquetas = do
    putStr "  Etiquetas (fijo,variable) » "
    hFlush stdout
    input <- getLine
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
    putStrLn "  ║  No hay registros registrados.                   ║"
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
              ++ "│ " ++ padR 20  "Categoria"
    separador  = replicate (length encabezado) '-'

filaConNumero :: [Categoria] -> Int -> RegistroFinanciero -> String
filaConNumero categorias n r =
    padR 5 (show n)
    ++ "│ " ++ padR 6  (show (idRegistro r))
    ++ "│ " ++ padR 11 (show (tipoRegistro r))
    ++ "│ " ++ padR 10 (show (montoRegistro r))
    ++ "│ " ++ padR 12 (show (fechaRegistro r))
    ++ "│ " ++ padR 15 (descripcionRegistro r)
    ++ "| " ++ padR 20 (show nombreCat)    -- ← show para ver exactamente que contiene
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
            putStr "\n  Numero de la tabla a editar » "
            hFlush stdout
            input <- getLine
            case readMaybe input :: Maybe Int of
                Nothing -> err "Debe ingresar un numero."
                Just n ->
                    if n < 1 || n > length registros
                        then err "Numero fuera de rango."
                        else do
                            -- convertir numero de tabla a indice real en la lista original
                            let listaVisible = reverse registros
                                rOriginal    = listaVisible !! (n - 1)
                                indiceReal   = length registros - n
                            nuevo <- solicitarDatosRegistro (idRegistro rOriginal)
                            let actualizados = reemplazarEn indiceReal nuevo registros
                            guardarRegistros actualizados
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
            putStr "\n  Numero de la tabla a eliminar » "
            hFlush stdout
            input <- getLine
            case readMaybe input :: Maybe Int of
                Nothing -> err "Debe ingresar un numero."
                Just n ->
                    if n < 1 || n > length registros
                        then err "Numero fuera de rango."
                        else do
                            let indiceReal = length registros - n
                            let actualizados = eliminarEn indiceReal registros
                            guardarRegistros actualizados
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

