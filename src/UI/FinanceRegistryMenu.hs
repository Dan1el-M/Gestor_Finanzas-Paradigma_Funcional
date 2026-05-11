module UI.FinanceRegistryMenu where

import System.IO (hFlush, stdout)
import Models
import Services.FinanceRegistryService
import Services.DateService
import UI.CategoryMenu (menuCategoria,pedirIdCategoria)
import Utils (splitOn)

menuRegistroFinanciero :: IO ()
menuRegistroFinanciero = do
    putStrLn "===================================="
    putStrLn "  Gestion de Registros Financieros"
    putStrLn "===================================="
    putStrLn ""
    putStrLn "Seleccione una opcion:"
    putStrLn "1. Agregar nuevo registro financiero"
    putStrLn "2. Ver registros financieros"
    putStrLn "3. Editar registro financiero"
    putStrLn "4. Eliminar registro financiero"
    putStrLn "5. Gestionar categorías"
    putStrLn "6. Volver al menu principal"
    putStr "Opcion: "
    hFlush stdout

    opcion <- getLine
    ejecutarOpcion opcion
    

ejecutarOpcion :: String -> IO ()
ejecutarOpcion opcion =
    case opcion of
        "1" -> do
            subMenuAgregarRegistroFinanciero
            menuRegistroFinanciero

        "2" -> do
            registros <- cargarRegistros
            mostrarRegistros registros
            menuRegistroFinanciero

        "3" -> do
            subMenuEditarRegistroFinanciero
            menuRegistroFinanciero

        "4" -> do
            subMenuEliminarRegistroFinanciero
            menuRegistroFinanciero

        "5" -> do
            menuCategoria
            menuRegistroFinanciero

        "6" ->
            putStrLn "Volviendo al menu principal..."

        _ -> do
            putStrLn "Opcion invalida. Intente de nuevo."
            menuRegistroFinanciero

subMenuAgregarRegistroFinanciero :: IO ()
subMenuAgregarRegistroFinanciero = do
    putStrLn "===================================="
    putStrLn " Agregar Nuevo Registro Financiero"
    putStrLn "===================================="
    existentes <- cargarRegistros
    nuevo <- solicitarDatosRegistro (siguienteIdRegistro existentes)
    let actualizados = agregarRegistro existentes nuevo
    guardarRegistros actualizados
    putStrLn "Registro agregado y guardado correctamente."

solicitarDatosRegistro :: Int -> IO RegistroFinanciero
solicitarDatosRegistro idNuevo = do
    tipo <- pedirTipo

    putStr "Monto: "
    hFlush stdout
    monto <- readLn :: IO Double

    putStr "ID de categoria: "
    idCat <- pedirIdCategoria

    fecha <- pedirFecha

    putStr "Descripcion: "
    hFlush stdout
    desc <- getLine

    putStr "Etiquetas (separadas por coma, ej: fijo,variable): "
    hFlush stdout
    etiquetasRaw <- getLine
    let etiquetas = splitOn ',' etiquetasRaw

    return $ RegistroFinanciero
        { idRegistro          = idNuevo
        , tipoRegistro        = tipo
        , montoRegistro       = monto
        , idCategoriaRegistro = idCat
        , fechaRegistro       = fecha
        , descripcionRegistro = desc
        , etiquetasRegistro   = etiquetas
        }

siguienteIdRegistro :: [RegistroFinanciero] -> Int
siguienteIdRegistro [] = 1
siguienteIdRegistro registros = maximum (map idRegistro registros) + 1

pedirTipo :: IO TipoRegistro
pedirTipo = do
    putStrLn "Tipo de registro:"
    putStrLn "1. Ingreso"
    putStrLn "2. Gasto"
    putStrLn "3. Ahorro"
    putStrLn "4. Inversion"
    putStr "Opcion: "
    hFlush stdout
    op <- getLine
    case op of
        "1" -> return Ingreso
        "2" -> return Gasto
        "3" -> return Ahorro
        "4" -> return Inversion
        _   -> do
            putStrLn "Opcion invalida, seleccione de nuevo."
            pedirTipo

mostrarRegistros :: [RegistroFinanciero] -> IO ()
mostrarRegistros [] = putStrLn "No hay registros."
mostrarRegistros rs = mapM_ mostrarRegistro rs

mostrarRegistro :: RegistroFinanciero -> IO ()
mostrarRegistro r = do
    putStrLn "----------------------------"
    putStrLn $ "ID:          " ++ show (idRegistro r)
    putStrLn $ "Tipo:        " ++ show (tipoRegistro r)
    putStrLn $ "Monto:       " ++ show (montoRegistro r)
    putStrLn $ "ID Categoria:" ++ show (idCategoriaRegistro r)
    putStrLn $ "Fecha:       " ++ show (fechaRegistro r)
    putStrLn $ "Descripcion: " ++ descripcionRegistro r
    putStrLn $ "Etiquetas:   " ++ unwords (etiquetasRegistro r)


subMenuEditarRegistroFinanciero :: IO ()
subMenuEditarRegistroFinanciero = do
    putStrLn "===================================="
    putStrLn " Editar Registro Financiero"
    putStrLn "===================================="
    registros <- cargarRegistros
    if null registros
        then putStrLn "No hay registros para editar."
        else do
            mostrarRegistrosNumerados registros
            putStr "Numero de registro a editar: "
            hFlush stdout
            input <- getLine
            let indice = read input :: Int
            if indice < 1 || indice > length registros
                then putStrLn "Numero invalido."
                else do
                    putStrLn "Ingrese los nuevos datos:"
                    nuevo <- solicitarDatosRegistro (idRegistro (registros !! (indice - 1)))
                    let actualizados = reemplazarEn (indice - 1) nuevo registros
                    guardarRegistros actualizados
                    putStrLn "Registro editado y guardado correctamente."

subMenuEliminarRegistroFinanciero :: IO ()
subMenuEliminarRegistroFinanciero = do
    putStrLn "===================================="
    putStrLn " Eliminar Registro Financiero"
    putStrLn "===================================="
    registros <- cargarRegistros
    if null registros
        then putStrLn "No hay registros para eliminar."
        else do
            mostrarRegistrosNumerados registros
            putStr "Numero de registro a eliminar: "
            hFlush stdout
            input <- getLine
            let indice = read input :: Int
            if indice < 1 || indice > length registros
                then putStrLn "Numero invalido."
                else do
                    let actualizados = eliminarEn (indice - 1) registros
                    guardarRegistros actualizados
                    putStrLn "Registro eliminado correctamente."


-- Muestra los registros con número de posición
mostrarRegistrosNumerados :: [RegistroFinanciero] -> IO ()
mostrarRegistrosNumerados registros =
    mapM_ mostrarConNumero (zip [1 :: Int ..] registros)
  where
    mostrarConNumero (n, r) = do
        putStrLn $ "\n[" ++ show n ++ "]"
        putStrLn $ "  ID:        " ++ show (idRegistro r)
        putStrLn $ "  Tipo:      " ++ show (tipoRegistro r)
        putStrLn $ "  Monto:     " ++ show (montoRegistro r)
        putStrLn $ "  ID Categoria: " ++ show (idCategoriaRegistro r)
        putStrLn $ "  Fecha:     " ++ show (fechaRegistro r)
        putStrLn $ "  Descripcion: " ++ descripcionRegistro r

-- Reemplaza el elemento en la posición dada
reemplazarEn :: Int -> a -> [a] -> [a]
reemplazarEn _ _ []     = []
reemplazarEn 0 nuevo (_:xs) = nuevo : xs
reemplazarEn n nuevo (x:xs) = x : reemplazarEn (n - 1) nuevo xs

-- Elimina el elemento en la posición dada
eliminarEn :: Int -> [a] -> [a]
eliminarEn _ []     = []
eliminarEn 0 (_:xs) = xs
eliminarEn n (x:xs) = x : eliminarEn (n - 1) xs
