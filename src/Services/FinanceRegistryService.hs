module Services.FinanceRegistryService where

import Data.Char (isSpace)
import Data.List (isPrefixOf, stripPrefix)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Text.Read (readMaybe)

import Models
import FileManager  -- aquí ya están rutaRegistros, leerLineasArchivo, guardarLineasArchivo

-- ─── Lógica pura ───────────────────────────────────────────

agregarRegistro :: [RegistroFinanciero] -> RegistroFinanciero -> [RegistroFinanciero]
agregarRegistro registros nuevo = nuevo : registros

filtrarPorTipo :: TipoRegistro -> [RegistroFinanciero] -> [RegistroFinanciero]
filtrarPorTipo tipo = filter (\r -> tipoRegistro r == tipo)

filtrarPorCategoria :: Int -> [RegistroFinanciero] -> [RegistroFinanciero]
filtrarPorCategoria idCategoriaBuscada =
    filter (\r -> idCategoriaRegistro r == idCategoriaBuscada)

filtrarPorEtiqueta :: String -> [RegistroFinanciero] -> [RegistroFinanciero]
filtrarPorEtiqueta etiqueta = filter (\r -> etiqueta `elem` etiquetasRegistro r)

totalRegistros :: [RegistroFinanciero] -> Double
totalRegistros = foldr (\r acc -> montoRegistro r + acc) 0.0

-- ─── Serialización ─────────────────────────────────────────

registroALinea :: RegistroFinanciero -> String
registroALinea = show

lineaARegistro :: String -> Maybe RegistroFinanciero
lineaARegistro linea =
    let lineaLimpia = limpiarEspacios linea
    in case reads lineaLimpia of
        [(r, "")] -> Just r
        _         -> lineaMostradaARegistro lineaLimpia

lineaMostradaARegistro :: String -> Maybe RegistroFinanciero
lineaMostradaARegistro linea = do
    contenido <- quitarPrefijoYSufijo "RegistroFinanciero {" "}" linea
    textoId <- extraerCampo "idRegistro = " ", tipoRegistro = " contenido
    textoTipo <- extraerCampo "tipoRegistro = " ", montoRegistro = " contenido
    textoMonto <- extraerCampo "montoRegistro = " ", idCategoriaRegistro = " contenido
    textoCategoria <- extraerCampo "idCategoriaRegistro = " ", fechaRegistro = " contenido
    textoFecha <- extraerCampo "fechaRegistro = " ", descripcionRegistro = " contenido
    textoDescripcion <- extraerCampo "descripcionRegistro = " ", etiquetasRegistro = " contenido
    textoEtiquetas <- tomarDespues "etiquetasRegistro = " contenido

    idReg <- readMaybe textoId
    tipo <- readMaybe textoTipo
    monto <- readMaybe textoMonto
    idCat <- readMaybe textoCategoria
    fecha <- parsearDia textoFecha
    descripcion <- readMaybe textoDescripcion
    etiquetas <- readMaybe textoEtiquetas

    return RegistroFinanciero
        { idRegistro = idReg
        , tipoRegistro = tipo
        , montoRegistro = monto
        , idCategoriaRegistro = idCat
        , fechaRegistro = fecha
        , descripcionRegistro = descripcion
        , etiquetasRegistro = etiquetas
        }

quitarPrefijoYSufijo :: String -> String -> String -> Maybe String
quitarPrefijoYSufijo prefijo sufijo texto = do
    sinPrefijo <- stripPrefix prefijo texto
    quitarSufijo sufijo sinPrefijo

quitarSufijo :: String -> String -> Maybe String
quitarSufijo sufijo texto
    | sufijo `esSufijoDe` texto = Just (take (length texto - length sufijo) texto)
    | otherwise = Nothing

esSufijoDe :: String -> String -> Bool
esSufijoDe sufijo texto =
    reverse sufijo `isPrefixOf` reverse texto

extraerCampo :: String -> String -> String -> Maybe String
extraerCampo inicio fin texto = do
    restante <- tomarDespues inicio texto
    return (takeUntil fin restante)

tomarDespues :: String -> String -> Maybe String
tomarDespues marcador texto =
    case stripPrefix marcador texto of
        Just restante -> Just restante
        Nothing ->
            case drop 1 texto of
                "" -> Nothing
                resto -> tomarDespues marcador resto

takeUntil :: String -> String -> String
takeUntil _ "" = ""
takeUntil marcador texto
    | marcador `isPrefixOf` texto = ""
    | otherwise = head texto : takeUntil marcador (tail texto)

parsearDia :: String -> Maybe Day
parsearDia =
    parseTimeM True defaultTimeLocale "%Y-%m-%d" . limpiarEspacios

limpiarEspacios :: String -> String
limpiarEspacios =
    reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- ─── IO ────────────────────────────────────────────────────

cargarRegistros :: IO [RegistroFinanciero]
cargarRegistros = do
    lineas <- leerLineasArchivoSeguro  rutaRegistros  -- rutaRegistros viene de FileManager
    let registros = [r | Just r <- map lineaARegistro lineas]
    return registros

guardarRegistros :: [RegistroFinanciero] -> IO ()
guardarRegistros registros =
    guardarLineasArchivo rutaRegistros (map registroALinea registros)
