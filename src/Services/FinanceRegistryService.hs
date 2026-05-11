module Services.FinanceRegistryService where

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
    case reads linea of
        [(r, "")] -> Just r
        _         -> Nothing

-- ─── IO ────────────────────────────────────────────────────

cargarRegistros :: IO [RegistroFinanciero]
cargarRegistros = do
    lineas <- leerLineasArchivoSeguro  rutaRegistros  -- rutaRegistros viene de FileManager
    let registros = [r | Just r <- map lineaARegistro lineas]
    return registros

guardarRegistros :: [RegistroFinanciero] -> IO ()
guardarRegistros registros =
    guardarLineasArchivo rutaRegistros (map registroALinea registros)
