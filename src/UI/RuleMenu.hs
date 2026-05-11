module UI.RuleMenu where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import FileManager (cargarReglas, guardarReglas)
import Models
import qualified Services.RuleService as Service
import UI.CategoryMenu (pedirIdCategoria)
import UI.UIHelpers (cerrar, err, ok, titulo)

menuReglas :: IO ()
menuReglas = do
    titulo "Reglas y Alertas"
    putStrLn "  ║  1. Ver reglas configuradas                      ║"
    putStrLn "  ║  2. Configurar regla de gasto por categoria      ║"
    putStrLn "  ║  3. Configurar regla de ahorro minimo            ║"
    putStrLn "  ║  4. Volver al menu principal                     ║"
    cerrar
    putStr "  Opcion » "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> verReglasMenu >> menuReglas
        "2" -> configurarGastoMenu >> menuReglas
        "3" -> configurarAhorroMenu >> menuReglas
        "4" -> ok "Volviendo al menu principal..."
        _   -> err "Opcion invalida." >> menuReglas

verReglasMenu :: IO ()
verReglasMenu = do
    reglas <- cargarReglasConfiguradas
    titulo "Reglas Configuradas"
    mapM_ mostrarRegla reglas
    cerrar

configurarGastoMenu :: IO ()
configurarGastoMenu = do
    putStrLn ""
    putStrLn "Seleccione la categoria que evaluara la regla de gastos:"
    idCategoriaSeleccionada <- pedirIdCategoria
    putStr "  Monto limite de gastos » "
    hFlush stdout
    textoMonto <- getLine
    case readMaybe textoMonto :: Maybe Double of
        Nothing -> err "Monto invalido. Debe ingresar un numero."
        Just montoLimite -> do
            reglas <- cargarReglas
            case Service.configurarReglaGasto idCategoriaSeleccionada montoLimite reglas of
                Left mensaje -> err mensaje
                Right actualizadas -> do
                    guardarReglas actualizadas
                    ok "Regla de gasto por categoria configurada correctamente."

configurarAhorroMenu :: IO ()
configurarAhorroMenu = do
    putStr "  Monto minimo de ahorro » "
    hFlush stdout
    textoMonto <- getLine
    case readMaybe textoMonto :: Maybe Double of
        Nothing -> err "Monto invalido. Debe ingresar un numero."
        Just montoMinimo -> do
            reglas <- cargarReglas
            case Service.configurarReglaAhorro montoMinimo reglas of
                Left mensaje -> err mensaje
                Right actualizadas -> do
                    guardarReglas actualizadas
                    ok "Regla de ahorro minimo configurada correctamente."

cargarReglasConfiguradas :: IO [ConfiguracionRegla]
cargarReglasConfiguradas = do
    reglas <- cargarReglas
    return (Service.asegurarReglasPorDefecto reglas)

mostrarRegla :: ConfiguracionRegla -> IO ()
mostrarRegla regla = do
    putStrLn $ "  Regla: " ++ nombreRegla regla
    putStrLn $ "    Categoria: " ++ mostrarCategoriaRegla regla
    putStrLn $ "    Monto: " ++ show (montoRegla regla)
    putStrLn $ "    Tipo: " ++ show (tipoPresupuestoRegla regla)

mostrarCategoriaRegla :: ConfiguracionRegla -> String
mostrarCategoriaRegla regla
    | nombreRegla regla == Service.nombreReglaAhorroMinimo = "No aplica"
    | otherwise = show (idCategoriaRegla regla)
