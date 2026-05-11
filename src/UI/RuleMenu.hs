module UI.RuleMenu where

import Text.Read (readMaybe)

import FileManager (cargarReglas, guardarReglas)
import Models
import qualified Services.RuleService as Service
import UI.CategoryMenu (pedirIdCategoria)
import UI.UIHelpers
    ( cerrar, err, menuOpciones, mostrarMonto, ok, opcion, prompt, promptOpcion
    , titulo
    )

menuReglas :: IO ()
menuReglas = do
    menuOpciones "Reglas y Alertas"
        [ opcion 1 "Ver reglas configuradas"
        , opcion 2 "Configurar regla de gasto por categoria"
        , opcion 3 "Configurar regla de ahorro minimo"
        , opcion 4 "Volver al menu principal"
        ]
    seleccion <- promptOpcion
    case seleccion of
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
    titulo "Regla de Gasto"
    cerrar
    idCategoriaSeleccionada <- pedirIdCategoria
    textoMonto <- prompt "Monto limite de gastos"
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
    titulo "Regla de Ahorro"
    cerrar
    textoMonto <- prompt "Monto minimo de ahorro"
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
    putStrLn $ "    Monto: " ++ mostrarMonto (montoRegla regla)
    putStrLn $ "    Tipo: " ++ show (tipoPresupuestoRegla regla)

mostrarCategoriaRegla :: ConfiguracionRegla -> String
mostrarCategoriaRegla regla
    | nombreRegla regla == Service.nombreReglaAhorroMinimo = "No aplica"
    | otherwise = show (idCategoriaRegla regla)
