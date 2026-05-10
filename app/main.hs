-- aarchvios main para llamar a los archivo de Ui y de logica de negocio

module Main where

import System.IO (hFlush, stdout)

-- Importamos los módulos del proyecto.
-- Aunque algunos todavía tienen funciones básicas,
-- esto deja preparada la estructura para integrar todo.
import FileManager
import Reports

-- Punto de entrada del programa.
-- Cuando se ejecuta "cabal run", Cabal inicia desde aquí.
main :: IO ()
main = do
    putStrLn "===================================="
    putStrLn "        FinanTrack Haskell"
    putStrLn " Sistema de Finanzas Personales"
    putStrLn "===================================="
    menuPrincipal

-- Muestra el menú principal y lee la opción del usuario.
-- Esta función usa IO porque interactúa con la consola.
menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn ""
    putStrLn "Seleccione una opcion:"
    putStrLn "1. Registrar movimiento financiero"
    putStrLn "2. Ver registros financieros"
    putStrLn "3. Gestionar presupuestos"
    putStrLn "4. Evaluar reglas y alertas"
    putStrLn "5. Ver analisis financiero"
    putStrLn "6. Simular escenario financiero"
    putStrLn "7. Generar reportes"
    putStrLn "8. Guardar datos"
    putStrLn "9. Salir"
    putStr "Opcion: "
    hFlush stdout

    opcion <- getLine
    ejecutarOpcion opcion

-- Ejecuta la acción correspondiente según la opción ingresada.
-- Por ahora algunas opciones son placeholders,
-- luego cada compañero conectará aquí sus funciones.
ejecutarOpcion :: String -> IO ()
ejecutarOpcion opcion =
    case opcion of
        "1" -> do
            putStrLn "Modulo de registro financiero pendiente de implementar."
            menuPrincipal

        "2" -> do
            putStrLn "Modulo para ver registros pendiente de implementar."
            menuPrincipal

        "3" -> do
            putStrLn "Modulo de presupuestos pendiente de implementar."
            menuPrincipal

        "4" -> do
            putStrLn "Modulo de reglas y alertas pendiente de implementar."
            menuPrincipal

        "5" -> do
            putStrLn "Modulo de analisis financiero pendiente de implementar."
            menuPrincipal

        "6" -> do
            putStrLn "Modulo de simulacion pendiente de implementar."
            menuPrincipal

        "7" -> do
            putStrLn "Generando reporte basico de prueba..."
            putStrLn (generarResumenBasico [])
            menuPrincipal

        "8" -> do
            putStrLn "Guardando datos de prueba..."
            guardarLineasArchivo rutaRegistros []
            putStrLn "Datos guardados correctamente."
            menuPrincipal

        "9" -> do
            putStrLn "Saliendo del sistema..."

        _ -> do
            putStrLn "Opcion invalida. Intente de nuevo."
            menuPrincipal