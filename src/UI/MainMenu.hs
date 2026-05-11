module UI.MainMenu where

import UI.CategoryMenu (menuCategorias)
import System.IO (hFlush, stdout)
import FileManager
import Reports
import Models

-- Inicia la aplicación con datos cargados
iniciarAplicacion :: [Categoria] -> [RegistroFinanciero] -> IO ()
iniciarAplicacion categorias registros = do
    menuPrincipal categorias registros

menuPrincipal :: [Categoria] -> [RegistroFinanciero] -> IO ()
menuPrincipal categorias registros = do
    putStrLn ""
    putStrLn "Seleccione una opcion:"
    putStrLn "1. Registrar movimiento financiero"
    putStrLn "2. Ver registros financieros"
    putStrLn "3. Gestionar categorias"
    putStrLn "4. Gestionar presupuestos"
    putStrLn "5. Evaluar reglas y alertas"
    putStrLn "6. Ver analisis financiero"
    putStrLn "7. Simular escenario financiero"
    putStrLn "8. Generar reportes"
    putStrLn "9. Guardar datos"
    putStrLn "10. Salir"
    putStr "Opcion: "
    hFlush stdout

    opcion <- getLine
    ejecutarOpcion opcion categorias registros

-- Ejecuta la acción correspondiente según la opción ingresada.
ejecutarOpcion :: String -> [Categoria] -> [RegistroFinanciero] -> IO ()
ejecutarOpcion opcion categorias registros =
    case opcion of
        "1" -> do
            putStrLn "Modulo de registro financiero pendiente de implementar."
            menuPrincipal categorias registros

        "2" -> do
            putStrLn "Modulo para ver registros pendiente de implementar."
            menuPrincipal categorias registros

        "3" -> do
            categoriasActualizadas <- menuCategorias categorias registros
            menuPrincipal categoriasActualizadas registros

        "4" -> do
            putStrLn "Modulo de presupuestos pendiente de implementar."
            menuPrincipal categorias registros

        "5" -> do
            putStrLn "Modulo de reglas y alertas pendiente de implementar."
            menuPrincipal categorias registros

        "6" -> do
            putStrLn "Modulo de analisis financiero pendiente de implementar."
            menuPrincipal categorias registros

        "7" -> do
            putStrLn "Generando reporte basico de prueba..."
            putStrLn (generarResumenBasico registros)
            menuPrincipal categorias registros

        "8" -> do
            putStrLn "Guardando datos de prueba..."
            guardarLineasArchivo rutaRegistros []
            putStrLn "Datos guardados correctamente."
            menuPrincipal categorias registros

        "9" -> do
            guardarCategorias categorias
            putStrLn "Datos guardados correctamente."
            menuPrincipal categorias registros

        "10" -> do
            putStrLn "Saliendo del sistema..."

        _ -> do
            putStrLn "Opcion invalida. Intente de nuevo."
            menuPrincipal categorias registros