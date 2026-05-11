module UI.UIHelpers where

-- Helpers de UI en consola con ancho fijo para mantener simetría.
-- Ancho interno: 50 caracteres (incluye los 2 espacios iniciales del contenido).

anchoInterno :: Int
anchoInterno = 50

lineaTop :: String
lineaTop = "  +" ++ replicate anchoInterno '-' ++ "+"

linea :: String
linea = lineaTop

lineaBot :: String
lineaBot = lineaTop

titulo :: String -> IO ()
titulo t = do
    putStrLn ""
    putStrLn lineaTop
    putStrLn $ "  |" ++ padR anchoInterno ("  " ++ t) ++ "|"
    putStrLn linea

cerrar :: IO ()
cerrar = putStrLn lineaBot

ok :: String -> IO ()
ok msg = putStrLn $ "  [OK] " ++ msg

err :: String -> IO ()
err msg = putStrLn $ "  [!] " ++ msg

padR :: Int -> String -> String
padR n s = take n (s ++ repeat ' ')

-- Imprime una línea dentro de la caja (sin romper la simetría).
enCaja :: String -> IO ()
enCaja s = putStrLn $ "  |" ++ padR anchoInterno ("  " ++ s) ++ "|"

mostrarMonto :: Double -> String
mostrarMonto m = "CRC " ++ show m

