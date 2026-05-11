module UI.UIHelpers where

-- ─── Bordes ────────────────────────────────────────────────

lineaTop :: String
lineaTop = "  ╔══════════════════════════════════════════════════╗"

linea :: String
linea = "  ╠══════════════════════════════════════════════════╣"

lineaBot :: String
lineaBot = "  ╚══════════════════════════════════════════════════╝"

-- ─── Helpers de impresión ──────────────────────────────────

titulo :: String -> IO ()
titulo t = do
    putStrLn ""
    putStrLn lineaTop
    putStrLn $ "  ║  " ++ t ++ replicate (48 - length t) ' ' ++ "║"
    putStrLn linea

cerrar :: IO ()
cerrar = putStrLn lineaBot

ok :: String -> IO ()
ok msg = putStrLn $ "  ✓ " ++ msg

err :: String -> IO ()
err msg = putStrLn $ "  ✗ " ++ msg

-- Rellena con espacios a la derecha hasta el ancho dado
padR :: Int -> String -> String
padR n s = take n (s ++ repeat ' ')