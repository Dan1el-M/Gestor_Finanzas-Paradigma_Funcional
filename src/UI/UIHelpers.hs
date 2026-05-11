module UI.UIHelpers where

import System.IO (hFlush, stdout)
import Text.Printf (printf)

anchoInterno :: Int
anchoInterno = 50

-- ─── Bordes ────────────────────────────────────────────────

lineaTop :: String
lineaTop = "  ╔" ++ replicate anchoInterno '═' ++ "╗"

linea :: String
linea = "  ╠" ++ replicate anchoInterno '═' ++ "╣"

lineaBot :: String
lineaBot = "  ╚" ++ replicate anchoInterno '═' ++ "╝"

-- ─── Helpers de impresión ─────────────────────────────────

titulo :: String -> IO ()
titulo t = do
    putStrLn ""
    putStrLn lineaTop
    enCajaTitulo t
    putStrLn linea

cerrar :: IO ()
cerrar = putStrLn lineaBot

ok :: String -> IO ()
ok msg = putStrLn $ "  ✓ " ++ msg

err :: String -> IO ()
err msg = putStrLn $ "  ✗ " ++ msg

-- Rellena con espacios a la derecha hasta el ancho dado.
padR :: Int -> String -> String
padR n s = take n (s ++ repeat ' ')

padL :: Int -> String -> String
padL n s = replicate (max 0 (n - length s)) ' ' ++ take n s

enCaja :: String -> IO ()
enCaja s = putStrLn $ "  ║" ++ padR anchoInterno ("  " ++ s) ++ "║"

enCajaTitulo :: String -> IO ()
enCajaTitulo t = putStrLn $ "  ║" ++ centrar anchoInterno t ++ "║"

centrar :: Int -> String -> String
centrar n s =
    let limpio = take n s
        total = max 0 (n - length limpio)
        izq = total `div` 2
        der = total - izq
    in replicate izq ' ' ++ limpio ++ replicate der ' '

menuOpciones :: String -> [String] -> IO ()
menuOpciones nombre opciones = do
    titulo nombre
    mapM_ enCaja opciones
    cerrar

opcion :: Int -> String -> String
opcion n texto = show n ++ ". " ++ texto

prompt :: String -> IO String
prompt etiqueta = do
    putStr $ "  " ++ etiqueta ++ " » "
    hFlush stdout
    getLine

promptOpcion :: IO String
promptOpcion = prompt "Opcion"

separador :: IO ()
separador = putStrLn $ "  " ++ replicate anchoInterno '─'

mostrarMonto :: Double -> String
mostrarMonto m = printf "₡%.2f" m

