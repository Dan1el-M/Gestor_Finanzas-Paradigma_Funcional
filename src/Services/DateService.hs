module Services.DateService where

import System.IO (hFlush, stdout)
import Data.Time (Day, fromGregorian, gregorianMonthLength)
import Text.Read (readMaybe)

-- ─── Lógica pura ───────────────────────────────────────────

fechaValida :: Integer -> Int -> Int -> Bool
fechaValida anio mes dia =
    mes >= 1 && mes <= 12 &&
    dia >= 1 && dia <= gregorianMonthLength anio mes

construirFecha :: Integer -> Int -> Int -> Maybe Day
construirFecha anio mes dia
    | fechaValida anio mes dia = Just $ fromGregorian anio mes dia
    | otherwise                = Nothing

-- ─── IO ────────────────────────────────────────────────────

pedirFecha :: IO Day
pedirFecha = do
    anio <- pedirAnio
    mes  <- pedirMes
    dia  <- pedirDia anio mes
    return $ fromGregorian anio mes dia

pedirAnio :: IO Integer
pedirAnio = do
    putStr "Año (ej: 2025): "
    hFlush stdout
    input <- getLine
    case readMaybe input :: Maybe Integer of
        Nothing -> do
            putStrLn "Debe ingresar un numero."
            pedirAnio
        Just anio ->
            if anio < 1900 || anio > 2100
                then do
                    putStrLn "Anio invalido. Debe ser entre 1900 y 2100."
                    pedirAnio
                else return anio

pedirMes :: IO Int
pedirMes = do
    putStr "Mes (1-12): "
    hFlush stdout
    input <- getLine
    case readMaybe input :: Maybe Int of
        Nothing -> do
            putStrLn "Debe ingresar un numero."
            pedirMes
        Just mes ->
            if mes < 1 || mes > 12
                then do
                    putStrLn "Mes invalido. Debe ser entre 1 y 12."
                    pedirMes
                else return mes

pedirDia :: Integer -> Int -> IO Int
pedirDia anio mes = do
    let maxDias = gregorianMonthLength anio mes
    putStr $ "Dia (1-" ++ show maxDias ++ "): "
    hFlush stdout
    input <- getLine
    case readMaybe input :: Maybe Int of
        Nothing -> do
            putStrLn "Debe ingresar un numero."
            pedirDia anio mes
        Just dia ->
            if dia < 1 || dia > maxDias
                then do
                    putStrLn $ "Dia invalido. El mes tiene " ++ show maxDias ++ " dias."
                    pedirDia anio mes
                else return dia