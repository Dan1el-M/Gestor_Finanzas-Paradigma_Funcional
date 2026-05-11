module Services.DateService where

import Data.Time (Day, fromGregorian, gregorianMonthLength)
import Text.Read (readMaybe)
import UI.UIHelpers (cerrar, err, ok, prompt, titulo)

-- ─── Lógica pura ───────────────────────────────────────────

fechaValida :: Integer -> Int -> Int -> Bool
fechaValida anio mes dia =
    mes `elem` [1 .. 12] &&
    dia `elem` [1 .. gregorianMonthLength anio mes]

construirFecha :: Integer -> Int -> Int -> Maybe Day
construirFecha anio mes dia
    | fechaValida anio mes dia = Just (fromGregorian anio mes dia)
    | otherwise                = Nothing

-- ─── Helpers funcionales ───────────────────────────────────

reintentar :: String -> IO a -> IO a
reintentar mensaje accion = do
    err mensaje
    accion

validarRango :: (Ord a) => a -> a -> a -> Bool
validarRango minVal maxVal valor =
    valor >= minVal && valor <= maxVal

leerNumero :: Read a => IO (Maybe a)
leerNumero = readMaybe <$> getLine

-- ─── IO ────────────────────────────────────────────────────

pedirFecha :: IO Day
pedirFecha = do
    titulo "Ingresar Fecha"
    cerrar

    anio <- pedirAnio
    mes  <- pedirMes
    dia  <- pedirDia anio mes

    let fecha = fromGregorian anio mes dia

    ok $ "Fecha seleccionada: " ++ show fecha
    pure fecha

pedirAnio :: IO Integer
pedirAnio = do
    input <- prompt "Año"
    maybe
        (reintentar "Debe ingresar un numero entero." pedirAnio)
        validar
        (readMaybe input)
  where
    validar anio
        | validarRango 1900 2100 anio = pure anio
        | otherwise =
            reintentar
                "Año invalido. Debe ser entre 1900 y 2100."
                pedirAnio

pedirMes :: IO Int
pedirMes = do
    putStrLn ""
    putStrLn "  Ene(1)  Feb(2)  Mar(3)  Abr(4)"
    putStrLn "  May(5)  Jun(6)  Jul(7)  Ago(8)"
    putStrLn "  Sep(9)  Oct(10) Nov(11) Dic(12)"

    input <- prompt "Mes"
    maybe
        (reintentar "Debe ingresar un numero." pedirMes)
        validar
        (readMaybe input)
  where
    validar mes
        | validarRango 1 12 mes = pure mes
        | otherwise =
            reintentar
                "Mes invalido. Debe ser entre 1 y 12."
                pedirMes

pedirDia :: Integer -> Int -> IO Int
pedirDia anio mes = do
    input <- prompt $ "Dia (1-" ++ show maxDias ++ ")"
    maybe
        (reintentar "Debe ingresar un numero." (pedirDia anio mes))
        validar
        (readMaybe input)
  where
    maxDias = gregorianMonthLength anio mes

    validar dia
        | validarRango 1 maxDias dia = pure dia
        | otherwise =
            reintentar
                ("Dia invalido. Este mes tiene " ++ show maxDias ++ " dias.")
                (pedirDia anio mes)
