--Aquí van funciones auxiliares.

module Utils where

import Data.Time (Day, defaultTimeLocale, parseTimeM)

-- Este módulo contiene funciones auxiliares que pueden ser usadas
-- por varias partes del sistema.

-- Divide un String usando un carácter separador.
-- Ejemplo:
-- splitBy '|' "Gasto|12000|Alimentacion"
-- Resultado: ["Gasto", "12000", "Alimentacion"]
separaPor :: Char -> String -> [String]
separaPor _ "" = [""]
separaPor separator text =
    case break (== separator) text of
        (part, "") -> [part]
        (part, _ : rest) -> part : separaPor separator rest

-- Convierte texto a Double de forma segura.
-- Si el texto no es un número válido, retorna Nothing.
-- Esto evita que el programa se caiga por entradas incorrectas.
guardarLecturaDoble :: String -> Maybe Double
guardarLecturaDoble text =
    case reads text of
        [(number, "")] -> Just number
        _ -> Nothing

-- Convierte una fecha en formato texto a Day.
-- El formato esperado es YYYY-MM-DD.
-- Ejemplo válido: 2026-05-10
parseaFecha :: String -> Maybe Day
parseaFecha =
    parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- Convierte una lista de etiquetas separadas por coma en una lista.
-- Ejemplo:
-- "fijo,mensual,salario"
-- Resultado: ["fijo", "mensual", "salario"]
parsearTags :: String -> [String]
parsearTags "" = []
parsearTags text = separaPor ',' text