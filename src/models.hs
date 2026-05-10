-- aca van los tipos d edatos del proyecto

module Models where

import Data.Time (Day)

-- Este módulo define los tipos de datos principales del sistema.
-- La idea es que todos los demás módulos usen estas estructuras


-- Representa el tipo de movimiento financiero.
-- Un registro puede ser ingreso, gasto, ahorro o inversión.
data TipoRegistro
    = Ingreso
    | Gasto
    | Ahorro
    | Inversion
    deriving (Show, Read, Eq)

-- Representa un registro financiero completo.
-- Este será el dato principal del sistema.
data RegistroFinanciero = RegistroFinanciero
    { tipoRegistro :: TipoRegistro
    , montoRegistro :: Double
    , categoriaRegistro :: String
    , fechaRegistro :: Day
    , descripcionRegistro :: String
    , etiquetasRegistro :: [String]
    } deriving (Show, Read, Eq)

-- Representa un presupuesto definido para una categoría.
-- Ejemplo: Alimentacion con límite de 100000 colones.
data Presupuesto = Presupuesto
    { categoriaPresupuesto :: String
    , limitePresupuesto :: Double
    } deriving (Show, Read, Eq)

-- Representa una regla del sistema.
-- Ejemplo: si Alimentacion supera 100000, mostrar una alerta.
data Regla = Regla
    { tipoRegla :: String
    , categoriaRegla :: String
    , limiteRegla :: Double
    , mensajeRegla :: String
    } deriving (Show, Read, Eq)

-- Representa una alerta generada por el sistema.
-- Las alertas no necesariamente se guardan; pueden generarse al evaluar reglas.
data Alerta = Alerta
    { mensajeAlerta :: String
    , nivelAlerta :: String
    } deriving (Show, Read, Eq)