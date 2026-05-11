module Models where

import Data.Time (Day)

-- Este módulo define los tipos de datos principales del sistema.
-- La idea es centralizar aquí las estructuras para que todos los módulos
-- usen los mismos modelos y no creen versiones diferentes.

-- Representa una categoría financiera.
-- La categoría existe en una lista independiente y los registros financieros
-- solo guardan el ID de la categoría.
data Categoria = Categoria
    { idCategoria :: Int
    , nombreCategoria :: String
    } deriving (Show, Read, Eq)

-- Representa el tipo principal de movimiento financiero.
data TipoRegistro
    = Ingreso
    | Gasto
    | Ahorro
    | Inversion
    deriving (Show, Read, Eq)

-- Representa un registro financiero.
-- Cada registro pertenece a una categoría mediante idCategoriaRegistro.
-- Las etiquetas sirven para filtrar detalles como fijo, variable,
-- planilla, aguinaldo, playa, mensual, etc.
data RegistroFinanciero = RegistroFinanciero
    { idRegistro :: Int
    , tipoRegistro :: TipoRegistro
    , montoRegistro :: Double
    , idCategoriaRegistro :: Int
    , fechaRegistro :: Day
    , descripcionRegistro :: String
    , etiquetasRegistro :: [String]
    } deriving (Show, Read, Eq)

-- Indica cómo se interpreta un presupuesto.
-- LimiteMaximo: se genera alerta si el gasto real supera el monto.
-- MetaMinima: se genera alerta si lo real no alcanza el monto esperado.
data TipoPresupuesto
    = LimiteMaximo
    | MetaMinima
    deriving (Show, Read, Eq)

-- Representa un presupuesto asociado a una categoría.
data Presupuesto = Presupuesto
    { idPresupuesto :: Int
    , idCategoriaPresupuesto :: Int
    , montoPresupuesto :: Double
    , tipoPresupuesto :: TipoPresupuesto
    } deriving (Show, Read, Eq)

-- Representa una regla configurable.
-- Las reglas base existen en el código, pero el usuario puede modificar
-- valores como la categoría evaluada y el monto límite/meta.
data ConfiguracionRegla = ConfiguracionRegla
    { nombreRegla :: String
    , idCategoriaRegla :: Int
    , montoRegla :: Double
    , tipoPresupuestoRegla :: TipoPresupuesto
    } deriving (Show, Read, Eq)

-- Representa una alerta generada por el sistema.
data Alerta = Alerta
    { mensajeAlerta :: String
    , nivelAlerta :: String
    } deriving (Show, Read, Eq)