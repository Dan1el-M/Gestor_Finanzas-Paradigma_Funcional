--Aquí va el sistema de reglas y alertas.

module Rules where

import Models

-- Este módulo evaluará las reglas financieras del sistema.
-- Ejemplo: si los gastos en una categoría superan cierto monto,
-- se genera una alerta.

-- Función base para crear una alerta simple.
crearAlerta :: String -> Alerta
crearAlerta mensaje = Alerta
    { mensajeAlerta = mensaje
    , nivelAlerta = "Advertencia"
    }