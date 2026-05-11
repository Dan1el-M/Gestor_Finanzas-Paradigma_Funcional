# FinanTrack Haskell

Sistema de gestion de finanzas personales desarrollado en Haskell bajo un enfoque funcional. La aplicacion funciona por consola y permite registrar movimientos financieros, administrar categorias, definir presupuestos, evaluar reglas de alerta, generar reportes, ejecutar analisis avanzados y simular escenarios de ahorro o reduccion de gastos.

## Tabla de Contenidos

- [Objetivo](#objetivo)
- [Caracteristicas](#caracteristicas)
- [Requisitos](#requisitos)
- [Ejecucion](#ejecucion)
- [Flujo General](#flujo-general)
- [Arquitectura](#arquitectura)
- [Estructura de Archivos](#estructura-de-archivos)
- [Modelo de Datos](#modelo-de-datos)
- [Persistencia](#persistencia)
- [Logica Funcional](#logica-funcional)
- [Notas de Mantenimiento](#notas-de-mantenimiento)

## Objetivo

FinanTrack centraliza la informacion financiera de una persona mediante registros clasificados por tipo, categoria, fecha y etiquetas. A partir de esos datos, el sistema calcula balances, comparaciones, rankings, presupuestos y alertas para apoyar la toma de decisiones financieras.

El proyecto esta organizado para separar claramente:

- La interfaz de consola.
- La logica de negocio.
- La lectura y escritura de archivos.
- Los modelos compartidos.
- Las utilidades transversales.

## Caracteristicas

- Gestion de registros financieros: ingresos, gastos, ahorros e inversiones.
- Gestion de categorias con IDs persistentes.
- Presupuestos por categoria y comparacion entre gasto real y presupuesto.
- Reglas configurables para alertas de gasto y ahorro minimo.
- Reportes mensuales, comparacion entre periodos y top de categorias con mayor gasto.
- Analisis financiero avanzado: flujo de caja, tendencias, proyecciones y rankings de impacto.
- Simulaciones: reduccion de gastos y proyeccion de ahorro.
- Persistencia local en archivos `.txt` usando representaciones `Show`/`Read` de Haskell.

## Requisitos

- GHC compatible con el proyecto. Actualmente se ha usado `ghc-9.6.7`.
- Cabal 3.x.
- Dependencias declaradas en `finanzas-haskell.cabal`:
  - `base`
  - `time`
  - `deepseq`

## Ejecucion

Desde la raiz del proyecto:

```bash
cabal build
cabal run
```

El ejecutable principal se define en:

```text
app/Main.hs
```

La configuracion del paquete esta en:

```text
finanzas-haskell.cabal
```

## Flujo General

El flujo de ejecucion inicia en `Main`, que delega al menu principal:

```text
app/Main.hs
    -> UI.MainMenu.iniciarAplicacion
        -> menuPrincipal
            -> menus especializados
                -> Services.*
                    -> FileManager
                        -> data/*.txt
```

El usuario interactua con menus de consola. Cada menu solicita datos, valida entradas basicas y llama a servicios especializados. Los servicios realizan la logica de negocio y, cuando corresponde, persisten cambios usando `FileManager`.

Ejemplo: crear una categoria.

```text
UI.CategoryMenu.crearCategoriaMenu
    -> Services.CategoryService.crearCategoriaService
        -> FileManager.guardarCategorias
            -> data/categorias.txt
```

Ejemplo: agregar un registro financiero.

```text
UI.FinanceRegistryMenu.subMenuAgregarRegistroFinanciero
    -> Services.FinanceRegistryService.cargarRegistros
    -> Services.BudgetService.excedePresupuestoConNuevoRegistro
    -> Services.FinanceRegistryService.agregarRegistro
    -> Services.FinanceRegistryService.guardarRegistros
    -> Services.RuleService.evaluarReglasAlRegistrar
```

## Arquitectura

El proyecto sigue una arquitectura por capas simple:

```text
┌─────────────────────────────┐
│ UI                          │
│ Menus, prompts y tablas     │
└──────────────┬──────────────┘
               │
┌──────────────▼──────────────┐
│ Services                    │
│ Logica de negocio pura/IO   │
└──────────────┬──────────────┘
               │
┌──────────────▼──────────────┐
│ FileManager                 │
│ Persistencia en archivos    │
└──────────────┬──────────────┘
               │
┌──────────────▼──────────────┐
│ data/*.txt                  │
│ Datos persistidos           │
└─────────────────────────────┘
```

`Models` queda como modulo transversal. Todos los menus y servicios comparten las mismas estructuras de datos, evitando duplicidad de tipos.

## Estructura de Archivos

### Entrada y configuracion

| Archivo | Funcion |
| --- | --- |
| `app/Main.hs` | Punto de entrada del ejecutable. Llama a `iniciarAplicacion`. |
| `finanzas-haskell.cabal` | Define el paquete, dependencias, modulos internos, version de Haskell y opciones de compilacion. |

### Modelos y utilidades

| Archivo | Funcion |
| --- | --- |
| `src/Models.hs` | Define los tipos principales: `Categoria`, `RegistroFinanciero`, `TipoRegistro`, `Presupuesto`, `TipoPresupuesto`, `ConfiguracionRegla` y `Alerta`. |
| `src/Utils.hs` | Funciones auxiliares para separar texto, parsear numeros, parsear fechas y procesar etiquetas. |
| `src/FileManager.hs` | Centraliza rutas de archivos, lectura segura, escritura y carga/guardado de categorias, presupuestos y reglas. |

### Capa UI

| Archivo | Funcion |
| --- | --- |
| `src/UI/UIHelpers.hs` | Helpers visuales para consola: titulos, bordes, mensajes de exito/error, prompts, padding, separadores y formato monetario. |
| `src/UI/MainMenu.hs` | Menu principal y navegacion hacia los modulos funcionales. |
| `src/UI/FinanceRegistryMenu.hs` | Interfaz para agregar, listar, editar y eliminar registros financieros. Tambien muestra alertas generadas por reglas. |
| `src/UI/CategoryMenu.hs` | Interfaz para crear, listar, buscar, actualizar y eliminar categorias. Muestra categorias como tabla con IDs persistentes. |
| `src/UI/BudgetMenu.hs` | Interfaz para asignar presupuestos por categoria y comparar gasto real contra presupuesto. |
| `src/UI/RuleMenu.hs` | Interfaz para configurar y visualizar reglas de gasto por categoria y ahorro minimo. |
| `src/UI/ReportMenu.hs` | Interfaz para reportes mensuales, comparacion de periodos y categorias con mayor gasto. |
| `src/UI/FinanceAnalysisMenu.hs` | Interfaz para analisis avanzado: flujo de caja, tendencias, proyecciones y ranking de impacto. |
| `src/UI/SimulationMenu.hs` | Interfaz para simulaciones de reduccion de gastos y proyeccion de ahorro. |

### Capa Services

| Archivo | Funcion |
| --- | --- |
| `src/Services/FinanceRegistryService.hs` | Logica para registros financieros: agregar, filtrar por tipo/categoria/etiqueta, totalizar, filtrar por mes, serializar y cargar/guardar registros. |
| `src/Services/CategoryService.hs` | Logica de categorias: crear, buscar, actualizar, eliminar, validar nombres y calcular siguiente ID sin reutilizar IDs borrados. |
| `src/Services/BudgetService.hs` | Logica de presupuestos: asegurar presupuestos por defecto, asignar presupuesto, calcular gasto real por categoria, comparar real vs presupuesto y detectar excedentes. |
| `src/Services/RuleService.hs` | Logica de reglas configurables: reglas por defecto, configuracion de limites, evaluacion de alertas al registrar movimientos. |
| `src/Services/ReportService.hs` | Logica de reportes: resumen mensual, comparacion entre periodos y top de categorias con mayor gasto. |
| `src/Services/FinanceAnalysisService.hs` | Analisis avanzado: flujo de caja mensual, frecuencia de categorias, proyeccion de gastos historicos y rankings por impacto financiero. |
| `src/Services/SimulationService.hs` | Simulaciones: reduccion porcentual de gastos y proyeccion de ahorro desde promedios historicos. |
| `src/Services/DateService.hs` | Solicitud y validacion de fechas. Valida año, mes y dia usando `Data.Time`. |

### Datos

| Archivo | Funcion |
| --- | --- |
| `data/registros.txt` | Registros financieros persistidos como `RegistroFinanciero`. |
| `data/categorias.txt` | Categorias persistidas como `Categoria`. Los IDs son estables para no romper registros existentes. |
| `data/presupuestos.txt` | Presupuestos persistidos como `Presupuesto`. |
| `data/reglas.txt` | Reglas persistidas como `ConfiguracionRegla`. |

## Modelo de Datos

### Categoria

Representa una clasificacion financiera. Su ID es importante porque los registros, presupuestos y reglas referencian categorias por ID.

```haskell
Categoria
    { idCategoria :: Int
    , nombreCategoria :: String
    }
```

Los IDs de categoria son persistentes. Si se elimina una categoria con ID `8`, una categoria con ID `9` conserva su ID `9`.

### RegistroFinanciero

Representa un movimiento financiero.

```haskell
RegistroFinanciero
    { idRegistro :: Int
    , tipoRegistro :: TipoRegistro
    , montoRegistro :: Double
    , idCategoriaRegistro :: Int
    , fechaRegistro :: Day
    , descripcionRegistro :: String
    , etiquetasRegistro :: [String]
    }
```

`TipoRegistro` puede ser:

```haskell
Ingreso | Gasto | Ahorro | Inversion
```

### Presupuesto

Relaciona una categoria con un monto presupuestado.

```haskell
Presupuesto
    { idPresupuesto :: Int
    , idCategoriaPresupuesto :: Int
    , montoPresupuesto :: Double
    , tipoPresupuesto :: TipoPresupuesto
    }
```

`TipoPresupuesto` puede ser:

```haskell
LimiteMaximo | MetaMinima
```

### ConfiguracionRegla

Representa reglas de alerta configurables.

```haskell
ConfiguracionRegla
    { nombreRegla :: String
    , idCategoriaRegla :: Int
    , montoRegla :: Double
    , tipoPresupuestoRegla :: TipoPresupuesto
    }
```

## Persistencia

La persistencia se realiza en archivos de texto bajo `data/`. El proyecto usa `Show` y `Read` para convertir estructuras Haskell a texto y viceversa.

Ejemplo de categoria:

```haskell
Categoria {idCategoria = 9, nombreCategoria = "Futbol"}
```

Ejemplo de registro:

```haskell
RegistroFinanciero {idRegistro = 1, tipoRegistro = Gasto, montoRegistro = 1200.0, idCategoriaRegistro = 9, fechaRegistro = 2026-05-11, descripcionRegistro = "Compra", etiquetasRegistro = ["variable"]}
```

`FileManager` contiene las rutas y funciones comunes:

- `leerLineasArchivo`
- `leerLineasArchivoSeguro`
- `guardarLineasArchivo`
- `cargarCategorias` / `guardarCategorias`
- `cargarPresupuestos` / `guardarPresupuestos`
- `cargarReglas` / `guardarReglas`

Los registros financieros tienen funciones de carga/guardado en `FinanceRegistryService`, porque ese modulo tambien contiene compatibilidad con formatos de registros mostrados anteriormente.

## Logica Funcional

El proyecto aprovecha patrones funcionales caracteristicos de Haskell:

- Transformaciones con `map`, `filter`, `foldr` y `foldl`.
- Datos inmutables: las operaciones devuelven nuevas listas en vez de modificar estructuras en sitio.
- Validaciones con `Maybe` y `Either`.
- Separacion entre funciones puras y funciones con efectos `IO`.
- Composicion de funciones para calculos financieros.

Ejemplos:

- `compararRealVsPresupuesto` calcula una nueva lista de comparaciones desde categorias, presupuestos y registros.
- `generarResumenMensual` filtra registros por periodo y tipo para construir un resumen.
- `simularReduccionGastos` retorna `Either String ResultadoReduccion`, separando error y resultado valido.
- `crearCategoriaService` valida entrada y devuelve `Either String [Categoria]`.

## Flujo de Funcionalidades

### Registros financieros

1. El usuario selecciona gestion de registros.
2. Puede agregar, ver, editar o eliminar registros.
3. Al agregar un registro, el sistema solicita:
   - tipo,
   - monto,
   - categoria,
   - fecha,
   - descripcion,
   - etiquetas.
4. Si el registro es un gasto, se valida si excede el presupuesto configurado.
5. Se guardan los datos en `data/registros.txt`.
6. Se evaluan reglas y se muestran alertas si aplica.

### Categorias

1. El usuario crea, lista, busca, actualiza o elimina categorias.
2. Al crear, el ID se calcula como `maximum idCategoria + 1`.
3. Al eliminar, se filtra la categoria sin renumerar las demas.
4. No se permite eliminar una categoria usada por registros financieros.

### Presupuestos

1. El sistema asegura que cada categoria tenga presupuesto por defecto.
2. El usuario puede asignar un monto a una categoria.
3. La comparacion calcula:
   - gasto real acumulado,
   - monto presupuestado,
   - diferencia entre presupuesto y gasto real.

### Analisis avanzado

Incluye:

- Flujo de caja mensual.
- Categorias con mayor frecuencia de gasto.
- Proyeccion de gastos historicos.
- Ranking de categorias por impacto financiero anual o mensual.

### Simulaciones

Permite estimar:

- Nuevo gasto luego de aplicar una reduccion porcentual.
- Ahorro acumulado proyectado a partir de promedios historicos.

### Reglas

Hay dos reglas principales:

- Gasto por categoria: alerta si el gasto acumulado supera un limite.
- Ahorro minimo: advertencia si un ahorro registrado es menor al minimo configurado.

### Reportes

Los reportes permiten:

- Resumen mensual por tipo de movimiento.
- Comparacion entre dos periodos.
- Top N de categorias con mayor gasto.

## Notas de Mantenimiento

- Al agregar un nuevo modulo en `src/`, tambien debe registrarse en `other-modules` dentro de `finanzas-haskell.cabal`.
- No se deben renumerar categorias, porque los registros financieros guardan solo `idCategoriaRegistro`.
- Si se cambia la estructura de un tipo en `Models.hs`, se debe revisar la compatibilidad de lectura en `FileManager` y servicios relacionados.
- Mantener la UI en `src/UI/*` y la logica en `src/Services/*` facilita pruebas y evita mezclar entrada/salida con calculos puros.
- Los archivos en `data/` forman parte del estado local de la aplicacion. Cambiarlos manualmente puede afectar reportes, presupuestos y reglas.

