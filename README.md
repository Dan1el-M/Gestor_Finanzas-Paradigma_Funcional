# Gestor_Finanzas-Paradigma_Funcional

## Cambios recientes

Resolví los conflictos manteniendo lo visual que integraron y preservando (y mejorando) toda la lógica de presupuestos.

- Conflictos arreglados y limpios: `finanzas-haskell.cabal` y `src/UI/FinanceRegistryMenu.hs`.
- Opción 2 de “Gestionar presupuestos” ahora imprime la comparación en tabla usando `UIHelpers`: `src/UI/BudgetMenu.hs:69`.
- Reemplacé todos los `if` por `case` en el proyecto (los que existían en `DateService`, `CategoryService` y menús).
- Warning de exceso de presupuesto ahora aparece apenas se ingresa `monto` + `categoría` (antes de pedir fecha/descripcion): `src/UI/FinanceRegistryMenu.hs:55`, apoyado por `src/Services/BudgetService.hs:130`.
- Menú principal vuelve a llamar al módulo de presupuestos en la opción 2: `src/UI/MainMenu.hs:5`.

Si querés, corro `cabal run` para que lo veás funcionando (solo asegurate de no tener abierto `finanzas-haskell.exe`, porque en Windows bloquea el linker).
