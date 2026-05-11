module CategoryManager where

import Models

-- Este módulo maneja operaciones con categorías.
-- Proporciona funciones para gestionar categorías financieras.

-- Busca una categoría por su ID
buscarCategoriaPorId :: Int -> [Categoria] -> Maybe Categoria
buscarCategoriaPorId _ [] = Nothing
buscarCategoriaPorId idBuscado (c:cs)
    | idCategoria c == idBuscado = Just c
    | otherwise = buscarCategoriaPorId idBuscado cs

-- Busca una categoría por nombre
buscarCategoriaPorNombre :: String -> [Categoria] -> Maybe Categoria
buscarCategoriaPorNombre _ [] = Nothing
buscarCategoriaPorNombre nombreBuscado (c:cs)
    | nombreCategoria c == nombreBuscado = Just c
    | otherwise = buscarCategoriaPorNombre nombreBuscado cs

-- Agrega una nueva categoría a la lista
agregarCategoria :: Categoria -> [Categoria] -> [Categoria]
agregarCategoria cat cats = cats ++ [cat]

-- Obtiene todas las categorías (función placeholder para futuras extensiones)
obtenerCategorias :: [Categoria] -> [Categoria]
obtenerCategorias = id

-- Crea una nueva categoría con ID autogenerado
crearCategoria :: String -> [Categoria] -> [Categoria]
crearCategoria nombre categorias =
    let nuevoId = if null categorias then 1 else maximum (map idCategoria categorias) + 1
        nuevaCategoria = Categoria nuevoId nombre
    in agregarCategoria nuevaCategoria categorias

-- Actualiza el nombre de una categoría existente
actualizarCategoria :: Int -> String -> [Categoria] -> [Categoria]
actualizarCategoria idCategoria' nuevoNombre = map actualizarSiCoinc
  where
    actualizarSiCoinc cat
        | idCategoria cat == idCategoria' = cat { nombreCategoria = nuevoNombre }
        | otherwise = cat

-- Verifica si una categoría puede ser eliminada (no está siendo usada)
puedeEliminarCategoria :: Int -> [RegistroFinanciero] -> Bool
puedeEliminarCategoria idCat registros = not (any (\r -> idCategoriaRegistro r == idCat) registros)

-- Elimina una categoría si no está siendo usada por registros
eliminarCategoria :: Int -> [Categoria] -> [RegistroFinanciero] -> [Categoria]
eliminarCategoria idCat categorias registros
    | puedeEliminarCategoria idCat registros = filter (\c -> idCategoria c /= idCat) categorias
    | otherwise = categorias