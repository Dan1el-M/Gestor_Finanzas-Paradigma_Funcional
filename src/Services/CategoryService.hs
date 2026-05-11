{-- Este archivo contiene las funciones principales que se
utilizan en el menu de categorias, para esta misma se edita en
caliente el documento de categorias.txt
--} 
module Services.CategoryService
    ( crearCategoriaService
    , buscarCategoriaPorId
    , buscarCategoriaPorNombre
    , actualizarCategoriaService
    , eliminarCategoriaService
    , puedeEliminarCategoria
    ) where

import Data.Char (isSpace, toLower)

import FileManager (guardarCategorias)
import Models

-- Servicio con la logica de negocio para categorias.

crearCategoriaService :: String -> [Categoria] -> IO (Either String [Categoria])
crearCategoriaService nombre categorias
    | nombreLimpio == "" =
        return (Left "El nombre de la categoria no puede estar vacio.")
    | existeNombreCategoria nombreLimpio categorias =
        return (Left "Ya existe una categoria con ese nombre.")
    | otherwise = do
        let nuevasCategorias = categorias ++ [Categoria (siguienteIdCategoria categorias) nombreLimpio]
        guardarCategorias nuevasCategorias
        return (Right nuevasCategorias)
  where
    nombreLimpio = limpiarTexto nombre

buscarCategoriaPorId :: Int -> [Categoria] -> Maybe Categoria
buscarCategoriaPorId _ [] = Nothing
buscarCategoriaPorId idBuscado (categoria:resto)
    | idCategoria categoria == idBuscado = Just categoria
    | otherwise = buscarCategoriaPorId idBuscado resto

buscarCategoriaPorNombre :: String -> [Categoria] -> Maybe Categoria
buscarCategoriaPorNombre nombre categorias =
    buscarPorNombreNormalizado (normalizarNombre nombre) categorias

actualizarCategoriaService :: Int -> String -> [Categoria] -> IO (Either String [Categoria])
actualizarCategoriaService idBuscado nuevoNombre categorias
    | nombreLimpio == "" =
        return (Left "El nombre de la categoria no puede estar vacio.")
    | buscarCategoriaPorId idBuscado categorias == Nothing =
        return (Left "No existe una categoria con ese ID.")
    | existeNombreEnOtraCategoria idBuscado nombreLimpio categorias =
        return (Left "Ya existe otra categoria con ese nombre.")
    | otherwise = do
        let categoriasActualizadas = map actualizarSiCoincide categorias
        guardarCategorias categoriasActualizadas
        return (Right categoriasActualizadas)
  where
    nombreLimpio = limpiarTexto nuevoNombre

    actualizarSiCoincide categoria
        | idCategoria categoria == idBuscado =
            categoria { nombreCategoria = nombreLimpio }
        | otherwise = categoria

eliminarCategoriaService :: Int -> [Categoria] -> [RegistroFinanciero] -> IO (Either String [Categoria])
eliminarCategoriaService idBuscado categorias registros =
    case buscarCategoriaPorId idBuscado categorias of
        Nothing ->
            return (Left "No existe una categoria con ese ID.")

        Just _ ->
            if puedeEliminarCategoria idBuscado registros
                then do
                    let categoriasActualizadas = filtrarCategoria idBuscado categorias
                    guardarCategorias categoriasActualizadas
                    return (Right categoriasActualizadas)
                else
                    return (Left "No se puede eliminar la categoria porque esta siendo usada por registros financieros.")

puedeEliminarCategoria :: Int -> [RegistroFinanciero] -> Bool
puedeEliminarCategoria _ [] = True
puedeEliminarCategoria idBuscado (registro:resto)
    | idCategoriaRegistro registro == idBuscado = False
    | otherwise = puedeEliminarCategoria idBuscado resto

siguienteIdCategoria :: [Categoria] -> Int
siguienteIdCategoria [] = 1
siguienteIdCategoria categorias = maximum (map idCategoria categorias) + 1

filtrarCategoria :: Int -> [Categoria] -> [Categoria]
filtrarCategoria _ [] = []
filtrarCategoria idBuscado (categoria:resto)
    | idCategoria categoria == idBuscado = filtrarCategoria idBuscado resto
    | otherwise = categoria : filtrarCategoria idBuscado resto

existeNombreCategoria :: String -> [Categoria] -> Bool
existeNombreCategoria nombre categorias =
    buscarCategoriaPorNombre nombre categorias /= Nothing

existeNombreEnOtraCategoria :: Int -> String -> [Categoria] -> Bool
existeNombreEnOtraCategoria _ _ [] = False
existeNombreEnOtraCategoria idBuscado nombre (categoria:resto)
    | idCategoria categoria /= idBuscado
        && normalizarNombre (nombreCategoria categoria) == normalizarNombre nombre = True
    | otherwise = existeNombreEnOtraCategoria idBuscado nombre resto

buscarPorNombreNormalizado :: String -> [Categoria] -> Maybe Categoria
buscarPorNombreNormalizado _ [] = Nothing
buscarPorNombreNormalizado nombreBuscado (categoria:resto)
    | normalizarNombre (nombreCategoria categoria) == nombreBuscado = Just categoria
    | otherwise = buscarPorNombreNormalizado nombreBuscado resto

limpiarTexto :: String -> String
limpiarTexto = quitarEspaciosFinales . quitarEspaciosIniciales

quitarEspaciosIniciales :: String -> String
quitarEspaciosIniciales = dropWhile isSpace

quitarEspaciosFinales :: String -> String
quitarEspaciosFinales = reverse . quitarEspaciosIniciales . reverse

normalizarNombre :: String -> String
normalizarNombre = map toLower . limpiarTexto
