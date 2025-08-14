import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Maybe (mapMaybe)

data Libro = Libro {
    codigo :: Int,
    titulo :: String,
    autor :: String,
    categoria :: String,
    estado :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime
}deriving (Show, Read)

--Funcion Registrar Libro--
registrarEntradaLibro :: Int -> String -> String -> String -> String -> UTCTime -> [Libro] -> [Libro]
registrarEntradaLibro codigoLibro tituloLibro autorLibro categoriaLibro estadoLibro tiempo libreria =
    Libro codigoLibro tituloLibro autorLibro categoriaLibro "Disponible" tiempo Nothing : libreria

--Funcion Prestar Libro--
registrarSalidaLibro :: Int -> UTCTime -> [Libro] -> [Libro]
registrarSalidaLibro codigoLibro tiempo libreria =
    map (\v -> if codigoLibro == codigo v
               then v {salida = Just tiempo, estado = "Prestado"}
               else v) libreria


--Funcion Buscar Libro por Codigo--
buscarLibroCodigo :: Int -> [Libro] -> String
buscarLibroCodigo codigoLibro libreria =
    let resultado = filter (\v -> codigoLibro == codigo v) libreria
    in if null resultado
       then "No hay ningun libro con este codigo"
       else unlines (map mostrarLibro resultado)

--Funcion Buscar Libro por Titulo--
buscarLibroTitulo :: String -> [Libro] -> String
buscarLibroTitulo tituloLibro libreria =
    let resultado = filter (\v -> tituloLibro == titulo v) libreria
    in if null resultado
       then "No hay ningun libro con este titulo"
       else unlines (map mostrarLibro resultado)

--Funcion Buscar Libro por Autor--
buscarLibroAutor :: String -> [Libro] -> String--Filer devuelve una lista--
buscarLibroAutor autorLibro libreria =
    let resultado = filter (\v -> autorLibro == autor v) libreria
    in if null resultado
       then "No hay libros de este autor en la biblioteca."
       else unlines (map mostrarLibro resultado)

--Funcion Buscar Libro por Categoria--
buscarLibroCategoria :: String -> [Libro] -> String
buscarLibroCategoria categoriaLibro libreria =
    let resultado = filter (\v -> categoriaLibro == categoria v) libreria
    in if null resultado
       then "No hay libros de esta categoria"
       else unlines (map mostrarLibro resultado)

--Funcion Buscar Libro por Estado--
buscarLibroEstado :: String -> [Libro] -> String
buscarLibroEstado estadoLibro libreria =
    let resultado = filter (\v -> estadoLibro == estado v) libreria
    in if null resultado
       then "No hay libros con este estado"
       else unlines (map mostrarLibro resultado)




guardarArchivo :: [Libro] -> IO ()
guardarArchivo libreria = do
    resultado <- reintentar 5 (writeFile "libreria.txt" (unlines (map mostrarLibro libreria)))
    case resultado of
        Left ex -> putStrLn ("Error guardando el archivo " ++ show  ex)
        Right _ -> putStrLn ("Registro del libro guardado en el archivo libreria.txt")


cicloPrincipal :: [Libro] -> IO ()
cicloPrincipal libreria = do
    putStrLn "Seleccione una opción:"
    putStrLn "1. Registrar entrada de libro"
    putStrLn "2. Registrar salida de libro"
    putStrLn "3. Buscar libro por codigo"
    putStrLn "4. Listar los libros de la biblioteca"
    putStrLn "5. Salir"

main :: IO ()
main = do
    putStrLn "¡Bienvenido al Sistema de Prestamos de libros de nuestra biblioteca!!"
