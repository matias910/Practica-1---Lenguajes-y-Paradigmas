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


--Funcion para calcular el tiempo que estuvo el libro en la Libreria--
tiempoLibreria:: Libro -> UTCTime -> NominalDiffTime
tiempoLibreria libro tiempoActual =
    case salida libro of
        Just tiempoSalida -> diffUTCTime tiempoSalida (entrada libro)
        Nothing -> diffUTCTime tiempoActual (entrada libro)

--Funcion para guardar la informacion en un txt--
guardarLibro :: [Libro] -> IO ()
guardarLibro libreria = do
    resultado <- reintentar 5 (writeFile "libreria.txt" (unlines (map mostrarLibro libreria)))
    case resultado of
        Left ex -> putStrLn $ "Error guardando el libro: " ++ show ex
        Right _ ->  putStrLn "Libro guardado en el archivo libreria.txt"


--Funcion para reintentar en caso de errores--
reintentar :: Int -> IO a -> IO (Either IOException a)
reintentar 0 accion = catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
reintentar n accion = do
    resultado <- catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
    case resultado of
        Left ex -> do
            threadDelay 1000000  -- Esperar 1 segundo antes de reintentar
            reintentar (n - 1) accion
        Right val -> return (Right val)

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
