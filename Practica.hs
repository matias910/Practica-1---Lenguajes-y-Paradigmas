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


--Funcion para mostrar la informacion del Libro como texto--
mostrarLibro :: Libro -> String
mostrarLibro libro =
    show (codigo libro) ++ " - " ++ titulo libro ++ " - " ++ autor libro ++ " - " ++ categoria libro ++ " - " ++ estado libro ++ " - " ++ show (entrada libro) ++ " - " ++ show (salida libro)


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


--Funcion para guardar la informacion en archivo txt --
guardarLibreria :: [Libro] -> IO ()
guardarLibreria libreria = do
    resultado <- try (writeFile "libreria.txt" (unlines (map show libreria))) :: IO (Either IOException ())
    case resultado of
        Left ex -> putStrLn $ "Error guardando la libreria " ++ show ex
        Right _ -> putStrLn "Libro guardado en el archivo libreria.txt."


--Funcion para cargar archivos desde un txt--
cargarLibreria :: IO [Libro]
cargarLibreria = do
    resultado <- try (readFile "libreria.txt") :: IO (Either IOException String)
    case resultado of
        Left ex -> do
            putStrLn $ "Error cargando el archivo" ++ show ex
            return []
        Right contenido -> do
            let lineas = lines contenido
            return (map leerLibro lineas)
    where
        leerLibro linea = read linea :: Libro



--Funcion Ciclo del Programa (Menu) --
cicloPrincipal :: [Libro] -> IO ()
cicloPrincipal libreria = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de libro"
    putStrLn "2. Registrar prestamo de libro"
    putStrLn "3. Buscar libro por filtros"
    putStrLn "4. Listar los libros de la biblioteca"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1"  -> do
            putStrLn "Ingresa el codigo del libro"
            codigoStr <- getLine
            let codigoLibro = read codigoStr :: Int
            putStrLn "Ingresa el titulo"
            tituloLibro <- getLine
            putStrLn "Ingresa el autor"
            autorLibro <- getLine
            putStrLn "Ingresa la categoria"
            categoriaLibro <- getLine
            let estadoLibro = "Disponible"
            tiempo <- getCurrentTime

            let libreriaActualizada = registrarEntradaLibro codigoLibro tituloLibro autorLibro categoriaLibro estadoLibro tiempo libreria
            putStrLn $ "El libro: " ++ tituloLibro ++ " con codigo: " ++ show codigoLibro ++ " ha ingresado a la biblioteca"
            guardarLibreria libreriaActualizada
            cicloPrincipal libreriaActualizada

        "2" -> do
            putStrLn "Ingresar el codigo del libro que vas a llevar: "
            codigoStr <- getLine
            let codigoLibro = read codigoStr :: Int
            tiempoActual <- getCurrentTime
            let libreriaActualizada = registrarSalidaLibro codigoLibro tiempoActual libreria
            putStrLn $ "El libro con codigo: " ++ show codigoLibro ++ " ha sido retirado para prestamo exitosamente"
            guardarLibreria libreriaActualizada
            cicloPrincipal libreriaActualizada




main :: IO ()
main = do
    libreria <- cargarLibreria
    putStrLn "¡Bienvenido al Sistema de Prestamos de libros de nuestra biblioteca!!"

    cicloPrincipal libreria
