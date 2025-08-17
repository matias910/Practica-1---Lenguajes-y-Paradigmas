import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Maybe (mapMaybe)
import Control.DeepSeq (deepseq) -- Para forzar la evaluación completa de los datos --


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
registrarEntradaLibro :: Int -> String -> String -> String -> UTCTime -> [Libro] -> [Libro]
registrarEntradaLibro codigoLibro tituloLibro autorLibro categoriaLibro tiempo libreria =
    Libro codigoLibro tituloLibro autorLibro categoriaLibro "Disponible" tiempo Nothing : libreria

--Funcion Prestar Libro--
registrarSalidaLibro :: Int -> UTCTime -> [Libro] -> [Libro]
registrarSalidaLibro codigoLibro tiempo libreria =
    map (\v -> if codigoLibro == codigo v
               then v { salida = Just tiempo, estado = "Prestado"}
               else v) libreria


--Funcion devolver Libro--
devolverLibro :: Int -> [Libro] -> IO [Libro]
devolverLibro codigoBuscado libreria = do
    tiempoActual <- getCurrentTime
    let libreriaActualizada = map (\v ->
            if codigo v == codigoBuscado
            then v { salida = Just tiempoActual, estado = "Disponible" }
            else v)
            libreria
    writeFile "libreria.txt" (unlines (map show libreriaActualizada))
    return libreriaActualizada

--Formatear informacion de la salida del libro--
formatearSalidaLibro :: Maybe UTCTime -> String
formatearSalidaLibro Nothing = "El libro sigue en la biblioteca"
formatearSalidaLibro (Just tiempo) = "El libro fue retirado el: " ++ show tiempo


--Funcion para mostrar la informacion del Libro como texto--
mostrarLibro :: Libro -> String
mostrarLibro libro =
    show (codigo libro) ++ " - " ++ titulo libro ++ " - " ++ autor libro ++ " - " ++ categoria libro ++ " - " ++ estado libro ++ " - " ++ show (entrada libro) ++ " - " ++ formatearSalidaLibro (salida libro)


--Funcion Buscar Libro por Codigo--
buscarLibroCodigo :: Int -> [Libro] -> UTCTime -> String
buscarLibroCodigo codigoLibro libreria tiempoActual =
    let resultado = filter (\v -> codigoLibro == codigo v) libreria
    in if null resultado
       then "No hay ningun libro con este codigo"
       else unlines (map (\libro ->
                        "El libro con codigo: " ++ show (codigo libro) ++ " se encuentra en la biblioteca: " ++
                        "\n Titulo: " ++ titulo libro ++
                        "\n Autor: " ++ autor libro ++
                        "\n Categoria: " ++ categoria libro ++
                        "\n Estado: " ++ estado libro ++
                        "\n Fecha de entrada: " ++ show (entrada libro) ++
                        "\n Tiempo en la libreria: " ++ show (tiempoLibreria libro tiempoActual) ++ " segundos") resultado)



--Funcion Buscar Libro por Titulo--
buscarLibroTitulo :: String -> [Libro] -> UTCTime -> String
buscarLibroTitulo tituloLibro libreria tiempoActual =
    let resultado = filter (\v -> tituloLibro == titulo v) libreria
    in if null resultado
       then "No hay ningun libro con este titulo"
       else unlines (map (\libro ->
                        "El libro con titulo: " ++ titulo libro ++ " se encuentra en la biblioteca: " ++
                        "\n Codigo: " ++ show (codigo libro) ++
                        "\n Autor: " ++ autor libro ++
                        "\n Categoria: " ++ categoria libro ++
                        "\n Estado: " ++ estado libro ++
                        "\n Fecha de entrada: " ++ show (entrada libro) ++
                        "\n Tiempo en la libreria: " ++ show (tiempoLibreria libro tiempoActual) ++ " segundos") resultado)

--Funcion Buscar Libro por Autor--
buscarLibroAutor :: String -> [Libro] -> UTCTime -> String  --Filer devuelve una lista--
buscarLibroAutor autorLibro libreria tiempoActual =
    let resultado = filter (\v -> autorLibro == autor v) libreria
    in if null resultado
       then "No hay libros de este autor en la biblioteca."
       else unlines (map (\libro ->
                        "El libro del autor: " ++ autor libro ++ " se encuentra en la biblioteca: " ++
                        "\n Codigo: " ++ show (codigo libro) ++
                        "\n Titulo: " ++ titulo libro ++
                        "\n Categoria: " ++ categoria libro ++
                        "\n Estado: " ++ estado libro ++
                        "\n Fecha de entrada: " ++ show (entrada libro) ++
                        "\n Tiempo en la libreria: " ++ show (tiempoLibreria libro tiempoActual) ++ " segundos") resultado)

--Funcion Buscar Libro por Categoria--
buscarLibroCategoria :: String -> [Libro] -> UTCTime -> String
buscarLibroCategoria categoriaLibro libreria tiempoActual =
    let resultado = filter (\v -> categoriaLibro == categoria v) libreria
    in if null resultado
       then "No hay libros de esta categoria"
       else unlines (map (\libro ->
                        "El libro de la categoria: " ++ categoria libro ++ " se encuentra en la biblioteca: " ++
                        "\n Codigo: " ++ show (codigo libro) ++
                        "\n Titulo: " ++ titulo libro ++
                        "\n Autor: " ++ autor libro ++
                        "\n Estado: " ++ estado libro ++
                        "\n Fecha de entrada: " ++ show (entrada libro) ++
                        "\n Tiempo en la libreria: " ++ show (tiempoLibreria libro tiempoActual) ++ " segundos") resultado)


--Funcion Buscar Libro por Estado--
buscarLibroEstado :: String -> [Libro] -> UTCTime -> String
buscarLibroEstado estadoLibro libreria tiempoActual =
    let resultado = filter (\v -> estadoLibro == estado v) libreria
    in if null resultado
       then "No hay libros con este estado"
       else unlines (map (\libro ->
                        "El libro con estado: " ++ estado libro ++ " se encuentra en la biblioteca: " ++
                        "\n Codigo: " ++ show (codigo libro) ++
                        "\n Titulo: " ++ titulo libro ++
                        "\n Autor: " ++ autor libro ++
                        "\n Categoria: " ++ categoria libro ++
                        "\n Fecha de entrada: " ++ show (entrada libro) ++
                        "\n Tiempo en la libreria: " ++ show (tiempoLibreria libro tiempoActual) ++ " segundos") resultado)



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
    resultado <- reintentar 5 (writeFile "libreria.txt" (unlines (map show libreria))) :: IO (Either IOException ())
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
            contenido `deepseq` return (map read (lines contenido)) --deepseq es para que se fuerce la evaluacion completa del archivo, sin esta el txt se mantiene abierto en segundo plano y no dejaria agregar mas libros--


--Funcion Ciclo del Programa (Menu) --
cicloPrincipal :: [Libro] -> IO ()
cicloPrincipal libreria = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada / devolucion de libro"
    putStrLn "2. Registrar prestamo de libro"
    putStrLn "3. Devolver libro"
    putStrLn "4. Buscar libro por filtros"
    putStrLn "5. Listar los libros de la biblioteca"
    putStrLn "6. Salir"

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

            let libreriaActualizada = registrarEntradaLibro codigoLibro tituloLibro autorLibro categoriaLibro tiempo libreria
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

        "3" -> do
            putStrLn "Ingrese el codigo del libro que desea devolver: "
            codigoStr <- getLine
            let codigoLibro = read codigoStr :: Int
            libreriaActualizada <- devolverLibro codigoLibro libreria
            putStrLn $ "El libro con codigo: " ++ show codigoLibro ++ " ha sido devuelto exitosamente"
            cicloPrincipal libreriaActualizada

        "4" -> do
            putStrLn "\nSeleccione una opción:"
            putStrLn "1. Buscar libro por codigo"
            putStrLn "2. Buscar libro por titulo"
            putStrLn "3. Buscar libro por autor"
            putStrLn "4. Buscar libro por categoria"
            putStrLn "5. Buscar libro por estado"

            opcionBusqueda <- getLine
            case opcionBusqueda of
                "1" -> do
                    putStrLn "Ingrese el codigo del libro"
                    codigoStr <- getLine
                    let codigoLibro = read codigoStr :: Int
                    tiempoActual <- getCurrentTime
                    putStrLn (buscarLibroCodigo codigoLibro libreria tiempoActual)
                    cicloPrincipal libreria


                "2" -> do
                    putStrLn "Ingrese el titulo del libro"
                    tituloLibro <- getLine
                    tiempoActual <- getCurrentTime
                    putStrLn (buscarLibroTitulo tituloLibro libreria tiempoActual)
                    cicloPrincipal libreria

                "3" -> do
                    putStrLn "Ingrese el autor del libro"
                    autorLibro <- getLine
                    tiempoActual <- getCurrentTime
                    putStrLn (buscarLibroAutor autorLibro libreria tiempoActual)
                    cicloPrincipal libreria

                "4" -> do
                    putStrLn "Ingrese la categoria"
                    categoriaLibro <- getLine
                    tiempoActual <- getCurrentTime
                    putStrLn (buscarLibroCategoria categoriaLibro libreria tiempoActual)
                    cicloPrincipal libreria


                "5" -> do
                    putStrLn "\nSeleccione una opción:"
                    putStrLn "1. Disponible"
                    putStrLn "2. Prestado"

                    tiempoActual <- getCurrentTime

                    opcionEstado <- getLine
                    case opcionEstado of
                        "1" -> do
                            let estadoLibro = "Disponible"
                            putStrLn (buscarLibroEstado estadoLibro libreria tiempoActual)
                            cicloPrincipal libreria
                        "2" -> do
                            let estadoLibro = "Prestado"
                            putStrLn (buscarLibroEstado estadoLibro libreria tiempoActual)
                            cicloPrincipal libreria
                        _ -> do
                            putStrLn "Opcion no valida, intente de nuevo"
                            cicloPrincipal libreria
                _ -> do
                    putStrLn "Opcion no valida, intente de nuevo"
                    cicloPrincipal libreria


        "5" -> do
            putStrLn "Mostrando libros en la biblioteca: "
            --Libreria Actualizada--
            libreriaActualizada <- cargarLibreria
            mapM_ (\v -> putStrLn $ mostrarLibro v) libreriaActualizada
            cicloPrincipal libreriaActualizada


        "6" -> putStrLn "¡Hasta luego, Gracias por usar nuestra aplicacion!"

        _ -> do
            putStrLn "Opcion no valida, intente de nuevo"
            cicloPrincipal libreria



main :: IO ()
main = do
    libreria <- cargarLibreria
    putStrLn "¡Bienvenido al Sistema de Prestamos de libros de nuestra biblioteca!!"

    cicloPrincipal libreria
