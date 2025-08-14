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
    entrada :: UTCTime,
    salida :: Maybe UTCTime
}deriving (Show, Read)


registrarEntradaLibro :: Int -> String -> String -> String -> UTCTime -> [Libro] -> [Libro]
registrarEntradaLibro codigoLibro tituloLibro autorLibro categoriaLibro tiempo libreria =
    Libro codigoLibro tituloLibro autorLibro categoriaLibro tiempo Nothing : libreria



registrarSalidaLibro :: Int -> UTCTime -> [Libro] -> [Libro]
registrarSalidaLibro codigoLibro tiempo libreria =
    map (\v -> if codigoLibro == codigo v then v{salida = Just tiempo} else v) libreria



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
