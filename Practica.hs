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


