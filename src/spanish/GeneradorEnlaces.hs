module GeneradorEnlaces (
    enlacesAleatoriosPorIdioma,
    enlacesAleatorios
    )where

import Prelude hiding (id)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import GHC.Generics

data ArticuloAleatorio = ArticuloAleatorio
    { id    :: Int
    } deriving (Show, Generic) 

data BusquedaAleatoria = BusquedaAleatoria
    { random :: [ArticuloAleatorio]
    } deriving (Show, Generic)

data RespuestaApi = RespuestaApi
    { query :: BusquedaAleatoria
    } deriving (Show, Generic)

instance FromJSON ArticuloAleatorio
instance FromJSON BusquedaAleatoria
instance FromJSON RespuestaApi

enlacesAleatoriosPorIdioma :: Int -> String -> IO [(String,String)]
enlacesAleatoriosPorIdioma nArticulos idioma = do
    manager <- newManager tlsManagerSettings
    let url = "https://" ++ idioma ++ ".wikipedia.org/w/api.php?action=query"++
              "&format=json&list=random&rnnamespace=0&rnlimit="++(show nArticulos)
    req <- parseRequest url
    res <- httpLbs req manager
    let maybeResponse = decode (responseBody res) :: Maybe RespuestaApi
    return $ case maybeResponse of
        Just respuestaApi ->
            fmap (\page -> (idioma,show (id page))) (random (query respuestaApi))
        Nothing -> []
           
enlacesAleatorios :: Int -> [String] -> IO [(String,String)]
enlacesAleatorios nArticulos idiomas = 
   if nArticulos > 500 
   then do resto <- enlacesAleatorios (nArticulos-500) idiomas
           resultadoAux <- mapM (\x -> enlacesAleatoriosPorIdioma 500 x) idiomas
           return ((concat resultadoAux) ++ resto)
   else do resul <- mapM (\x -> enlacesAleatoriosPorIdioma nArticulos x) idiomas
           return (concat resul)
