module GeneradorTextos (
    Wiki,
    idioma,
    titulo,
    texto,
    textosAleatorios,
    extractorIntroTitulo
    )where

import GeneradorEnlaces
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Char

data RespuestaApi = RespuestaApi
     { query :: Busqueda
     } deriving (Show, Generic)
     
data Busqueda = Busqueda
     { pages :: HM.HashMap T.Text Articulo
     } deriving (Show, Generic)
     
data Articulo = Articulo
     { title :: !T.Text,
       extract :: !T.Text
     } deriving (Show, Generic)
          
data Wiki = Wiki
     { idioma :: !T.Text,
       titulo :: !T.Text,
       texto  :: !T.Text
     } deriving (Show,Generic)          
          
instance FromJSON RespuestaApi
instance FromJSON Busqueda
instance FromJSON Articulo

extractorIntro :: (String,String) -> IO (Either String Wiki)
extractorIntro (idioma, articuloID) = do
    manager <- newManager tlsManagerSettings
    let url = "https://" ++ idioma ++ 
              ".wikipedia.org/w/api.php?action=query&prop=info|extracts"
           ++ "&explaintext&exintro&format=json&pageids=" ++ articuloID
    req <- parseRequest url
    res <- httpLbs req manager
    let maybeRespuestaApi = decode (responseBody res) :: Maybe RespuestaApi
    return $ case maybeRespuestaApi of
        Just respuestaApi ->
            let articulo = Prelude.head $ HM.elems $ pages $ query respuestaApi
                texto = extract articulo
                textoLimpio = limpiadorIntroduccion texto
                titulo = title articulo
            in Right (Wiki (T.pack idioma) titulo textoLimpio)
        Nothing -> Left $ ("Texto no encontrado")

extractorIntroTitulo :: (String,String) -> IO (Either String Wiki)
extractorIntroTitulo (idioma, titulo) = do
    manager <- newManager tlsManagerSettings
    let url = "https://" ++ idioma ++ 
              ".wikipedia.org/w/api.php?action=query&prop=info|extracts"
           ++ "&explaintext&exintro&format=json&titles=" ++ titulo
    req <- parseRequest url
    res <- httpLbs req manager
    let maybeRespuestaApi = decode (responseBody res) :: Maybe RespuestaApi
    return $ case maybeRespuestaApi of
        Just respuestaApi ->
            let articulo = Prelude.head $ HM.elems $ pages $ query respuestaApi
                texto = extract articulo
                textoLimpio = limpiadorIntroduccion texto
                tituloAux = title articulo
            in Right (Wiki (T.pack idioma) tituloAux textoLimpio)
        Nothing -> Left $ ("Texto no encontrado")

limpiadorIntroduccion :: T.Text -> T.Text
limpiadorIntroduccion text =
     let introLimpia = T.replace (T.pack "\n")  (T.pack " ") text
     in T.pack (Prelude.map toLower (Prelude.filter (\c -> isAlpha c || c == ' ') 
                                                    (T.unpack ( introLimpia))))

textosAleatorios :: Int -> [String] -> IO [Either String Wiki]
textosAleatorios nArticulos idiomas = do
   aux <- enlacesAleatorios nArticulos idiomas
   mapM extractorIntro aux
