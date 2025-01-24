module ClasificadorIdioma where

import GeneradorBaseDatos
import Clasificador
import Data.List as L
import Data.Hashable
import Data.HashMap.Strict as HM
import Data.Ord
import Database.HDBC.Sqlite3
import Database.HDBC
import Data.Char

frecUsuariosPorIdioma :: HashMap String Double
frecUsuariosPorIdioma = HM.fromList [("en",115659),("de",16787),("fr",15990),
   ("es",13430),("ja",12651),("ru",8752),("pt",8256),("it",7203),("zh",7153),
   ("fa",5071),("pl",4253),("ar",3766),("nl",3425),("he",3276),("uk",2975),
   ("tr",2581),("id",2567),("cs",1994),("ko",1978),("sv",1955),("vi",1575),
   ("fi",1535),("hu",1407),("th",1341),("bn",1335),("no",1079),("ca",967),
   ("hi",959),("el",862),("ro",784)]

introsIdiomas :: Int -> IO ()
introsIdiomas nIntros = do
   baseDatosAleatoria "introsIdiomas.sql" "tablaIdiomas" nIntros idiomas
   where idiomas = HM.keys frecUsuariosPorIdioma
   
intros :: IO [(String,String)]
intros = do
   baseDatos <- buscadorBaseDatos 
               "introsIdiomas.sql" "SELECT idioma, texto FROM tablaIdiomas"
   let introsAux = zip (extraerColumna baseDatos 0) (extraerColumna baseDatos 1)
   return $ HM.keys $ frecuencia introsAux
   
clasificadorIdioma :: String -> IO String
clasificadorIdioma texto = do
     datos <- intros
     let clases = HM.keys $ (frecuencia . L.map fst) datos
         nPalabras = fromInteger $ sum $ HM.elems
                     ((frecuencia . concatMap words) (L.map snd datos))
         nExtractos = sum $ HM.elems frecUsuariosPorIdioma
         probs = clasificadorClases texto datos frecUsuariosPorIdioma 
                                            clases nPalabras nExtractos
     return $ fst (L.maximumBy (comparing snd) probs)

clasificadorIdiomaTodos :: String -> IO [(String, Double)]
clasificadorIdiomaTodos texto = do
   datos <- intros
   let clases = HM.keys $ (frecuencia . L.map fst) datos
       nPalabras = fromInteger $ sum $ HM.elems
                   ((frecuencia . concatMap words) (L.map snd datos))
       nExtractos = sum $ HM.elems frecUsuariosPorIdioma
       probs = clasificadorClases (L.map toLower texto) datos 
                        frecUsuariosPorIdioma clases nPalabras nExtractos
   return (L.sortBy (comparing (Down . snd)) probs)
