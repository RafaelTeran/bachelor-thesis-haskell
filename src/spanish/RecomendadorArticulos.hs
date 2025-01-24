module RecomendadorArticulos where

import GeneradorBaseDatos
import Recomendador
import Data.List as L
import Data.HashMap.Strict as HM
import Database.HDBC.Sqlite3
import Database.HDBC
  
introsArticulos :: Int -> String -> IO ()
introsArticulos nIntros idioma = do
   baseDatosAleatoria "introsArticulos.sql" "tablaArticulos" nIntros [idioma]
   
insertarArticulo :: (String,String) -> IO ()
insertarArticulo articulo = do
   insertarTextoEnBaseDatosArticulo articulo "introsArticulos.sql"
                                                  "tablaArticulos"

introsTitulos :: IO [(String,String)]
introsTitulos = do
   baseDatos <- buscadorBaseDatos 
               "introsArticulos.sql" "SELECT titulo, texto FROM tablaArticulos"
   let intros = zip (extraerColumna baseDatos 0) (extraerColumna baseDatos 1)
   return $ HM.keys $ frecuencia intros

palabrasVacias :: [String]
palabrasVacias = ["algún", "alguna", "algunas", "alguno", "algunos", "ambos",
             "empleamos", "ante", "antes", "aquel", "aquellas", "aquellos",
             "aquí", "arriba", "atrás", "bajo", "bastante", "bien", "cada",
             "cierta", "ciertas", "cierto", "ciertos", "como", "con",
             "conseguimos", "conseguir", "consigo", "consigue", "consiguen",
             "consigues", "cual", "cuando", "dentro", "desde", "donde", "dos",
             "el", "ellas", "ellos", "empleáis", "emplean", "emplear",
             "empleas", "empleo", "en", "encima", "entonces", "entre", "era",
             "eramos", "eran", "eras", "eres", "es", "esta", "estaba", "estado",
             "estáis", "estamos", "están", "estoy", "fin", "fue", "fueron",
             "fui", "fuimos", "bueno", "ha", "hace", "hacéis", "hacemos",
             "hacen", "hacer", "haces", "hago", "incluso", "intenta",
             "intentáis", "intentamos", "intentan", "intentar", "intentas",
             "intento", "ir", "la", "largo", "las", "lo", "los", "mientras",
             "mio", "modo", "muchos", "muy", "nos", "nosotros", "otro", "para",
             "pero", "podéis", "podemos", "poder", "podría", "podríais",
             "podríamos", "podrían", "podrías", "por", "por qué", "porque",
             "primero", "puede", "pueden", "puedo", "quien", "sabe", "sabéis",
             "sabemos", "saben", "saber", "sabes", "ser", "si", "siendo", "sin",
             "sobre", "sois", "solamente", "solo", "somos", "soy", "su", "sus",
             "también", "tenéis", "tenemos", "tener", "tengo", "tiempo",
             "tiene", "tienen", "todo", "trabaja", "trabajáis", "trabajamos",
             "trabajan", "trabajar", "trabajas", "trabajo", "tras", "tuyo",
             "ultimo", "un", "una", "unas", "uno", "unos", "usa", "usáis",
             "usamos", "usan", "usar", "usas", "uso", "va", "vais", "valor",
             "vamos", "van", "vaya", "verdad", "verdadera", "verdadero",
             "vosotras", "vosotros", "voy", "yo", "él", "ésta", "éstas", "éste",
             "éstos", "última", "últimas", "último", "últimos", "a", "añadió",
             "aún", "actualmente", "adelante", "además", "afirmó", "agregó",
             "ahí", "ahora", "al", "algo", "alrededor", "anterior", "apenas",
             "aproximadamente", "aquí", "así", "aseguró", "aunque", "ayer",
             "buen", "buena", "buenas", "bueno", "buenos", "cómo", "casi",
             "cerca", "cinco", "comentó", "conocer", "consideró", "considera",
             "contra", "cosas", "creo", "cuales", "cualquier", "cuanto",
             "cuatro", "cuenta", "da", "dado", "dan", "dar", "de", "debe",
             "deben", "debido", "decir", "dejó", "del", "demás", "después",
             "dice", "dicen", "dicho", "dieron", "diferente", "diferentes",
             "dijeron", "dijo", "dio", "durante", "e", "ejemplo", "ella",
             "ello", "embargo", "encuentra", "esa", "esas", "ese", "eso",
             "esos", "está", "están", "estaban", "estar", "estará", "estas",
             "este", "esto", "estos", "estuvo", "ex", "existe", "existen",
             "explicó", "expresó", "fuera", "gran", "grandes", "había",
             "habían", "haber", "habrá", "hacerlo", "hacia", "haciendo", "han",
             "hasta", "hay", "haya", "he", "hecho", "hemos", "hicieron", "hizo",
             "hoy", "hubo", "igual", "indicó", "informó", "junto", "lado", "le",
             "les", "llegó", "lleva", "llevar", "luego", "lugar", "más",
             "manera", "manifestó", "mayor", "me", "mediante", "mejor",
             "mencionó", "menos", "mi", "misma", "mismas", "mismo", "mismos",
             "momento", "mucha", "muchas", "mucho", "nada", "nadie", "ni",
             "ningún", "ninguna", "ningunas", "ninguno", "ningunos", "no",
             "nosotras", "nuestra", "nuestras", "nuestro", "nuestros", "nueva",
             "nuevas", "nuevo", "nuevos", "nunca", "o", "ocho", "otra", "otras",
             "otros", "parece", "parte", "partir", "pasada", "pasado", "pesar",
             "poca", "pocas", "poco", "pocos", "podrá", "podrán", "podría",
             "podrían", "poner", "posible", "próximo", "próximos", "primer",
             "primera", "primeros", "principalmente", "propia", "propias",
             "propio", "propios", "pudo", "pueda", "pues", "qué", "que",
             "quedó", "queremos", "quién", "quienes", "quiere", "realizó",
             "realizado", "realizar", "respecto", "sí", "sólo", "se", "señaló",
             "sea", "sean", "según", "segunda", "segundo", "seis", "será",
             "serán", "sería", "sido", "siempre", "siete", "sigue", "siguiente",
             "sino", "sola", "solas", "solos", "son", "tal", "tampoco", "tan",
             "tanto", "tenía", "tendrá", "tendrán", "tenga", "tenido",
             "tercera", "toda", "todas", "todavía", "todos", "total", "trata",
             "través", "tres", "tuvo", "usted", "varias", "varios", "veces",
             "ver", "vez", "y", "ya", "the", "enero", "febrero", "marzo",
             "abril", "mayo", "junio", "julio", "agosto", "septiembre",
             "octubre", "noviembre", "diciembre", "año", "años", "of", "m",
             "km", "mundo", "mundial", "conocido", "conocida",
             "perteneciente", "ubicada", "ubicado","forma", "nombre"]

recomendadorArticulos :: String -> Int -> IO [String]
recomendadorArticulos titulo n = do
  insertarArticulo ("es",titulo)
  datos <- introsTitulos
  return $ recomendador datos palabrasVacias titulo n 50 0.9
