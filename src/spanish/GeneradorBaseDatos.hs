module GeneradorBaseDatos where

import GeneradorTextos
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List
import Data.Either

crearBaseDatos :: String -> String -> IO()
crearBaseDatos archivo nombreTabla = do
  conn <- connectSqlite3 archivo
  run conn createStatement []
  commit conn
  disconnect conn
  putStrLn "Base de datos creada con éxito."
  where createStatement = "CREATE TABLE " ++ nombreTabla ++ 
                          " (idioma TEXT, titulo TEXT, texto TEXT)"
  
insertarTextoEnBaseDatos :: Wiki -> String -> String -> IO()
insertarTextoEnBaseDatos wiki archivo nombreTabla = do
  conn <- connectSqlite3 archivo
  stmt <- prepare conn insertStatement
  execute stmt [toSql (idioma wiki), toSql (titulo wiki), toSql (texto wiki)]
  commit conn
  disconnect conn
  where insertStatement = "INSERT INTO " ++ nombreTabla ++ " VALUES (?, ?, ?)"

insertarTextoEnBaseDatosArticulo :: (String, String) -> String -> String -> IO()
insertarTextoEnBaseDatosArticulo articulo archivo nombreTabla = do
  conn <- connectSqlite3 archivo
  stmt <- prepare conn insertStatement
  maybeWiki <- extractorIntroTitulo articulo
  case maybeWiki of
      Left error -> putStrLn "Texto no encontrado"
      Right wiki -> do
        execute stmt [toSql (idioma wiki), toSql (titulo wiki), 
                                           toSql (texto wiki)]
        commit conn
        disconnect conn
        putStrLn "Insertado en la base de datos con éxito."
  where insertStatement = "INSERT INTO " ++ nombreTabla ++ " VALUES (?, ?, ?)"

baseDatosAleatoria :: String -> String -> Int -> [String] -> IO()
baseDatosAleatoria archivo nombreTabla nArticulos idiomas = do
  textos <- textosAleatorios nArticulos idiomas
  crearBaseDatos archivo nombreTabla
  mapM_ (\texto -> either putStrLn 
        (\x -> insertarTextoEnBaseDatos x archivo nombreTabla) texto) textos
  let exitos = length (rights textos)
  putStrLn $ "Se han insertado " ++ show exitos ++ 
             " elementos en la base de datos con éxito."
                                            
extraerColumna :: [[SqlValue]] -> Integer -> [String]
extraerColumna sql num =
  map (\col -> fromSql $ genericIndex col num :: String) sql

buscadorBaseDatos :: FilePath -> String -> IO [[SqlValue]]
buscadorBaseDatos archivo busqueda = do
  conn <- connectSqlite3 archivo
  res <- quickQuery' conn busqueda []
  disconnect conn
  return res
