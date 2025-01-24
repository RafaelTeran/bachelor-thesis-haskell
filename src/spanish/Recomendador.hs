module Recomendador where

import Data.List as L
import Data.Hashable
import Data.HashMap.Strict as HM
import Data.Ord
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix as LA
import Data.Maybe

frecuencia :: (Eq k, Data.Hashable.Hashable k, Integral v) => [k] -> HashMap k v
frecuencia [] = HM.empty
frecuencia (x:xs) = HM.insertWith (+) x 1 (frecuencia xs)

palabrasClave :: [(String,String)] -> [String] -> Int -> [(String,Integer)]
palabrasClave datos palabrasVacias nClaves =
  let frecPalabras = frecuencia (concatMap words (L.map snd datos))
      palabrasNoVacias = L.filter (\(word, _) -> notElem word palabrasVacias) 
                                  (HM.toList frecPalabras)
  in take nClaves (sortBy (\(_,c1) (_,c2) -> compare c2 c1) palabrasNoVacias)

palabrasMatriz :: [(String,String)] -> [String] -> [String] -> [[Double]]
palabrasMatriz datos claves nombres =
  let frecPalabrasNombre = HM.fromList 
        (L.map (\(nombre,intro) -> (nombre, frecuencia $ words intro)) datos)
  in L.map (\nombre -> L.map 
           (\palabra -> frecPalabraPorNombre nombre palabra frecPalabrasNombre) 
           claves) nombres

frecPalabraPorNombre :: String -> String ->
                        HashMap String (HashMap String Integer) -> Double    
frecPalabraPorNombre nombre palabra frecuencias =
   fromIntegral $ HM.lookupDefault 0 palabra (frecuencias HM.! nombre)

eigDescomposicion :: [[Double]] -> (Vector Double, Matrix Double)
eigDescomposicion matriz =
    let vectores = fromLists matriz
        (_, covMatriz) = meanCov vectores
        (autovalores, autovectores) = eigSH covMatriz
    in (autovalores, autovectores)

nAutovectores :: Vector Double -> Double -> Int
nAutovectores autovalores porcentaje =
    let varianzaTotal = sumElements autovalores
        varianzaExplicada = LA.toList $ cmap (/varianzaTotal) autovalores
        varianzaAcumulada = scanl1 (+) varianzaExplicada
    in fromMaybe (1) (L.findIndex (> porcentaje) varianzaAcumulada)

analisisComponentesPrincipales :: Matrix Double -> [[Double]]
                               -> Int -> Matrix Double
analisisComponentesPrincipales autovectores matriz nAutoV =
  tr $ mul (tr autovectoresClave) (tr vectores)
  where
    vectores = fromLists matriz
    autovectoresClave = takeColumns nAutoV autovectores

distEuclidCuadrPorFila :: (Matrix Double) -> Int -> Vector Double
distEuclidCuadrPorFila matriz fila =
  sum $ toColumns difCuaradoMatriz
  where
    d = cols matriz
    matrizAux = repmat (subMatrix (fromIntegral fila, 0) (1, d) matriz) 
                       (rows matriz) 1
    difMatriz = matriz - matrizAux
    difCuaradoMatriz = difMatriz * difMatriz

indicesOrdenados :: [Double] -> [Int]
indicesOrdenados lista =
   let listaOrdenada = nub $ sort lista
   in concatMap (\x -> L.elemIndices x lista) listaOrdenada

recomendador :: [(String,String)] -> [String] -> String -> Int -> Int -> 
                                     Double -> [String]
recomendador datos palabrasVacias nombre n nClaves porcentaje =
   let claves = L.map fst (palabrasClave datos palabrasVacias nClaves)
       nombres = keys $ frecuencia $ L.map fst datos
       matriz = palabrasMatriz datos claves nombres
       (autovalores, autovectores) = eigDescomposicion matriz
       nAutoV = nAutovectores autovalores porcentaje
       pcaMatriz = analisisComponentesPrincipales autovectores matriz nAutoV
       indNombre = fromMaybe (error $ "Error: No se encuentra el elemento "
              ++ nombre ++ " en la base de datos.") (elemIndex nombre nombres)
       sumaDeCuadrados = LA.toList $ distEuclidCuadrPorFila pcaMatriz indNombre
       masCercanos = indicesOrdenados sumaDeCuadrados
   in L.map (\ind -> genericIndex nombres ind) 
            (take n (L.filter (/= indNombre) masCercanos))
