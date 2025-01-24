module Clasificador where

import Data.List as L
import Data.Hashable
import Data.HashMap.Strict as HM
import Data.Ord

frecuencia :: (Eq k, Data.Hashable.Hashable k, Integral v) => [k] -> HashMap k v
frecuencia [] = HM.empty
frecuencia (x:xs) = HM.insertWith (+) x 1 (frecuencia xs)

frecClase :: [(String, String)] -> HashMap String Double
frecClase datos =
  HM.map fromIntegral (frecuencia (L.map fst datos))

probClase :: String -> Double -> HashMap String Double -> Double
probClase clase nExtractos frecuencias = 
  lookupDefault 0 clase (HM.map (/nExtractos) frecuencias)

frecPalabrasPorClase :: [(String, String)] -> String -> HashMap String Integer
frecPalabrasPorClase datos clase =
   frecuencia (concatMap words (L.map snd
              (L.filter (\x-> clase == (fst x)) datos)))

probSuaviPalabraDadaClase :: String -> String
                          -> [(String, String)] -> Double -> Double
probSuaviPalabraDadaClase palabra clase datos nPalabras =
   let frecPalabrasEnClase = frecPalabrasPorClase datos clase
       nPalabrasEnClase = fromInteger $ sum $ HM.elems frecPalabrasEnClase
       nPalabraEnClase = fromInteger $
                          lookupDefault 0 palabra frecPalabrasEnClase
   in (nPalabraEnClase + 1)/(nPalabrasEnClase + nPalabras)

verosiClaseDadoTexto :: String -> String -> Double ->
        HashMap String Double -> [(String, String)]-> Double -> Double
verosiClaseDadoTexto clase texto nPalabras frecuencias datos nExtractos =
    let pTexto = L.map (\palabras -> probSuaviPalabraDadaClase palabras clase 
                                          datos nPalabras) (words texto)
        pClase = probClase clase nExtractos frecuencias
    in pClase * (product pTexto)

clasificadorClases :: String -> [(String,String)] ->  HashMap String Double 
          -> [String] -> Double -> Double -> [(String, Double)]
clasificadorClases texto datos frecuencias clases nPalabras nExtractos =
 L.map (\clase ->(clase, verosiClaseDadoTexto clase texto nPalabras
                                       frecuencias datos nExtractos)) clases

clasificador :: String -> [(String,String)] -> String
clasificador texto datos =
     let frecuencias = frecClase datos
         clases = HM.keys $ (frecuencia . L.map fst) datos
         nPalabras = fromInteger $ sum $ HM.elems
                     ((frecuencia . concatMap words) (L.map snd datos))
         nExtractos = sum $ HM.elems frecuencias
         probs = clasificadorClases texto datos frecuencias 
                                    clases nPalabras nExtractos        
     in fst (L.maximumBy (comparing snd) probs)
