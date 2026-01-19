module Main where

import Data.List (sort)
import Text.Printf (printf)
import Data.Char (toUpper)

data Auto = Auto 
    { precio      :: String
    , transmision :: String
    , kilometraje :: String
    } deriving Show

data Stats = Stats
    { sSum    :: Double
    , sMean   :: Double
    , sStd    :: Double
    , sMedian :: Double
    , sMin    :: Double
    , sMax    :: Double
    } deriving Show


computeStats :: [Double] -> Stats
computeStats [] = error "No hay datos para calcular las estadisticas"
computeStats numbers = Stats
    { sSum    = suma
    , sMean   = promedio
    , sStd    = desviacionEstandar
    , sMedian = mediana
    , sMin    = minimum numbers         
    , sMax    = maximum numbers
    }
  where
    n = fromIntegral (length numbers)
    suma = sum numbers                 
    promedio = suma / n
    varianza = sum [ (x - promedio)^2 | x <- numbers ] / n         
    desviacionEstandar = sqrt varianza
    sortedNumbers = sort numbers
    mid = length numbers `div` 2
    mediana = if odd (length numbers)
              then sortedNumbers !! mid
              else (sortedNumbers !! (mid - 1) + sortedNumbers !! mid) / 2


transformText :: String -> Int -> Char -> String
transformText text l padChar = 
    let 
        upperText = map toUpper text
        collapsed = unwords (words upperText) 
        currentLen = length collapsed       
    in 
        if currentLen > l 
        then take l collapsed   
        else collapsed ++ replicate (l - currentLen) padChar    


formatearReporte :: String -> Stats -> String       
formatearReporte titulo s = 
    printf "\nEstadisticas de %s \n" titulo ++
    printf "Suma Total:      %.2f\n" (sSum s) ++
    printf "Promedio (Mean):  %.2f\n" (sMean s) ++
    printf "Desv. estandar:  %.2f\n" (sStd s) ++
    printf "Mediana:         %.2f\n" (sMedian s) ++
    printf "Minimo:          %.2f\n" (sMin s) ++
    printf "Maximo:          %.2f\n" (sMax s) ++
    "\n"

splitOnComma :: String -> [String]
splitOnComma "" = []
splitOnComma s = 
    let (extraido, resto) = break (== ',') s    
    in extraido : case resto of         
                    [] -> []
                    (_:r) -> splitOnComma r

procesarFila :: [String] -> Maybe Auto
procesarFila columnas   
    | length columnas >= 5 = Just $ Auto (columnas !! 2) 
      (columnas !! 3) (columnas !! 4)
    | otherwise = Nothing

--variables constantes
longitudL :: Int
longitudL = 10

caracterPad :: Char
caracterPad = '*'

main :: IO ()
main = do
    contenido <- readFile "bmw.csv"         
    let todasLasLineas = lines contenido    
    let datosSinEncabezado = drop 1 todasLasLineas 
    
    let listaAutos = [auto | linea <- datosSinEncabezado,       
                             let columnas = splitOnComma linea,
                             Just auto <- [procesarFila columnas]]      
    
    let transmisionesLimpias = map (\a ->                                  
            let original = transmision a        
                transformada = transformText original longitudL caracterPad 
            in printf "%-12s -> %s" original transformada :: String        
          ) listaAutos
    
    --Se hacen los calculos estadisticos
    let precios = [read (precio a) :: Double | a <- listaAutos]         
    let statsPrecio = computeStats precios
    let kms = [read (kilometraje a) :: Double | a <- listaAutos]
    let statsKms = computeStats kms
    

    --Se guarda todo en un reporte
    let reporteStats = formatearReporte "precio" statsPrecio ++ 
                       formatearReporte "kilometraje" statsKms
    
    let reporteTransmisiones = "\n TEXTO TRANSFORMADO:\n" ++ "Original -> Transformado (Longitud=" ++ show longitudL ++ ", padChar " ++ show caracterPad ++ ")\n" ++ 
                               unlines transmisionesLimpias
    
    writeFile "results.txt" (reporteStats ++ reporteTransmisiones)
    
    putStrLn "Proceso completado y guardado en results.txt"