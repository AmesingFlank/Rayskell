module Image(Image,Color,produceImage) where

type Color = (Float,Float,Float)

type Image = [[Color]]


countRows im = length im

countCols im = length (head im)

showColor (r,g,b) = show (round r) ++ " "++ show (round g) ++ " " ++ show (round b) ++ "\n"

imageToText :: Image -> String
imageToText im = 
    "P3\n" ++ 
    show (countCols im) ++ " " ++ show (countRows im) ++ "\n" ++
    "255\n"++
    concat (map showColor (concat im))


produceImage :: String -> Image -> IO()
produceImage fileName im = writeFile fileName (imageToText im)
    