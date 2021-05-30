import Text.Printf

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

svgCircle :: Int -> Int -> Int -> String -> String 
svgCircle x y r style = 
  printf "<circle cx='%d' cy='%d' r='%d' fill='%s' />\n" x y r style

-- Gera SVG com 2 círculos, um verde e um vermelho, com 0.4 de opacidade.
-- A opacidade pode não ser suportada em alguns visualizadores de SVG.
svgAll :: String
svgAll = 
  svgBegin 500 500 ++
  (foldl (\acc x -> acc ++ x) "" circles) ++
  svgEnd

main :: IO ()
main = do
  writeFile "circles.svg" svgAll

--

color :: Int -> Int -> Int -> Float -> String
color r g b a = printf "rgb(%d, %d, %d, %.2f)" r g b a

right_triangles :: [(Int, Int, Int)]
right_triangles = [(h, ca, cb) | h <- [1..], ca <- [1..h], cb <- [1..ca], h^2 == ca^2 + cb^2]

first  (a,_,_) = a
second (_,a,_) = a
third  (_,_,a) = a

first_500_right_triangles = takeWhile (\triangle -> first triangle < 500) right_triangles

corresponding_colors = map (\triangle -> color (first triangle `mod` 256) (second triangle `mod` 256) (third triangle `mod` 256) 0.5) first_500_right_triangles

circles :: [String]
circles = map (\((h, ca, cb), color) -> svgCircle h ca cb color) (zip first_500_right_triangles corresponding_colors)
