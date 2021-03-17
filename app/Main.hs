import Graphics.Gloss
    ( white,
      display,
      Display(InWindow),
      Picture(Text, Translate, Scale), makeColorI, animate, simulate, Color )
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Picture

background :: Color
background =            makeColorI 72 62 51 255

window :: Display
window = InWindow "Tic Tac Bro" (700, 600) (500, 500)

initGert :: Int
initGert = 10

update :: ViewPort -> Float -> Int -> Int
update _ _ gert = gert + 15

main :: IO ()
main = simulate window background 30 initGert picture update

mooi :: Int -> Int -> Int
mooi gert base = 
  let piet = gert `div` base
  in mod piet 256

kruisje :: Point -> Float -> Picture
kruisje (x,y) size = 
  let girth = size / 10
      slab = rectangleSolid girth size
  in translate x y $ pictures [ rotate 45 slab, rotate (-45) slab ]

picture :: Int -> Picture
picture gert = color (makeColorI (mooi gert 65536) (mooi gert 256) (mooi gert 1) 255) $ kruisje (50,150) 200