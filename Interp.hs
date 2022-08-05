module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo
type FloatingPic = Vector -> Vector -> Vector -> Picture
type Output a = a -> FloatingPic

-- el vector nulo
zero :: Vector
zero = (0,0)

half :: Vector -> Vector
half = (0.5 V.*)

-- comprender esta función es un buen ejericio.
hlines :: Vector -> Float -> Float -> [Picture]
hlines (x,y) mag sep = map (hline . (*sep)) [0..]
  where hline h = line [(x,y+h),(x+mag,y+h)] 

-- Una grilla de n líneas, comenzando en v con una separación de sep y
-- una longitud de l (usamos composición para no aplicar este
-- argumento)
grid :: Int -> Vector -> Float -> Float -> Picture
grid n v sep l = pictures [ls,translate 0 (l*toEnum n) (rotate 90 ls)]
  where ls = pictures $ take (n+1) $ hlines v sep l

-- figuras adaptables comunes
trian1 :: FloatingPic
trian1 a b c = line $ map (a V.+) [zero, half b V.+ c , b , zero]

trian2 :: FloatingPic
trian2 a b c = line $ map (a V.+) [zero, c, b,zero]

trianD :: FloatingPic
trianD a b c = line $ map (a V.+) [c, half b , b V.+ c , c]

myBlank _ _ _ = blank

rectan :: FloatingPic
rectan a b c = line [a, a V.+ b, a V.+ b V.+ c, a V.+ c,a]

simple :: Picture -> FloatingPic
simple p _ _ _ = p

fShape :: FloatingPic
fShape a b c = line . map (a V.+) $ [ zero,uX, p13, p33, p33 V.+ uY , p13 V.+ uY 
                 , uX V.+ 4 V.* uY ,uX V.+ 5 V.* uY, x4 V.+ y5
                 , x4 V.+ 6 V.* uY, 6 V.* uY, zero]    
  where p33 = 3 V.* (uX V.+ uY)
        p13 = uX V.+ 3 V.* uY
        x4 = 4 V.* uX
        y5 = 5 V.* uY
        uX = (1/6) V.* b
        uY = (1/6) V.* c

-- Dada una función que produce una figura a partir de un a y un vector
-- producimos una figura flotante aplicando las transformaciones
-- necesarias. Útil si queremos usar figuras que vienen de archivos bmp.
transf :: (a -> Vector -> Picture) -> a -> Vector -> FloatingPic
transf f d (xs,ys) a b c  = translate (fst a') (snd a') .
                             scale (magV b/xs) (magV c/ys) .
                             rotate ang $ f d (xs,ys)
  where ang = radToDeg $ argV b
        a' = a V.+ half (b V.+ c)

-- rotar(f)(x, w, h)	f(x+w, h, -w)
rotar :: FloatingPic -> FloatingPic
rotar f a b c = f (a V.+ b) c ((b V.- b) V.-b)

--rot45(f)(x, w, h)	f(x+(w+h)/2, (w+h)/2, (h-w)/2)
rot45 :: FloatingPic -> FloatingPic
rot45 f a b c =  f (a V.+ half (b V.+ c)) (half (b V.+ c)) (half (c V.-b))

--espejar(f)(x, w, h)	f(x+w, -w, h)
espejar :: FloatingPic -> FloatingPic
espejar f a b c = f (a V.+ b) ((b V.- b) V.-b) c

--encimar(f,g)(x, w, h)	f(x, w, h) ∪ g(x, w, h)
encimar :: FloatingPic -> FloatingPic -> FloatingPic
encimar f g a b c = pictures [ f a  b  c, g a b c]  

--juntar(n, m, f, g)(x, w, h)  f(x, w', h) ∪ g(x+w', r'w, h) 
--con r'=n/(m+n), r=m/(m+n), w'=rw

juntar :: Int -> Int -> FloatingPic -> FloatingPic -> FloatingPic
juntar n m f g a b c = pictures [ f a (r V.* b) c 
                                 , g (a V.+ w') (r' V.* b) c] 
  where r = (fromIntegral n) / (fromIntegral (n + m))
        r' =  (fromIntegral m) / (fromIntegral (n + m))
        w' = r V.* b 


--apilar(n, m, f, g)(x, w, h) f(x + h', w, rh) ∪ g(x, w, h') 
--con r' = n/(m+n), r=m/(m+n), h'=r'h
apilar :: Int -> Int -> FloatingPic -> FloatingPic -> FloatingPic
apilar n m f g a b c  = pictures [f (a V.+ h') b (r V.* c), g a b h']
  where r = (fromIntegral n) / (fromIntegral (n + m))
        r' = (fromIntegral m) / (fromIntegral (n + m))
        h' = r' V.* c

-- interp :: (a->FloatingPic) -> Dibujo a -> FloatingPic
interp :: Output a -> Output (Dibujo a)
interp f = sem f rotar rot45 espejar apilar juntar encimar  

