module Basica.Escher where
import Dibujo
import Interp


data MyBasic = Trian | Blanca --

interpBasic :: Output MyBasic
interpBasic Trian = trian2    
--interpBasic Trian = fShape
interpBasic Blanca = myBlank 


type Escher = MyBasic

-- El dibujo t.
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT d = Encimar d (Encimar (trian2) (trian3))
    where trian2 = Espejar (Rot45 d)
          trian3 = r270 trian2

-- El dibujoU  
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU d = Encimar (Encimar (trian2) (Rotar (trian2))) 
                         (Encimar (r180 (trian2)) (r270 (trian2))) 
    where trian2 = Espejar(Rot45 d)
 --         trian3 = r270 trian2

-- Esquina con nivel de detalle en base a la figura p.
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 0 d = Basica Blanca
esquina n d = cuarteto (esquina (n-1) d) (lado (n-1) d)
                         (Rotar (lado (n-1) d)) (dibujoU d)

-- Lado con nivel de detalle.
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 0 d = Basica Blanca
lado n d = cuarteto (lado (n-1) d) (lado (n-1) d)
                     (Rotar (dibujoT d)) (dibujoT d)

-- Por suerte no tenemos que poner el tipo!
noneto p q r s t u v w x = Apilar 1 2 (Juntar 1 2 p (Juntar 1 1 q r)) 
                                      (Apilar 3 3 (Juntar 1 2 s (Juntar 1 1 t u)) 
                                                  (Juntar 1 2 v (Juntar 1 1 w x)))

-- El dibujo de Escher:

                
escher n e = noneto (esquina n d) (lado n d) (Espejar(esquina n d)) (Rotar(lado n d))
                (dibujoU d) (r270(lado n d)) (Rotar(esquina n d)) (r180(lado n d))
                (r180(esquina n d))
             where d = e

escher2 = escher 2 (Basica Trian)

cuadrado_p = esquina 2 (Basica Trian)

pyq = Juntar 1 1 (esquina 2 (Basica Trian)) (lado 2 (Basica Trian))
dibtest = dibujoT (Basica Trian)
