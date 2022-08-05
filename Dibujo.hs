module Dibujo where

type Bas b = b

-- Definir el lenguaje.
data Dibujo a = Basica (Bas a) | Rotar (Dibujo a) | Rot45 (Dibujo a) 
              | Espejar (Dibujo a)
              | Apilar Int Int (Dibujo a) (Dibujo a)
              | Juntar Int Int (Dibujo a) (Dibujo a)
              | Encimar (Dibujo a) (Dibujo a)
              deriving (Show,Eq)

              --flip(p)(a;b;c)=p(a+b;-b;c)

---- Composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a
comp f n a | n <= 0 = a
           | n > 0 = comp f (n-1)  (f a)


-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 a = comp Rotar 2 a

r270 :: Dibujo a -> Dibujo a  
r270 a = comp Rotar 3 a

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) a b = Apilar 100 100 a b

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) fig_a fig_b = Juntar 100 100 fig_a fig_b

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) fig_a fig_b = Encimar fig_a fig_b

-- Dadas cuatro figuras las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto fig_a fig_b fig_c fig_d = (fig_a /// fig_b) .-. (fig_c /// fig_d)

-- Una figura repetida con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a 
encimar4 fig_a = Encimar (Encimar (Encimar (fig_a) (Rotar fig_a)) (r180 fig_a)) (r270 fig_a)

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar fig_a = cuarteto (fig_a) (Rotar fig_a) (r180 fig_a) (r270 fig_a)

-- ESQUEMAS PARA PARA LA MANIPULACION DE FIGURAS BASICAS

-- constructor de figura basica
pureDib :: a -> Dibujo a
pureDib a = Basica a

-- map para nuestro lenguaje.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Basica a) = Basica (f a)
mapDib f (Rotar a) = Rotar (mapDib f a)
mapDib f (Rot45 a) = Rot45 (mapDib f a)
mapDib f (Espejar a) = Espejar (mapDib f a)
mapDib f (Apilar a b dib_a dib_b) = Apilar a b (mapDib f dib_a) (mapDib f dib_b)
mapDib f (Juntar a b dib_a dib_b) = Juntar a b (mapDib f dib_a) (mapDib f dib_b)
mapDib f (Encimar a b) = Encimar (mapDib f a) (mapDib f b)

-- semantica
sem :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Int -> Int -> b -> b -> b) -> 
       (Int -> Int -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a -> b
sem bas rot r45 esp api jun enc (Basica a) = bas a 
sem bas rot r45 esp api jun enc (Rotar a) = rot (sem bas rot r45 esp api jun enc a) 
sem bas rot r45 esp api jun enc (Rot45 a) = r45 (sem bas rot r45 esp api jun enc a)
sem bas rot r45 esp api jun enc (Espejar a) = esp (sem bas rot r45 esp api jun enc a)
sem bas rot r45 esp api jun enc (Apilar x y a b) = api x y (sem bas rot r45 esp api jun enc a) (sem bas rot r45 esp api jun enc b)
sem bas rot r45 esp api jun enc (Juntar x y a b) = jun x y (sem bas rot r45 esp api jun enc a) (sem bas rot r45 esp api jun enc b)
sem bas rot r45 esp api jun enc (Encimar a b) = enc (sem bas rot r45 esp api jun enc a) (sem bas rot r45 esp api jun enc b)

type Pred a = a -> Bool

-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP pred_1 pred_2 a = pred_1 a && pred_2 a

-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP pred_1 pred_2 a = pred_1 a || pred_2 a

-- Dado un predicado sobre básicas, cambiar todas las que satisfa cen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> a -> Dibujo a -> Dibujo a
cambiar pred basica fig = mapDib (\ x -> if (pred x) then basica else x) fig                          

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool 
anyDib pred = sem pred id id id or2 or2 (||) where or2 _ _ = (||)

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib pred = sem pred id id id and2 and2 (&&) where and2 _ _ = (&&)

-- Describe la figura. Ejemplos: 
--   desc (const "b") (Basica b) = "b"
--   desc (const "b") (Rotar (Basica b)) = "rot (b)"
--   desc (const "b") (Apilar n m (Basica b) (Basica b)) = "api n m (b) (b)"
-- La descripción de cada constructor son sus tres primeros
-- símbolos en minúscula, excepto `Rot45` al que se le agrega el `45`.
desc :: (a -> String) -> Dibujo a -> String
desc f (Basica a) = f a
desc f (Rotar a) = "rot" ++ " (" ++ (desc f a) ++ ")"
desc f (Rot45 a) = "rot45" ++ " (" ++ (desc f a) ++ ")"
desc f (Espejar a) = "esp" ++ " (" ++ (desc f a) ++ ")"
desc f (Apilar a b c d) = "api "++ show a ++ " " ++  show b ++ " ("
                         ++ (desc f c) ++ ")" ++ " (" ++ (desc f d) ++")" 
desc f (Juntar a b c d) = "jun "++ show a ++ " " ++ show b ++ " (" 
                         ++ (desc f c) ++ ")" ++ " (" ++ (desc f d)
desc f (Encimar a b) = "enc" ++ " (" ++ (desc f a) ++ ")" ++ " (" 
                      ++ (desc f b) ++ ")"

-- Junta todas las figuras básicas de un dibujo.
basicas :: Dibujo a -> [a]
basicas = sem (:[]) id id id unir unir (++) where unir _ _ = (++)

-- Estos predicados indican una superfluocidad de operaciones

-- Hay 4 rotaciones seguidas.
esRot360 :: Pred (Dibujo a)
esRot360 (Basica a) = False
esRot360 (Rotar(Rotar(Rotar(Rotar a)))) = True 
esRot360 (Rotar a) = esRot360 a
esRot360 (Rot45 a) = esRot360 a 
esRot360 (Espejar a) = esRot360 a
esRot360 (Apilar a b c d) = esRot360 c || esRot360 d
esRot360 (Juntar a b c d) = esRot360 c || esRot360 d
esRot360 (Encimar a b) = esRot360 a || esRot360 b

-- Hay 2 espejados seguidos.
esFlip2 :: Pred (Dibujo a)
esFlip2 (Basica a) = False
esFlip2 (Espejar(Espejar a)) = True
esFlip2 (Espejar a) = esFlip2 a 
esFlip2 (Rotar a) = esFlip2 a
esFlip2 (Rot45 a) = esFlip2 a 
esFlip2 (Apilar a b c d) = esFlip2 c || esFlip2 d
esFlip2 (Juntar a b c d) = esFlip2 c || esFlip2 d
esFlip2 (Encimar a b) = esFlip2 a || esFlip2 b

data Superfluo = RotacionSuperflua | FlipSuperfluo
                deriving (Show,Eq)

-- Aplica todos los chequeos y acumula todos los errores, y
-- sólo devuelve la figura si no hubo ningún error.
check :: Dibujo a -> Either [Superfluo] (Dibujo a)
check fig | (esFlip2  fig) && (esRot360 fig) = Left (FlipSuperfluo :RotacionSuperflua:[])
          | esRot360 fig = Left (RotacionSuperflua : [])
          | esFlip2 fig = Left (FlipSuperfluo : [])
          | otherwise = Right fig
