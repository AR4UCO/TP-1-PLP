module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)



recrExpr :: (Float -> b) 
         -> (Float -> Float -> b) 
         -> (Expr -> b -> Expr -> b -> b) 
         -> (Expr -> b -> Expr -> b -> b) 
         -> (Expr -> b -> Expr -> b -> b) 
         -> (Expr -> b -> Expr -> b -> b) 
         -> Expr 
         -> b
recrExpr f1 f2 f3 f4 f5 f6 a = case a of
                               Const x -> f1 x
                               Rango x y -> f2 x y
                               Suma x y -> f3 x (rec x) y (rec y)
                               Resta x y -> f4 x (rec x) y (rec y)
                               Mult x y -> f5 x (rec x) y (rec y)
                               Div x y -> f6 x (rec x) y (rec y)
                              where rec = recrExpr f1 f2 f3 f4 f5 f6



foldExpr :: (Float -> b) 
         -> (Float -> Float -> b) 
         -> (b -> b -> b) 
         -> (b -> b -> b) 
         -> (b -> b -> b) 
         -> (b -> b -> b) 
         -> Expr 
         -> b
foldExpr f1 f2 f3 f4 f5 f6 a = case a of
                                Const x -> f1 x
                                Rango x y -> f2 x y
                                Suma x y -> f3 (rec x) (rec y)
                                Resta x y -> f4 (rec x) (rec y)
                                Mult x y -> f5 (rec x) (rec y)
                                Div x y -> f6 (rec x) (rec y)
                              where rec = foldExpr f1 f2 f3 f4 f5 f6




eval :: Expr -> G Float
eval expr = foldExpr 
                     (\x g -> (x, g)) 
                     (\x y g-> dameUno (x, y) g) 
                     (\fx fy g-> let (x, g1) = fx g ; (y, g2) = fy g1 in (x + y, g2)) --suma
                     (\fx fy g-> let (x, g1) = fx g ; (y, g2) = fy g1 in (x - y, g2)) --resta
                     (\fx fy g-> let (x, g1) = fx g ; (y, g2) = fy g1 in (x * y, g2)) --mult
                     (\fx fy g-> let (x, g1) = fx g ; (y, g2) = fy g1 in (x / y, g2)) --div
                      expr




-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = let valores = muestra f n g
                            in (histograma m (rango95 (fst valores)) (fst valores), (snd valores))



-- Aplica una función múltiples veces a un generador.
-- muestra :: G a -> Int -> G [a]
-- muestra _ 0 g = ([], g)
-- muestra f n g = (x : xs, sf)
--   where
--     (x, s1) = f g
--     (xs, sf) = muestra f (n - 1) s1


-- | Dada una lista finita no vacía de números reales devuelve un
-- rango con un 95% de confianza que contienen los valores de la lista
-- asumiendo que es una muestra de una distribución normal.
-- Si todos los números son iguales, devuelve el rango @(valor-1, valor+1)@.
--rango95 :: [Float] -> (Float, Float)
--rango95 xs = (promedio - s, promedio + s)
--  where
--    cantidad = fromIntegral (length xs)
--    promedio = sum xs / cantidad
-- desviacion = sqrt $ sum [(x - promedio) ^ 2 | x <- xs] / cantidad
-- s = if desviacion == 0 then 1 else desviacion * 1.96



--histograma :: Int -> (Float, Float) -> [Float] -> Histograma
--histograma m r l = foldr agregar (vacio n r) l




-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr g = armarHistograma m n (eval expr) g



-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- COMPLETAR EJERCICIO 10

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- COMPLETAR EJERCICIO 10

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = error "COMPLETAR EJERCICIO 11"



-- mostrar = recrExpr 
--                   (\x -> show x)   --const
--                   (\x y -> (show x) ++ " ~ " ++ (show y))   --rango ∼
--                   (\TipoDeX x TipoDeY y -> x ++ " + " ++ y )   --sum
--                   ()   --rest
--                   ()   --mult
--                   ()   --div

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s






{--

mostrar :: Expr -> String
mostrar = recrExpr
  (\x -> show x)                                   -- Const
  (\x y -> show x ++ "∼" ++ show y)                -- Rango
  (\ex sx ey sy -> sx ++ " + " ++ sy)              -- Suma
  (\ex sx ey sy -> maybeParen (constructor ex == CEResta) sx 
                ++ "- " 
                ++ maybeParen (constructor ey /= CEConst && constructor ey /= CERango) sy) -- Resta
  (\ex sx ey sy -> 
      let izq = maybeParen (constructor ex `elem` [CESuma, CEResta]) sx
          der = maybeParen (constructor ey `elem` [CESuma, CEResta]) sy
      in izq ++ " * " ++ der)                      -- Mult
  (\ex sx ey sy -> 
      let izq = maybeParen (constructor ex /= CEConst && constructor ex /= CERango) sx
          der = maybeParen True sy
      in izq ++ " / " ++ der)                      -- Div

--}