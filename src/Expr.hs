{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- que es esto? se puede sacar?
{-# HLINT ignore "Use infix" #-} -- esto es del hls, esto si se puede sacar
{-# HLINT ignore "Use first" #-} -- esto es del hls, esto si se puede sacar
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


-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval expr = foldExpr
                     (\x g -> (x, g))
                     (\x y g-> dameUno (x, y) g)
                     (\fx fy g-> let (x, g1) = fx g ; (y, g2) = fy g1 in (x + y, g2)) --suma
                     (\fx fy g-> let (x, g1) = fx g ; (y, g2) = fy g1 in (x - y, g2)) --resta
                     (\fx fy g-> let (x, g1) = fx g ; (y, g2) = fy g1 in (x * y, g2)) --mult
                     (\fx fy g-> let (x, g1) = fx g ; (y, g2) = fy g1 in (x / y, g2)) --div
                      expr
-- podriamos explicar por lo menos en el caso de la suma que estamos haciendo


-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = let valores = muestra f n g
                            in (histograma m (rango95 (fst valores)) (fst valores), snd valores)
                            -- aca lo que devolvemos es de tipo (Histograma, Gen) donde el histograma lo armamos a partir de ....



-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr g = armarHistograma m n (eval expr) g


-- ESTO LO BORRAMOS?
-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- COMPLETAR EJERCICIO 10

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- COMPLETAR EJERCICIO 10

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = recrExpr
         (\x -> show x)   --const
         (\x y -> show x ++ "~" ++ show y)   --rango ∼
         (\tipoDeX x tipoDeY y -> maybeParen (elem (constructor tipoDeX) [CEDiv, CEMult]) x
                                 ++ " + "
                                 ++ maybeParen (elem (constructor tipoDeY) [CEMult,CEDiv]) y )   --sum
         (\tipoDeX x tipoDeY y -> maybeParen (elem (constructor tipoDeX) [CEMult,CEDiv,CEResta]) x
                                 ++  " - "
                                 ++ maybeParen (elem (constructor tipoDeY) [CEMult,CEDiv,CEResta]) y)   --rest
         (\tipoDeX x tipoDeY y -> maybeParen (elem (constructor tipoDeX) [CEResta,CESuma]) x
                                 ++ " * "
                                 ++ maybeParen (elem (constructor tipoDeY) [CEResta,CESuma]) y )   --mult
         (\tipoDeX x tipoDeY y -> maybeParen (elem (constructor tipoDeX) [CEResta,CESuma]) x
                                 ++ " / "
                                 ++ maybeParen (elem (constructor tipoDeY) [CEResta,CESuma]) y )   --div


                                 
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