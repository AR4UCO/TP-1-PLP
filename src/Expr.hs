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
-- para los casos recursivos, lo que se hace es pasar un lambda que reciba tres parametros, los cuales
-- fx fy son de tipo G Float y g de tipo Gen. Luego se evalua fx g y eso devuelve algo de tipo (Float, Gen)
-- usamos ese nuevo Gen en fy y luego hacemos la operacion correspondiente con los Floats al caso en el que estamos,
-- guardando el ultimo generador devuelto por (y, g2). De esta manera tenemos siempre un generador distinto.


-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = let valores = muestra f n g
                            in (histograma m (rango95 (fst valores)) (fst valores), snd valores)
-- como devolvemos algo de tipo (Histograma, Gen), el gen que pasamos es el ultimo generado en valores.



-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr g = armarHistograma m n (eval expr) g


-- ESTO LO BORRAMOS?
-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,0,1],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,348,0,736],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = recrExpr
         (\x -> show x)   --const
         (\x y -> show x ++ "~" ++ show y)   --rango ∼
         (\exprDeX x exprDeY y -> maybeParen (elem (constructor exprDeX) [CEDiv, CEMult]) x
                                 ++ " + "
                                 ++ maybeParen (elem (constructor exprDeY) [CEMult,CEDiv]) y )   -- caso suma
         (\exprDeX x exprDeY y -> maybeParen (elem (constructor exprDeX) [CEMult,CEDiv,CEResta]) x
                                 ++  " - "
                                 ++ maybeParen (elem (constructor exprDeY) [CEMult,CEDiv,CEResta]) y)   -- caso resta
         (\exprDeX x exprDeY y -> maybeParen (elem (constructor exprDeX) [CEResta,CESuma]) x
                                 ++ " * "
                                 ++ maybeParen (elem (constructor exprDeY) [CEResta,CESuma]) y )   -- caso mult
         (\exprDeX x exprDeY y -> maybeParen (elem (constructor exprDeX) [CEResta,CESuma]) x
                                 ++ " / "
                                 ++ maybeParen (elem (constructor exprDeY) [CEResta,CESuma]) y )   -- caso div


                                 
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
