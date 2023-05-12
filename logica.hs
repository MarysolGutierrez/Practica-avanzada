-- Definir la funci ́on nand a b = not (a && b) en Haskell sin utilizar not y &&.
nand:: Bool->Bool->Bool
nand True True = False
nand True False = True
nand False _ = True
-- ---------------------------------------------------- 
-- maj retorna True sii al menos 2 argumentos son True.
-- ----------------------------------------------------
maj :: Bool -> Bool -> Bool -> Bool
maj x y z |(x==True && y==True) || (y==True && z==True) || (x==True && z==True) = True
          |otherwise= False

-- ---------------------------------------------------- 
-- Para las siguientes funciones se debe respetar el 
-- perfil propuesto.
-- La lista [Int] de paraTodo representa las posiciones 
-- sobre las que cuantificamos en [a].
-- Mientras que (Int -> [a] -> Bool) es la propiedad.
--		Ejemplo: paraTodo [0,1,2,3] [4,1,2,6] even 
--		retorna False, ya que existe una posición 
--		en la que el elemento de la lista es impar. 
--		paraTodo [0,2,4,6] [2,2,4,4,4,5,6] even  
--		retorna True.
-- ----------------------------------------------------
paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
paraTodo xs ys p = and [p x ys| x <- xs]

iseven :: Int -> [Int] -> Bool
iseven n xs = mod (xs!!n) 2 == 0

--------------------------------------------------------
-- Toma una lista y verifica si todos los elementos 
-- cumplen con un cierto predicado p. 
-- Cuantificador = (∀i : 0 ≤ i < #xs : even xs.i)
-- Haskell = and [even x | x <- xs]

paraTodo2 :: [a] -> (a -> Bool) -> Bool
paraTodo2 xs p = and[p x| x<-xs]
-- ----------------------------------------------------
-- La lista [Int] de paraTodo representa las posiciones 
-- sobre las que cuantificamos en [a]. 
-- (Int -> [a] -> Bool) es la propiedad.
--
--		Ejemplo: existe [0,1,2,3] [4,1,2,6] odd
--		retorna True.
-- ----------------------------------------------------
existe :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
existe xs ys p = or [p x ys| x<-xs]

isodd :: Int -> [Int] -> Bool
isodd n xs = mod (xs!!n) 2 /= 0

--------------------------------------------------------
--Toma una lista y verifica  que al menos un elemento 
--cumpla con ciento predicado p.
--Cuantificador = (∃i : 0 ≤ i < #xs : even xs.i )
--Hakell = or [even x | x <- xs]

existe2 :: [a] -> (a -> Bool) -> Bool
existe2 xs p= or [p x | x <- xs]

-------------------------------------------------------
sumatoria :: [Int] -> [a] -> (Int -> [a] -> Int) -> Int
sumatoria xs ys f= sum [f x ys| x <- xs]

ident :: Int -> [Int] -> Int
ident n xs = xs!!n

-------------------------------------------------------
productoria :: [Int] -> [a] -> (Int -> [a] -> Int) -> Int
productoria xs ys f= product [f x ys| x <- xs]

--------------------------------------------------------
contatoria :: [Int] -> [a] -> (Int -> [a] -> Bool) -> Int
contatoria xs ys p = length [x | x <- xs, p x ys == True]



--Agrego linea 79 para probar git xd
