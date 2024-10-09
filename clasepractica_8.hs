data [a] = [] -- caso base
		|  a : [a] -- caso recursivo

-- conjunto inductivo
-- Sea a un tipo cualquiera,
-- una lista se forma con las siguientes reglas:
-- regla base) [] está en el conjunto [a]
-- regla inductiva) si a es un elemento cualquiera, y xs es una lista de a cualquiera,
-- entonces a : [a] está en el conjunto [a]

-- esquema de recorrido
-- recursion estructural explicita
-- f [] = []
-- f (x:xs) = ... f xs

-- siempre termina (recursión)
-- inducción (vi todos los casos, para todos los casos posibles del conjunto)

length :: [a] -> Int
length [] =  0 -- neutro de la operacion
length (x:xs) = 1 + length xs

sumatoria :: [Int] -> Int
f []     = 0
f (x:xs) = x + sumatoria xs

producto :: [Int] -> Int
f []     = 1
f (x:xs) = x * sumatoria xs

todosTrue :: [Bool] -> Bool 
todosTrue [] 	 = True
todosTrue (x:xs) = x && todosTrue xs

algunoTrue :: [Bool] -> Bool
algunoTrue []     = False
algunoTrue (x:xs) = x || algunoTrue xs

(++) :: [a] -> [a] -> [a]
-- si tengo dos elementos, tengo que ver cual recorro (trato de recorrer el primero)
(++) [] ys	   = ys
(++) (x:xs) ys = x : (++) xs ys 

-- trato de lograr una expresión que represente la respuesta que quiero
-- que pasa si recorro las dos listas al mismo tiempo?

take :: Int -> [a] -> [a]
-- take 0 []     = [] -- no hace falta analizar si es 0, la linea de abajo lo contempla
take n []     = [] -- base
-- take 0 (x:xs) = [] -- se analiza en la linea de abajo
take 0 _      = []
take n (x:xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop 0 []     = []
drop n []     = []
drop 0 (x:xs) = (x:xs)
drop n (x:xs) = drop (n - 1) xs 

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x, y) : zip xs ys
zip _ _ 		  = [] -- este pm contempla 3 casos

length (xs ++ ys) = length xs + length ys
-- length se distribuye con respecto al (++) sobre las listas

sumatoria (xs ++ ys) = sumatoria xs + sumatoria ys 
producto (xs ++ ys) = producto xs * producto ys
todosTrue (xs ++ ys) = todosTrue xs && todosTrue ys

-- /************************************************/
Para todo xs, ys.
 	length (xs ++ ys) = length xs + length ys

Sea zs una lista cualquiera, voy a hacer induccion estructural sobre zs

¿length (zs ++ ys) = length zs + length ys?


-- Planteo demostración por inducción estructural sobre listas
Caso base zs = []

TB) ¿length ([] ++ ys) = length [] + length ys?

Caso inductivo) zs = (w:ws)

HI) ¡length (ws ++ ys) = length ws + length ys!
TI) ¿length ((w:ws) ++ ys) = length (w:ws) + length ys?

-- demuestro P_(N+1) suponiendo P_N verdadero
P_0 => P_1 => P_2 => ... => P_N => P_(N + 1) => ...

Caso base) 
-- lado izq
length ([] ++ ys)
= -- def (++) [] ys	   = ys
length ys 

-- lado der
length [] + length ys 
= -- def length [] =  0
0 + length ys 
= -- por neutro de la suma
length ys

-- ¡caso base verdadero!

Caso inductivo)
-- lado izq 
length ((w:ws) ++ ys)
-- def (++) => (x:xs) ys = x : (++) xs ys 
length (w : (ws ++ ys))
= -- def length, con x = w, xs = ws ++ ys
1 + length (ws ++ ys)

-- lado der 
length (w:ws) + length ys
= -- def length, con x = w, xs = ws
1 + length ws + length ys 
= -- por hipotesis inductiva
1 + length (ws ++ ys)

-- ¡caso inductivo verdadero!

-- si caso base e inductivo son verdaderos => equivalencia verdadera


-- /************************************************/

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-- demostrar
Para todo xs.
	length xs = length (reverse xs)

-- voy a demostar por inducción estructural sobre listas

Sea xs una lista cualquiera

Caso base) xs = []

TB) ¿length [] = length (reverse [])?

Caso inductivo) xs = (z:zs)
HI) ¡length zs = length (reverse zs)!
TI) ¿length (z:zs) = length (reverse (z:zs))?

-- resolución
Caso base)

-- lado izq
length []
= -- def de length
0

-- lado der
length (reverse [])
= -- def de reverse
length []
= -- def de length
0

-- caso base verdadero

Caso inductivo)

-- lado izq
length (z:zs)
= -- def de length
1 + length zs
= -- por def de length (x:xs) = 1 + length xs
length (z:zs)

-- lado der
length (reverse (z:zs))
= -- def reverse
length (reverse zs ++ [z])
= -- propiedad anteriormente demostrada, si ya se demostró antes
  -- en este caso, se demostró length (xs ++ ys) = length xs + length ys
  -- donde xs = zs, ys = [z]
  -- si antes no lo habia demostrado, pongo lema y lo demuestro en otro lado 
length (reverse zs) + length [z:[]]
= -- def length
length (reverse zs) + 1 + 0 
= -- def length
length (reverse zs) + 1 
= -- por HI length zs = length (reverse zs)
length zs + 1 
= -- por def de length (x:xs) = 1 + length xs
length (z:zs)
