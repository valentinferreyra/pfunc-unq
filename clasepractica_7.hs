data Ingrediente = Aceitunas Int 
				|  Cebolla
				|  Jamon
				|  Queso
				|  Salsa


data Pizza = Prepizza -- caso base
		|    Capa Ingrediente Pizza  -- caso inductivo

-- def conjunto
-- Definición del conjunto Pizza, con las siguientes reglas
-- regla base) Prepizza está en el conjunto Pizza
-- regla inductiva) si i está en el conjunto ingrediente y p está en el conjunto pizza
--					entonces Capa i p está en el conjunto Pizza

Prepizza :: Pizza
Capa Jamon Prepizza :: Pizza 
Capa Jamon (Capa Salsa (Capa Queso Prepizza)) :: Pizza

-- definción recursión estructural
-- f Prepizza    = ...
-- f (Capa i pz) = ... f pz

cantCapas :: Pizza -> Int
cantCapas Prepizza    = ...
cantCapas (Capa i pz) = 1 + cantCapas pz

-- 

cantAceitunas :: Pizza -> Int
cantAceitunas Prepizza    = 0
cantAceitunas (Capa i pz) = 
	aceitunas i + cantAceitunas pz 

aceitunas :: Ingrediente -> Int
aceitunas (Aceitunas n) = n 
aceitunas _ = 0

-- 

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza    = Prepizza
duplicarAceitunas (Capa i pz) = Capa (duplicar i) (duplicarAceitunas pz)

duplicar :: Ingrediente -> Ingrediente
duplicar (Aceitunas n) = Aceitunas (n * 2)
duplicar i = i

-- 

sinLactosa :: Pizza -> Pizza 
sinLactosa Prepizza    = Prepizza
sinLactosa (Capa i pz) = 
	capaSinLactosa i (sinLactosa pz)

capaSinLactosa :: Ingrediente -> Pizza
capaSinLactosa Queso pz = pz 
capaSinLactosa i pz     = Capa i pz
-- en caso de agregarse un ingrediente que tenga lactosa, se debe modificar (malo)

-- 
aptaIntoleranteLactosa :: Pizza -> Bool
aptaIntoleranteLactosa Prepizza = True
aptaIntoleranteLactosa (Capa i pz) = 
	esIntoleranteLactosa i && aptaIntoleranteLactosa pz


-- 

juntarAceitunasConsecutivas :: Pizza -> Pizza
juntarAceitunasConsecutivas Prepizza = ...
juntarAceitunasConsecutivas (Capa i pz) = 
	juntar i (juntarAceitunasConsecutivas pz)

juntar (Aceitunas n) (Capa (Aceitunas m) pz) = Capa (Aceitunas n + m) pz 
juntar i pz = Capa i pz 

-- practica
-- agruparConsecutivos  :: [Int] -> [[Int]]
-- agruparConsecutivos [1,1,2,2,2,3,1,1] = [[1,1], [2,2,2], [3], [1,1]]

-- 
juntarPizzas :: Pizza -> Pizza -> Pizza 
juntarPizzas Prepizza pz2 = pz2 
juntarPizzas (Capa i pz1) pz2 = Capa i (juntarPizzas pz1 pz2)

Para todo pz1, pz2. 
	cantCapas (juntarPizzas pz1 pz2) = cantCapas pz1 + cantCapas pz2

-- Voy a demostrar por inducción sobre la estructura Pizza

Sea pz cualquier Pizza.

Caso base) pz = Prepizza

TB) cantCapas (juntarPizzas Prepizza pz2) = cantCapas Prepizza + cantCapas pz2

Caso inductivo) pz = Capa i p 
HI) ¡cantCapas (juntarPizzas p pz2) = cantCapas p + cantCapas pz2!
TI) ¿cantCapas (juntarPizzas (Capa i p) pz2) = cantCapas (Capa i p) + cantCapas pz2?

-- resolucion
Caso base)
-- lado izq
cantCapas (juntarPizzas Prepizza pz2)
= -- def juntarPizzas
cantCapas pz2

-- lado der
cantCapas Prepizza + cantCapas pz2
= -- def cantCapas
0 + cantCapas pz2
= -- neutro de la suma
cantCapas pz2

Caso inductivo) 
-- lado izq
cantCapas (juntarPizzas (Capa i p) pz2)
= -- def juntarPizzas
cantCapas (Capa i (juntarPizzas p pz2))
= -- def cantCapas
1 + cantCapas (juntarPizzas p pz2)
= -- por HI
1 + cantCapas p + cantCapas pz2

-- lado der
cantCapas (Capa i p) + cantCapas pz2
= -- def cantCapas
1 + cantCapas p + cantCapas pz2



