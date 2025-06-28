type Programa = Robot -> Robot

data Robot = Robot {
    nombre :: String,
    nivelExperiencia :: Int,
    energia :: Int,
    programas :: [Programa]
}

-- Programas a implementar

recargaBateria :: Int -> Programa
recargaBateria n robot = robot { energia = energia robot + n }

descargaElectrica :: Programa
descargaElectrica robot
    | energia robot > 10 = robot { energia = energia robot - 10 }
    | otherwise          = robot { energia = energia robot `div` 2 }

olvidarPrograma :: Int -> Programa
olvidarPrograma n robot = robot { programas = drop n (programas robot) }

autoAtaque :: Programa
autoAtaque robot
    | null (programas robot) = error "no tiene programas"
    | otherwise              = head (programas robot) robot

-- 2.

poder :: Robot -> Int
poder robot = energia robot + nivelExperiencia robot * length (programas robot)

dano :: Robot -> Programa -> Int
dano robot programa 
    |energia robot == energia (programa robot) = 0
    | otherwise = energia robot - energia (programa robot)
    
diferenciaDePoder :: Robot -> Robot -> Int
diferenciaDePoder robot1 robot2 = abs (poder robot1 - poder robot2)

-- 3.

type Academia = [Robot]

existeAtlasSinProgramas :: Academia -> Bool
existeAtlasSinProgramas academia = any esAtlasSinProgramas academia
  where
    esAtlasSinProgramas robot = nombre robot == "Atlas" && null (programas robot)

todosViejosObstinados :: Academia -> Bool
todosViejosObstinados academia = all esObstinado robotsViejos
  where
    robotsViejos = filter (\r -> nivelExperiencia r > 16) academia
    esObstinado robot = length (programas robot) > 3 * experiencia robot

--Sobre la función auxiliar: (4)
--f x [y] = y
--f x (y1:y2:ys)
 --     | x y1 >= x y2 = f x (y1:ys)
 --     | otherwise = f x (y2 : ys)

--Explica brevemente cuál es su propósito, define su tipo y presenta una versión que sea más expresiva en el paradigma funcional.
--EXPLICACION:
--Esta función tiene como objetivo encontrar el elemento de una lista que, al aplicarle una función dada x, produce el valor máximo según la comparación. 
--En otras palabras, x es una función que transforma cada elemento de la lista, y la función f devuelve el elemento original cuya imagen bajo x es la mayor.
-- El proceso es recursivo: compara los primeros elementos y luego sigue evaluando el resto de la lista hasta encontrar el valor máximo.

-- Como se utiliza el operador ">=", podemos inferir que los valores deben ser comparables.
-- La función recibida como parámetro toma un valor de tipo A y devuelve uno de tipo B que debe ser comparable.
-- La lista contiene elementos del tipo A, ya que son los que se le pasan a la función.
-- Dado que el caso base retorna un elemento de la lista, el tipo de retorno de la función también es A.

valorMaximo :: Ord b => (a -> b) -> [a] -> a
valorMaximo funcion [valor] = valor
valorMaximo funcion (x : siguiente : xs)
      | funcion x >= funcion siguiente = valorMaximo funcion (x : xs)
      | otherwise = valorMaximo funcion (siguiente : xs)

--Sin definir funciones auxiliares, construye las siguientes:

mejorProgramaContra :: Robot -> Robot -> Programa
mejorProgramaContra victima atacante = foldl1 (\p1 p2 -> if danio victima p1 > danio victima p2 then p1 else p2) (programas atacante)
--Elige el programa del segundo robot que cause mayor reducción de energía al primero.

mejorOponente :: Robot -> Academia -> Robot
mejorOponente robot academia = foldl1 (\r1 r2 -> if diferenciaDePoder robot r1 > diferenciaDePoder robot r2 then r1 else r2) academia
--Encuentra el robot con la mayor diferencia de poder respecto al robot recibido.

