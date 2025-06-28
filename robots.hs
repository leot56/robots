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
poder robot = energia robot + experiencia robot * length (programas robot)

dano :: Robot -> Programa -> Int
dano robot programa = energia robot - energia (programa robot)

diferenciaDePoder :: Robot -> Robot -> Int
diferenciaDePoder robot1 robot2 = abs (poder robot1 - poder robot2)

