-- Llibreries necessaries ---------------------------

import Data.List
import Data.Char
import Data.List.Split
import System.IO (hFlush, stdout)

-- Tipus de dades ---------------------------

type Tauler2 = [[Casella]] -- representació per el tauler inicial

type Tauler = [Casella] -- representació del tauler real del joc

-- representa una casella del tauler
data Casella = Casella {peca:: Peca -- representa la peca de la casella
            , posicio :: Posicio -- representa la posicio de la casella en el tauler
            } deriving (Show,Eq)

data Peca = Peca Color Tipus deriving (Show, Eq) -- representa una peca del joc d'escacs
data Color = Blanc | Negre deriving (Show, Eq) -- representa el color d'una peca o blanc o negre
data Tipus = Peo | Cavall | Alfil | Torre | Dama | Rei deriving (Show, Eq) -- representa un tipus de peca d'escacs

-- representa una jugada de la partida
data Jugada = Jugada {pecaJugada :: Peca -- peca que realitza la jugada
            , posIni :: Posicio -- posicio inicial de la jugada
            , posFi :: Posicio -- posicio final de la jugada
            , esMata :: Bool -- inidica si durant la jugada es mata o no
            } deriving (Show, Eq)

type Posicio = (Int, Int) -- representa una posicio amb coordenada x i y

-- taulerInicial amb les posicions i peces inicials
taulerInicial = unlines ["tcadract",
                         "pppppppp",
                         "........",
                         "........",
                         "........",
                         "........",
                         "PPPPPPPP",
                         "TCADRACT"
                         ]

-- Funcions ---------------------------

-- Funcio per transformar un string a un tipus Tauler2
-- String -> representa taulerInicial que conté l'estat inicial del tauler
-- Tauler2 -> representa un tauler [[Casella]] amb les caselles inicials
llegirTauler :: String -> Tauler2
llegirTauler = map llegirFila . lines
  where llegirFila = map llegirCasella

-- metode per mostrar el tauler amb totes les peces i posicions
-- Tauler -> tauler actual
-- [Posicio] -> llista de totes les posicions del tauler
-- String -> string amb la sortida del tauler
mostrarTauler :: Tauler -> [Posicio] -> String
mostrarTauler t [] = ""
mostrarTauler t (pos:posicionsTauler) = ([(mostrarCasella t pos)] ++ mostrarTauler t posicionsTauler)

-- metode per mostrar el tauler segons la situació actual d'aquest
-- Tauler -> Tauler actual de la partida
-- IO -> retornem un output del tauler
mostrarTaulerActual :: Tauler -> IO()
mostrarTaulerActual tauler = putStr ((unlines (splitCaselles 8 (mostrarTauler tauler totesPosicions))))

-- metode per mostrar una casella
-- Tauler -> tauler amb totes les caselles
-- Posicio -> posicio de la casella en el tauler
-- Char -> caracter segons la casella que hi ha en la posició
mostrarCasella :: Tauler -> Posicio -> Char
mostrarCasella tauler pos =  if (hiHaPeca pos tauler) 
                        then mostrarPeca (getPeca tauler pos) 
                      else '.'

-- metode per llegir una casella segons un caracter
-- Char -> caracter a llegir
-- Casella -> retornem la casella segons el caracter que sigui
llegirCasella :: Char -> Casella
llegirCasella '.' = Casella {peca = (Peca Blanc Rei), posicio = (100,100)}
llegirCasella c = Casella {peca =(llegirPeca c), posicio = (0,0)}

-- metode per convertir una peca a un char
-- Peca -> peca que volem convertir
-- Char -> char que correspon a la peca
mostrarPeca :: Peca -> Char
mostrarPeca (Peca Blanc Peo)    = 'P'
mostrarPeca (Peca Blanc Cavall) = 'C'
mostrarPeca (Peca Blanc Alfil)  = 'A'
mostrarPeca (Peca Blanc Torre)  = 'T'
mostrarPeca (Peca Blanc Dama)   = 'D'
mostrarPeca (Peca Blanc Rei)    = 'R'
mostrarPeca (Peca Negre Peo)    = 'p'
mostrarPeca (Peca Negre Cavall) = 'c'
mostrarPeca (Peca Negre Alfil)  = 'a'
mostrarPeca (Peca Negre Torre)  = 't'
mostrarPeca (Peca Negre Dama)   = 'd'
mostrarPeca (Peca Negre Rei)    = 'r'

-- metode per saber que peca llegir
-- Char -> caracter que representa una peca
-- Peca -> peca que correspon al caracter
llegirPeca :: Char -> Peca
llegirPeca 'P' = (Peca Blanc Peo)
llegirPeca 'C' = (Peca Blanc Cavall)
llegirPeca 'A' = (Peca Blanc Alfil)
llegirPeca 'T' = (Peca Blanc Torre)
llegirPeca 'D' = (Peca Blanc Dama)
llegirPeca 'R' = (Peca Blanc Rei)
llegirPeca 'p' = (Peca Negre Peo)
llegirPeca 'c' = (Peca Negre Cavall)
llegirPeca 'a' = (Peca Negre Alfil)
llegirPeca 't' = (Peca Negre Torre)
llegirPeca 'd' = (Peca Negre Dama)
llegirPeca 'r' = (Peca Negre Rei)

-- metode per obtenir una peca del tauler a partir d'una posicio
-- Tauler -> tauler del que volem obtenir la peca
-- Posicio -> posicio de la que volem obtenir la peca del tauler
-- Peca -> peca que hi ha a la posicio del tauler
getPeca :: Tauler -> Posicio -> Peca
getPeca tauler pos = peca (head (filter (\p -> posicio p == pos) tauler))

-- metode per obtenir la llista de posicions del tauler
-- Tauler -> tauler del que volem obtenir les posicions
-- [Posicio] -> llista de posicions que tenim en el tauler actualment
getPosicionsTauler :: Tauler -> [Posicio]
getPosicionsTauler tauler = [posicio (tauler !! i) | i <- [0..(length tauler)-1]]

-- metode per saber si en una posicio del tauler hi ha peca o no
-- Posicio -> posicio on volem comprovar si hi ha peca
-- Tauler -> tauler del que volem saber si hi ha peca
-- Bool -> retorna cert si hi ha peca a la posicio en el tauler, fals altrament
hiHaPeca :: Posicio -> Tauler -> Bool
hiHaPeca posicio tauler = posicio `elem` (getPosicionsTauler tauler)

-- metode per saber si una posicio és valida o no en el tauler
-- Int -> coordenada x de la posicio
-- Int -> coordenada y de la posicio
-- Bool -> cert si es valida entre 0 i 7 les coordenades
validInTheBoard :: Int -> Int -> Bool
validInTheBoard x y = x >= 0 && x <= 7 && y <= 7 && y >= 0

-- metode per saber si una posicio és valida o no en el tauler
-- Posicio -> coordenadades x i y de la posicio
-- Bool -> cert si es valida entre 0 i 7 les coordenades
validInTheBoard2 :: Posicio -> Bool
validInTheBoard2 (x, y) = x >= 0 && x <= 7 && y <= 7 && y >= 0

-- metode per saber si una peca es un peo
-- Peca -> peca que volem saber si es un peo
-- Bool -> cert si la peca es un peo, fals altrament
esPeo :: Peca -> Bool
esPeo peca = peca == (Peca Blanc Peo) || peca == (Peca Negre Peo)

-- metode per saber si una peca es un rei
-- Peca -> peca que volem saber si es un rei
-- Bool -> cert si la peca es un rei, fals altrament
esRei :: Peca -> Bool
esRei peca = peca == (Peca Blanc Rei) || peca == (Peca Negre Rei)

-- metode per saber si una peca es una torre
-- Peca -> peca que volem saber si es una torre
-- Bool -> cert si la peca es una torre, fals altrament
esTorre :: Peca -> Bool
esTorre peca = peca == (Peca Blanc Torre) || peca == (Peca Negre Torre)

-- metode per saber si una peca es un alfil
-- Peca -> peca que volem saber si es un alfil
-- Bool -> cert si la peca es un alfil, fals altrament
esAlfil :: Peca -> Bool
esAlfil peca = peca == (Peca Blanc Alfil) || peca == (Peca Negre Alfil)

-- metode per saber si una peca es una dama
-- Peca -> peca que volem saber si es una dama
-- Bool -> cert si la peca es una dama, fals altrament
esDama :: Peca -> Bool
esDama peca = peca == (Peca Blanc Dama) || peca == (Peca Negre Dama)

-- metode per saber si una peca es un cavall
-- Peca -> peca que volem saber si es un cavall
-- Bool -> cert si la peca es un cavall, fals altrament
esCavall :: Peca -> Bool
esCavall peca = peca == (Peca Blanc Cavall) || peca == (Peca Negre Cavall)

-- metode per saber les posicions que pot fer una peca en una posicio del tauler buit
-- Peca -> peca de la que volem obtenir les posicions que pot anar
-- Posicio -> posicio des de que volem obtenir les posicions
-- [Posicio] -> llista de les posicions on pot anar la peca des de la posicio
moviment :: Peca -> Posicio -> [Posicio]
moviment peca posicio = 
        if esPeo peca
            then movimentsPossiblesPeo Casella{peca = peca, posicio = posicio}
        else if esRei peca
            then movimentsPosiblesRei Casella {peca = peca, posicio = posicio} movimentPosicioRei
        else if esTorre peca
            then movimentsPosiblesTorre Casella {peca = peca, posicio = posicio}
        else if esAlfil peca
            then movimentsPosiblesAlfil Casella {peca = peca, posicio = posicio}
        else if esDama peca
            then movimentsPosiblesDama Casella {peca = peca, posicio = posicio}
        else movimentsPosiblesCavall Casella {peca = peca, posicio = posicio}

-- metode per saber si hi ha algu en les posicions del tauler
-- Tauler -> tauler d'on volem saber si en les posicions hi ha algu
-- [Posicio] -> llista de posicions de les que volem saber si en alguna hi ha alguna peca
-- Bool -> cert si en alguna posicio hi ha una peca, fals altrament
alguEnLesPosicions :: Tauler -> [Posicio] -> Bool
alguEnLesPosicions tauler [] = False
alguEnLesPosicions tauler (x:xs) =
                    if (hiHaPeca x tauler)
                      then True
                    else alguEnLesPosicions tauler xs

-- metode per mirar si hi ha algu entre dues posicions d'un tauler
-- Tauler -> tauler del que volem mirar si hi ha algu entre
-- Posicio -> posicio inici
-- Posicio -> posicio final
-- Bool -> cert si hi ha algu entre la posicio inici i posicio final en el tauler, fals altrament
alguEntre :: Tauler -> Posicio -> Posicio -> Bool
alguEntre tauler pos1 pos2
  | fst(pos1) == fst(pos2) = miraEntreCasellesHoritzontal tauler pos1 pos2 --  "Horitzontal" 
  | snd(pos1) == snd(pos2) = miraEntreCasellesVertical tauler pos1 pos2 -- Vertical 
  | abs(fst(pos1)-fst(pos2)) == abs(snd(pos1)-snd(pos2)) = miraEntreCasellesDiagonals tauler pos1 pos2 --"Diagonal"
  | otherwise = error("moviment invalid")

-- mira si hi ha algu en les diagonals del tauler entre posicions
-- Tauler -> tauler del que volem mirar si hi ha algu entre diagonals
-- Posicio -> posicio inici
-- Posicio -> posicio final
-- Bool -> cer si hi ha algu entre la posicio inici i posicio final en diagonal del tauler, fals altrament 
miraEntreCasellesDiagonals :: Tauler -> Posicio -> Posicio -> Bool
miraEntreCasellesDiagonals t pos1 pos2
  | (fst(pos2) > fst(pos1)) && (snd(pos2) > snd(pos1)) = alguEnLesPosicions t [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) > (fst(pos1))) && ((snd(posicions)) > (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) < fst(pos2) && snd(posicions) < snd(pos2)]
  | (fst(pos1) > fst(pos2)) && (snd(pos1) > snd(pos2)) = alguEnLesPosicions t [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) < (fst(pos1))) && ((snd(posicions)) < (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) > fst(pos2) && snd(posicions) > snd(pos2)]
  | (fst(pos2) < fst(pos1)) && (snd(pos2) > snd(pos1)) = alguEnLesPosicions t [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) < (fst(pos1))) && ((snd(posicions)) > (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) > fst(pos2) && snd(posicions) < snd(pos2)]
  | (fst(pos2) > fst(pos1)) && (snd(pos2) < snd(pos1)) = alguEnLesPosicions t [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) > (fst(pos1))) && ((snd(posicions)) < (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) < fst(pos2) && snd(posicions) > snd(pos2)]
  | otherwise = False

-- mira si hi ha algu en la vertical del tauler entre posicions
-- Tauler -> tauler del que volem mirar si hi ha algu en la vertical
-- Posicio -> posicio inici
-- Posicio -> posicio final
-- Bool -> cer si hi ha algu entre la posicio inici i posicio final en vertical del tauler, fals altrament 
miraEntreCasellesVertical:: Tauler -> Posicio -> Posicio -> Bool
miraEntreCasellesVertical t pos1 pos2 = alguEnLesPosicions t [(x,y) |  x <- [fst(posicio1)+1..fst(posicio2)-1], y <- [snd(posicio1)], x < fst(posicio2)]
  where
    posicio1 = if (fst(pos1) > fst(pos2)) then pos2 else pos1
    posicio2 = if (fst(pos1) > fst(pos2)) then pos1 else pos2

-- mira si hi ha algu en horitzontal del tauler entre posicions
-- Tauler -> tauler del que volem mirar si hi ha algu en la horitzontal
-- Posicio -> posicio inici
-- Posicio -> posicio final
-- Bool -> cer si hi ha algu entre la posicio inici i posicio final en horitzontal del tauler, fals altrament 
miraEntreCasellesHoritzontal :: Tauler -> Posicio -> Posicio -> Bool
miraEntreCasellesHoritzontal t pos1 pos2 = alguEnLesPosicions t [(y,x) |  x <- [snd(posicio1)+1..snd(posicio2)-1], y <- [fst(posicio1)], x < snd(posicio2)]
  where
    posicio1 = if (snd(pos1) > snd(pos2)) then pos2 else pos1
    posicio2 = if (snd(pos1) > snd(pos2)) then pos1 else pos2

-- metode per saber els possibles moviments d'un peo
-- Casella -> casella on es troba el peo actualment
-- [Posicio] -> llista de possibles posicions on pot moure's el peo
movimentsPossiblesPeo :: Casella -> [Posicio]
movimentsPossiblesPeo casella = case validInTheBoard (fst(posicio casella)) (snd(posicio casella)) of
  False -> []
  True -> if (peca casella) == (Peca Blanc Peo)
          then if (fst(posicio casella)) == 6
                    then [((fst(posicio casella)-1), snd(posicio casella))] ++ [((fst(posicio casella)-2), snd(posicio casella))] 
                    else [((fst(posicio casella)-1), snd(posicio casella))]
          else 
            if (fst(posicio casella)) == 1
                    then [((fst(posicio casella)+1), snd(posicio casella))] ++ [((fst(posicio casella)+2), snd(posicio casella))] 
                    else [((fst(posicio casella)+1), snd(posicio casella))]

-- metode per saber si una peca es blanca
-- Peca -> peca que volem saber si es blanca
-- Bool -> cert si la peca es blanca, fals altrament
esBlanca :: Peca -> Bool
esBlanca peca = peca == (Peca Blanc Peo) || peca == (Peca Blanc Torre) || peca == (Peca Blanc Cavall) || peca == (Peca Blanc Alfil) || peca == (Peca Blanc Dama) || peca == (Peca Blanc Rei)

-- metode per saber si una peca es negre
-- Peca -> peca que volem saber si es negre
-- Bool -> cert si la peca es negre, fals altrament
esNegre :: Peca -> Bool
esNegre peca = peca == (Peca Negre Peo) || peca == (Peca Negre Torre) || peca == (Peca Negre Cavall) || peca == (Peca Negre Alfil) || peca == (Peca Negre Dama) || peca == (Peca Negre Rei)

-- metode per saber els possibles moviments d'un rei
-- Casella -> casella on es troba el rei actualment
-- [Posicio] -> llista de possibles posicions on pot moure's el rei
movimentsPosiblesRei :: Casella -> [Posicio] -> [Posicio]
movimentsPosiblesRei casella [] = []
movimentsPosiblesRei casella (x:xs) =  if(validInTheBoard2 (sumaPosicions (posicio casella) x)) 
                                       then [sumaPosicions (posicio casella) x ] ++ movimentsPosiblesRei casella xs
                                       else [] ++ movimentsPosiblesRei casella xs
                                    
-- metode per saber els possibles moviments d'una torre
-- Casella -> casella on es troba la torre actualment
-- [Posicio] -> llista de possibles posicions on pot moure's la torre
movimentsPosiblesTorre :: Casella -> [Posicio]
movimentsPosiblesTorre casella = [(x,y) | x <- [0..7], y <- [0..7], x == fst(posicio casella) || y == snd(posicio casella), (x /= fst(posicio casella) || y /= snd(posicio casella))]

-- metode per saber els possibles moviments d'un alfil
-- Casella -> casella on es troba l'alfil actualment
-- [Posicio] -> llista de possibles posicions on pot moure's l'alfil
movimentsPosiblesAlfil :: Casella -> [Posicio]
movimentsPosiblesAlfil casella = [((fst(posicio casella)) + x, (snd(posicio casella)) + y) | x <- [0..7] , y <- [0..7] , x == y, validInTheBoard2 ((fst(posicio casella)) + x , (snd(posicio casella)) + y), ((fst(posicio casella) + x) /= fst(posicio casella) || (snd(posicio casella)) + y /= snd(posicio casella))] ++
                                 [((fst(posicio casella)) - x, (snd(posicio casella)) - y) | x <- [0..7] , y <- [0..7] , x == y, validInTheBoard2 ((fst(posicio casella)) - x , (snd(posicio casella)) - y), ((fst(posicio casella) - x) /= fst(posicio casella) || (snd(posicio casella)) - y /= snd(posicio casella))] ++
                                 [((fst(posicio casella)) + x, (snd(posicio casella)) - y) | x <- [0..7] , y <- [0..7] , x == y, validInTheBoard2 ((fst(posicio casella)) + x , (snd(posicio casella)) - y), ((fst(posicio casella) + x) /= fst(posicio casella) || (snd(posicio casella)) - y /= snd(posicio casella))] ++
                                 [((fst(posicio casella)) - x, (snd(posicio casella)) + y) | x <- [0..7] , y <- [0..7] , x == y, validInTheBoard2 ((fst(posicio casella)) - x , (snd(posicio casella)) + y), ((fst(posicio casella) - x) /= fst(posicio casella) || (snd(posicio casella)) + y /= snd(posicio casella))] 

-- metode per saber els possibles moviments d'una dama
-- Casella -> casella on es troba la dama actualment
-- [Posicio] -> llista de possibles posicions on pot moure's la dama
movimentsPosiblesDama :: Casella -> [Posicio]
movimentsPosiblesDama casella = (movimentsPosiblesTorre Casella {peca = (Peca Blanc Torre), posicio = posicio casella}) ++ (movimentsPosiblesAlfil Casella {peca = (Peca Blanc Alfil), posicio = posicio casella})

-- metode per saber els possibles moviments d'un cavall
-- Casella -> casella on es troba el cavall actualment
-- [Posicio] -> llista de possibles posicions on pot moure's el cavall
movimentsPosiblesCavall :: Casella -> [Posicio]
movimentsPosiblesCavall casella = [(fst(posicio casella)+(fst(aux)),(snd(posicio casella))+(snd(aux))) | aux <- [(x,y) | x <- [-2..2], y <- [-2..2], abs(x) /= abs(y), x /=0, y /=0], validInTheBoard (fst(posicio casella)+(fst(aux))) ((snd(posicio casella))+(snd(aux)))]

-- realitza una jugada 
-- Tauler -> tauler actual
-- Jugada -> jugada que és vol realitzar en el tauler actual
-- Tauler -> tauler un cop realitzada la jugada
fesJugada :: Tauler -> Jugada -> Tauler
fesJugada tauler jugada = if (jugadaLegal tauler jugada) then (mourePeca tauler jugada) else error ("jugada invalida")

-- metode per moure una peca d'un lloc a un altre del tauler
-- Tauler -> tauler actual
-- Jugada -> jugada que es vol realitzar
-- Tauler -> tauler amb la peca moguda després de fer la jugada
mourePeca :: Tauler -> Jugada -> Tauler
mourePeca tauler jugada = if (existeixPosicio tauler (posFi jugada))
                          then delete (Casella{peca = (getPeca tauler (posFi jugada)) , posicio = (posFi jugada)}) (delete (Casella{peca = (pecaJugada jugada), posicio = (posIni jugada)}) tauler ++ [Casella{peca = (pecaJugada jugada), posicio = (posFi jugada)}])
                          else delete (Casella{peca = (pecaJugada jugada), posicio = (posIni jugada)}) tauler ++ [Casella{peca = (pecaJugada jugada), posicio = (posFi jugada)}]

-- metode per saber si existeix una posicio en el tauler
-- Tauler -> tauler amb totes les posicions
-- Posicio -> posicio que volem mirar si existeix
-- Bool -> cert si existeix la posicio, fals altrament
existeixPosicio :: Tauler -> Posicio -> Bool
existeixPosicio [] pos = False
existeixPosicio (x:xs) pos = if((posicio x) == pos) then True else existeixPosicio xs pos

-- metode per saber si una jugada és legal o no
-- Tauler -> tauler actual
-- Jugada -> jugada que es vol realitzar en el tauler
-- Bool -> si la jugada es valida retorna cert, fals altrament
jugadaLegal :: Tauler -> Jugada -> Bool
jugadaLegal tauler jugada = if not (hiHaPeca (posIni jugada) tauler) then False
                            else if ((not $ (esMata jugada)) && (not $ (hiHaPeca (posFi jugada) tauler))) -- cas fet
                                   then ((posFi jugada) `elem` moviment (pecaJugada jugada) (posIni jugada)) && (esCavall (pecaJugada jugada) || (not (alguEntre tauler (posIni jugada) (posFi jugada))))
                            else if (not $ esPeo (pecaJugada jugada)) && (esMata jugada) && (hiHaPeca (posFi jugada) tauler) && (esNegre (pecaJugada jugada) /= esNegre (getPeca tauler (posFi jugada))) 
                                    then ((posFi jugada) `elem` (moviment (pecaJugada jugada) (posIni jugada))) && (esCavall (pecaJugada jugada) || (not (alguEntre tauler (posIni jugada) (posFi jugada))))
                            else if (esPeo (pecaJugada jugada) && (esMata jugada) && (hiHaPeca (posFi jugada) tauler) && (esNegre (pecaJugada jugada) /= esNegre (getPeca tauler (posFi jugada))))
                                    then
                                      if (esBlanca (pecaJugada jugada))
                                        then ((fst(posIni jugada)-1,snd(posIni jugada)-1) == (posFi jugada)) || (( fst(posIni jugada)-1,snd(posIni jugada)+1 ) == (posFi jugada)) 
                                      else ((fst(posIni jugada)+1,snd(posIni jugada)-1) == (posFi jugada)) || (( fst(posIni jugada)+1,snd(posIni jugada)+1 ) == (posFi jugada))
                            else False

-- metode per sumar dos posicions
-- Posicio -> posicio1
-- Posicio -> posicio2
-- Posicio -> retornem la posicio de la suma de les dues posicions
sumaPosicions :: Posicio -> Posicio -> Posicio
sumaPosicions x y = ((fst(x) + fst(y)) , (snd(x) + snd(y)))

-- metode per crear una casella amb la peca d'una casella i una posicio
-- Casella -> casella per agafar-ne la peca
-- Posicio -> posicio que volem que tingui la casella
-- Casella -> casella que retornem amb la peca de la casella i la posicio
posaPosicio :: Casella -> Posicio -> Casella
posaPosicio cas posicio = Casella{peca = peca cas, posicio = posicio}

-- metode per obtenir totes les peces del tauler que siguin d'un color
-- Tauler -> tauler del que volem obtenir les peces
-- Color -> color del que volem filtrar les peces
-- Tauler -> peces que són del color que hem dit
pecesDunColor :: Tauler -> Color -> Tauler
pecesDunColor t col = case (col == Blanc) of
  True -> [cas | cas <- t, esBlanca (peca cas)]
  False -> [cas | cas <- t, esNegre (peca cas)]


-- metode per saber si no hi ha algu entre el rei
-- Tauler -> tauler actual
-- Casella -> casella on hi ha el rei
-- Tauler -> caselles del color contrari del rei
-- Bool -> cert si no hi ha algu entre el rei, fals altrament
noHiHaAlguEntreRei :: Tauler -> Casella -> Tauler -> Bool
noHiHaAlguEntreRei _ _ [] = False
noHiHaAlguEntreRei tauler casellaRei (x:pecesAltreColor) = if((jugadaLegal tauler Jugada{pecaJugada = (peca x), posIni = (posicio x), posFi = (posicio casellaRei), esMata = True})) then True else noHiHaAlguEntreRei tauler casellaRei pecesAltreColor


-- metode per fer split d'una llista a un conjunt de llistes
-- Int -> parametre de quantes subllistes es volen 
-- [a] -> llista d'entrada
-- [[a]] -> llista final
splitCaselles :: Int -> [a] -> [[a]]
splitCaselles _ [] = []
splitCaselles n xs = as : splitCaselles n bs
  where (as,bs) = splitAt n xs

-- metode per obtenir la posició d'una peca (quan només n'hi ha una p.ex rei o dama)
-- Tauler -> tauler del qual volem obtenir la posició de la peca
-- Peca -> peca de la que volem obtenir la posició
-- Posicio -> posicio de la peca en el tauler
getPosicio :: Tauler -> Peca -> Posicio
getPosicio [] _ = (0,0)
getPosicio (x:t) peca2 = if ((peca x) == peca2) then posicio x else getPosicio t peca2

-- metode per saber si un color esta amenaçat d'escac en el tauler
-- Tauler -> tauler del que volem saber si el color esta amenaçat
-- Color -> color que volem saber si esta amenaçat
-- Bool -> cert si el color esta amenaçat d'escac en el tauler, fals altrament
escac :: Tauler -> Color -> Bool
escac t col = if (col == Blanc)
                then noHiHaAlguEntreRei t Casella{peca = (Peca Blanc Rei), posicio = getPosicio t (Peca Blanc Rei)} (pecesDunColor t Negre)
              else noHiHaAlguEntreRei t Casella{peca = (Peca Negre Rei), posicio = getPosicio t (Peca Negre Rei)} (pecesDunColor t Blanc)

-- metode per saber si un color esta amenaçat amb escac i mat en el tauler
-- Tauler -> tauler del que volem saber si el color esta amenaçat
-- Color -> color del que volem saber si esta amenaçat
-- Casella -> casella que en principi fa escac i mat
-- Bool -> cert si la casella fa escac i mat al color del tauler
escacMat :: Tauler -> Color -> Peca -> Posicio -> Bool
escacMat tauler color pecaQueFaEscac posicioPecaQueFaEscac = (escac tauler color) && (not (potFugirRei tauler color)) && (not (teCoberturaRei tauler pecaQueFaEscac posicioPecaQueFaEscac color)) && (not(esPotMatarPecaSenseEscac tauler pecaQueFaEscac posicioPecaQueFaEscac (pecesDunColor tauler color)))

-- metode per saber si el rei té cobertura per part de les seves peces
-- Tauler -> tauler actual
-- Casella -> Casella que fa escac al rei
-- Color -> Color del rei que volem comprovar si hi ha cobertura
-- Bool -> cert si te cobertura per part d'una peca del meu color, fals altrament 
teCoberturaRei :: Tauler -> Peca -> Posicio -> Color -> Bool
teCoberturaRei tauler pecaQueFaEscac posicioPecaQueFaEscac color = if (esCavall pecaQueFaEscac)
                                                                   then False
                                                                   else existeixMovimentQueEmSalvi tauler (generaPosicionsEntre tauler (getPosicio tauler (Peca color Rei)) posicioPecaQueFaEscac) (pecesDunColor tauler color)


-- metode per saber si existeix algun moviment que em salvi
-- Tauler -> tauler actual
-- [Posicio] -> llista de posicions entre
-- Tauler -> peces del mateix color
-- Bool -> cert si hi ha un moviment que hem salvi, fals altrament
existeixMovimentQueEmSalvi :: Tauler -> [Posicio] -> Tauler -> Bool
existeixMovimentQueEmSalvi _ [] _ = False
existeixMovimentQueEmSalvi tauler (x:posicionsEntre) pecesMeves = if (hihaUnaPecaMevaQuePotAnar tauler x pecesMeves)
                                                                  then True
                                                                  else existeixMovimentQueEmSalvi tauler posicionsEntre pecesMeves


-- metode per saber si hi ha una peca del meu color que pot anar a una posició
-- Tauler -> tauler actual
-- Posicio -> posicio a la que vull anar
-- Tauler -> peces del meu color
-- Bool -> cert si hi ha alguna peca del meu color que pot anar a la posicio, fals altrament
hihaUnaPecaMevaQuePotAnar :: Tauler -> Posicio -> Tauler -> Bool
hihaUnaPecaMevaQuePotAnar _ _ [] = False
hihaUnaPecaMevaQuePotAnar tauler posEntre (x:pecesMeves) = if (jugadaLegal tauler Jugada{pecaJugada = (peca x), posIni = (posicio x), posFi = posEntre ,esMata = False})
                                                           then True 
                                                           else hihaUnaPecaMevaQuePotAnar tauler posEntre pecesMeves


-- metode per generar totes les posicions entre dues posicions donades
-- Tauler -> tauler actual
-- Posicio -> posicio1
-- Posicio -> posicio2
-- [Posicio] -> llista de posicions entre la posicio1 i la posicio2
generaPosicionsEntre :: Tauler -> Posicio -> Posicio -> [Posicio]
generaPosicionsEntre tauler pos1 pos2
  | fst(pos1) == fst(pos2) = retornaCasellesHoritzontal tauler pos1 pos2 --  "Horitzontal" 
  | snd(pos1) == snd(pos2) = retornaCasellesVertical tauler pos1 pos2 -- Vertical 
  | abs(fst(pos1)-fst(pos2)) == abs(snd(pos1)-snd(pos2)) = retornaCasellesDiagonals tauler pos1 pos2 --"Diagonal"
  | otherwise = []


-- metode per retornar les caselles diagonales entre dues posicions
-- Tauler -> tauler actual
-- Posicio -> posicio1
-- Posicio -> posicio2
-- [Posicio] -> llista de posicions en diagonal entre la posicio1 i la posicio2
retornaCasellesDiagonals :: Tauler -> Posicio -> Posicio -> [Posicio]
retornaCasellesDiagonals t pos1 pos2
  | (fst(pos2) > fst(pos1)) && (snd(pos2) > snd(pos1)) = [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) > (fst(pos1))) && ((snd(posicions)) > (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) < fst(pos2) && snd(posicions) < snd(pos2)]
  | (fst(pos1) > fst(pos2)) && (snd(pos1) > snd(pos2)) = [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) < (fst(pos1))) && ((snd(posicions)) < (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) > fst(pos2) && snd(posicions) > snd(pos2)]
  | (fst(pos2) < fst(pos1)) && (snd(pos2) > snd(pos1)) = [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) < (fst(pos1))) && ((snd(posicions)) > (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) > fst(pos2) && snd(posicions) < snd(pos2)]
  | (fst(pos2) > fst(pos1)) && (snd(pos2) < snd(pos1)) = [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) > (fst(pos1))) && ((snd(posicions)) < (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) < fst(pos2) && snd(posicions) > snd(pos2)]
  | otherwise = []


-- metode per retornar les caselles verticals entre dues posicions
-- Tauler -> tauler actual
-- Posicio -> posicio1
-- Posicio -> posicio2
-- [Posicio] -> llista de posicions en vertical entre la posicio1 i la posicio2
retornaCasellesVertical:: Tauler -> Posicio -> Posicio -> [Posicio]
retornaCasellesVertical t pos1 pos2 = [(x,y) |  x <- [fst(posicio1)+1..fst(posicio2)-1], y <- [snd(posicio1)], x < fst(posicio2)]
  where
    posicio1 = if (fst(pos1) > fst(pos2)) then pos2 else pos1
    posicio2 = if (fst(pos1) > fst(pos2)) then pos1 else pos2

-- metode per retornar les caselles horitzontals entre dues posicions
-- Tauler -> tauler actual
-- Posicio -> posicio1
-- Posicio -> posicio2
-- [Posicio] -> llista de posicions en horitzontal entre la posicio1 i la posicio2
retornaCasellesHoritzontal :: Tauler -> Posicio -> Posicio -> [Posicio]
retornaCasellesHoritzontal t pos1 pos2 = [(y,x) |  x <- [snd(posicio1)+1..snd(posicio2)-1], y <- [fst(posicio1)], y < snd(posicio2)]
  where
    posicio1 = if (snd(pos1) > snd(pos2)) then pos2 else pos1
    posicio2 = if (snd(pos1) > snd(pos2)) then pos1 else pos2


-- mètode que retorna cert si es pot matar la peça que et fa escac amb una de les teves peces i ja no està en escac. fals altrament
-- Tauler -> representa el tauler actual on hi ha escac
-- Peca -> representa la peça que fa escac al contrincant
-- Posicio -> representa la posicio d'on es troba la peça que fa escac
-- Tauler -> representa el conjunt de les peces que poden matar a la peça que et fa escac
-- Bool -> retornara cert si es pot matar la peça i dexar d'estar en escac, fals altrament
esPotMatarPecaSenseEscac :: Tauler -> Peca -> Posicio -> Tauler -> Bool
esPotMatarPecaSenseEscac _ _ _ [] = False
esPotMatarPecaSenseEscac tauler pecaQueFaEscac posicioPecaQueFaEscac (x:pecesDunColor) = if jugadaLegal tauler (Jugada{pecaJugada = peca x, posIni = posicio x, posFi = posicioPecaQueFaEscac, esMata = True}) && not(escac (mourePeca tauler Jugada{pecaJugada = peca x, posIni = posicio x, posFi = posicioPecaQueFaEscac, esMata = True}) color)
                                                                   then True
                                                                   else esPotMatarPecaSenseEscac tauler pecaQueFaEscac posicioPecaQueFaEscac pecesDunColor
                                                                     where color = if(esNegre (peca x)) then Negre else Blanc


-- metode per saber si el rei pot anar a una posicio on no sigui escac o si pot matar a algu per deixar d'estar a escac
-- Tauler -> tauler actual
-- Color -> color del rei
-- Bool -> cert si pot fugir el rei, fals altrament
potFugirRei :: Tauler -> Color -> Bool
potFugirRei tauler color = (esPotMoureRei tauler (moviment (Peca color Rei) posicioDeLaPeca) color) || (elReiPotMatar tauler (moviment (Peca color Rei) posicioDeLaPeca) color)
  where posicioDeLaPeca = getPosicio tauler (Peca color Rei)

-- metode per saber si el rei té una posició on moure's per deixar d'estar a escac
-- Tauler -> tauler actual
-- [Posicio] -> llista de posicions on es pot moure el rei en un tauler buit
-- Color -> color del rei
-- Bool -> cert si té una posicio on moure's sense escac, fals altrament
esPotMoureRei :: Tauler -> [Posicio] -> Color -> Bool
esPotMoureRei _ [] _ = False
esPotMoureRei tauler (x:movimentsPossibles) color = if((not (hiHaPeca x tauler)) && not(escac (mourePeca tauler Jugada{pecaJugada = (Peca color Rei), posIni = getPosicio tauler (Peca color Rei), posFi = x, esMata = False}) color))
                                                    then True
                                                    else esPotMoureRei tauler movimentsPossibles color

-- metode per saber si el rei pot matar a algu sense estar a escac
-- Tauler -> tauler actual
-- [Posicio] -> llista de posicions on es pot moure el rei en un tauler buit
-- Color -> color del rei
-- Bool -> cert si el rei pot matar a algu sense estar a escac, fals altrament
elReiPotMatar :: Tauler -> [Posicio] -> Color -> Bool
elReiPotMatar _ [] _ = False
elReiPotMatar tauler (x:movimentsPossibles) color = if((hiHaPeca x tauler) && (posXesColorContrari tauler x color) && not(escac (mourePeca tauler Jugada{pecaJugada = (Peca color Rei), posIni = getPosicio tauler (Peca color Rei), posFi = x, esMata = True}) color))
                                                    then True
                                                    else elReiPotMatar tauler movimentsPossibles color

-- metode per saber si la peca d'una posicio és contraria a un color
-- Tauler -> tauler actual
-- Posicio -> posicio de la peca que volem mirar si es color contrari
-- Color -> color que volem saber si es color contrari
-- Bool -> cert si la peca de la posicio és del color contrari que color, fals altrament
posXesColorContrari :: Tauler -> Posicio -> Color -> Bool
posXesColorContrari tauler posicio color = (esNegre (getPeca tauler posicio) && color == Blanc) ||  (esBlanca (getPeca tauler posicio) && color == Negre)

tauler = llegirTauler taulerInicial -- Tauler d'inici

movimentPosicioRei = [(x,y) | x <- [-1..1], y <- [-1..1], (x /= 0 || y /= 0)]

movimentPosicioTorre = [(0,1), (1,0), (0,-1), (-1,0)]

totesPosicions = [(x,y) | x <- [(0::Int)..(7::Int)], y <- [(0::Int)..(7::Int)]] -- totes les posicions possibles del tauler

posicions = [(x,y) | x <- [0..1]++[6..7], y <- [0..7]]

taulerFinal = [posaPosicio casella posicio | posicio <- posicions, casella <- [(tauler !! fst(posicio) !! snd(posicio))]] -- Tauler real


-- mètode que donat un tauler i un string llegeix el moviment d'una jugada, aquest moviment pot matar o no
-- Tauler -> representa el tauler actual del tauler
-- String -> representa la jugada es format string, que pot matar a una altra peça o no
-- Tauler -> representa el tauler final amb la jugada implementada
llegirMoviment :: Tauler -> String -> Tauler
llegirMoviment tauler input = if ((input !! 3) == 'x') then  mataPeca tauler input else ferMoviment tauler input

-- mètode que donat un char retorna la respectiva peça
-- Char -> representa la peça en format char
-- Peca -> representa la peça que s'ha creat a partir del char
convertCharToPeca :: Char -> Peca
convertCharToPeca c = llegirPeca c

-- mètode que donat dos char, columna i fila, retorna la posicio respectiva al nostre tauler
-- Char -> representa la columna del tauler en format char
-- Char -> representa la fila del tauler en format char
-- Posicio -> representa la posicio que es crea a partir dels paràmetres anteriors
convertPosicio :: Char -> Char -> Posicio
convertPosicio columna fila = (abs((digitToInt fila) - 8), (ord columna) - 97) 

-- mètode que donat un string crea una nova jugada, amb les seves respectives dades
-- String -> representa la jugada en format string, tal com s'explica en l'enunciat
-- Jugada -> representa la jugada que es crea a partir de string donat
crearJugada :: String -> Jugada
crearJugada input = if ((input !! 3) == 'x') then jugadaMatant else juagadaSenseMatar
                      where jugadaMatant = Jugada{pecaJugada = p, posIni = posicioInicial, posFi = posicioFinal1, esMata = True}
                            posicioFinal1 = convertPosicio (input !! 4) (input !! 5)
                            juagadaSenseMatar = Jugada{pecaJugada = p, posIni = posicioInicial, posFi = posicioFinal2, esMata = False}
                            p = convertCharToPeca (input !! 0)
                            posicioInicial = convertPosicio (input !! 1) (input !! 2)
                            posicioFinal2 = convertPosicio (input !! 3) (input !! 4)

-- mètode que donat un tauler i un string retorna un altra tauler amb la jugada feta, aquest mètode mou una peça i es menja una altra peça
-- Tauler -> representa al tauler actual que s'ha de modificar
-- String -> representa la jugada que s'ha de fer en format string
-- Tauler -> representa el tauler final que retornarà, amb la jugada feta
mataPeca :: Tauler -> String -> Tauler
mataPeca tauler input =   (fesJugada tauler jugada)
                     where jugada = Jugada{pecaJugada = p, posIni = posicioInicial, posFi = posicioFinal, esMata = True}
                           p = convertCharToPeca (input !! 0)
                           posicioInicial = convertPosicio (input !! 1) (input !! 2)
                           posicioFinal = convertPosicio (input !! 4) (input !! 5)

-- mètode que donat un tauler i un string retorna un altra tauler amb la jugada feta, aquest mètode mou una peça, pero no es menja cap peça.
-- Tauler -> representa al tauler actual que s'ha de modificar
-- String -> representa la jugada que s'ha de fer en format string
-- Tauler -> representa el tauler final que retornarà, amb la jugada feta
ferMoviment :: Tauler -> String -> Tauler
ferMoviment tauler input = (fesJugada tauler jugada)
                      where jugada = Jugada{pecaJugada = p, posIni = posicioInicial, posFi = posicioFinal, esMata = False}
                            p = convertCharToPeca (input !! 0)
                            posicioInicial = convertPosicio (input !! 1) (input !! 2)
                            posicioFinal = convertPosicio (input !! 3) (input !! 4)

-- metode que retorna si una entrada es valida o no, cert si entren una o dues jugades, fals altrament
-- String -> representa l'entrada string que analitzarà
-- Bool -> retorna cert si l'entrada es valida, fals altrament
validInput :: String -> Bool
validInput str = if ((length (splitOn " " str) /= 1) && (length (splitOn " " str) /= 2))
                  then False
                 else True

-- metode per obtenir la columna de la posicio final, a partir d'un string
-- String -> entrada string d'on es vol obtenir la columna
-- Char -> char que retorna corresponent a la columna de la jugada final
getColumnaFinalJugada :: String -> Char
getColumnaFinalJugada entrada = if((entrada !! 3) == 'x') then (entrada !! 4) else (entrada !! 3) 

-- metode per obtenir la fila de la posicio final, a partir d'un string
-- String -> entrada string d'on es vol obtenir la fila
-- Char -> char que retorna corresponent a la fila de la jugada final
getFilaFinalJugada :: String -> Char
getFilaFinalJugada entrada = if((entrada !! 3) == 'x') then (entrada !! 5) else (entrada !! 4) 

-- main del programa principal d'escacs
-- Retorna Outputs del programa
main :: IO()
main = do
       putStrLn "Introdueix el nom del fitxer, si us plau."
       fitxer <- getLine
       s <- readFile fitxer
       let ls = lines s
       joc taulerFinal ls 0


-- mètode que implementa una jugada a partir d'un tauler
-- Tauler -> representa el tauler actual, on s'implementa el joc
-- [String] -> representa el conjunt de tirades que es van jugant en el tauler
-- Int -> representa la tirada que es jugarà en aquesta iteració
-- IO() -> retorna outputs per pantalla
joc :: Tauler -> [String] -> Int -> IO()
joc tauler lineas numLinea = do
  putStrLn "========"
  mostrarTaulerActual tauler
  putStrLn "========"
  putStrLn "abcdefgh"
  putStr "Entrada> "
  let entrada = lineas !! numLinea
  realitzaAccio tauler lineas entrada numLinea

-- metode per realitzar una accio a partir d'una entrada
-- Tauler -> tauler actual
-- [String] -> conjunt de tirades llegides del fitxer
-- String -> conté la tirada actual a realitzar
-- Int -> conté l'index de la tirada actual
-- IO() -> retorna outputs de l'acció a realitzar
realitzaAccio :: Tauler -> [String] -> String -> Int -> IO()
realitzaAccio _ _ "sortir" _ = putStrLn "Adeu gràcies per jugar"
realitzaAccio tauler lineas entrada numLinea = case entrada of
  "" -> putStrLn ("Entrada invalida " ++ entrada)
  otherwise -> if not $ validInput entrada -- si la entrada no es correcte
               then putStrLn ("Entrada invalida " ++ entrada)
                  else if ((length (splitOn " " entrada) == 1) && (jugadaLegal tauler (crearJugada ((splitOn " " entrada) !! 0))))
                     then do
                          let taulerAux = llegirMoviment tauler ((splitOn " " entrada) !! 0)
                          if(escacMat taulerAux Negre (convertCharToPeca (((splitOn " " entrada) !! 0) !! 0)) (convertPosicio (getColumnaFinalJugada(((splitOn " " entrada) !! 0))) (getFilaFinalJugada(((splitOn " " entrada) !! 0)))))
                          then do
                           putStrLn "Jugada valida"
                           mostrarTaulerActual taulerAux
                           putStrLn "Fi de partida, blanques guanyen!!!"
                          else do
                           putStrLn ("Jugada invalida (escac mat invalid) " ++ ((splitOn " " entrada) !! 0))
               else if (((splitOn " " entrada) !! 0) !! 3 == 'x') && (((splitOn " " entrada) !! 1)  !! 3 == 'x')
                    then do
                    if (jugadaLegal tauler (crearJugada ((splitOn " " entrada) !! 0)))
                       then do
                             let taulerAmbBlanquesMatenPeca = llegirMoviment tauler ((splitOn " " entrada) !! 0)
                             if (jugadaLegal taulerAmbBlanquesMatenPeca (crearJugada ((splitOn " " entrada) !! 1)))
                             then do
                                   let tauler = llegirMoviment taulerAmbBlanquesMatenPeca ((splitOn " " entrada) !! 1)
                                   putStrLn "Jugada valida" >> joc tauler lineas (numLinea + 1)
                             else putStrLn ("Jugada invalida " ++ ((splitOn " " entrada) !! 1))
                    else putStrLn ("Jugada invalida " ++ ((splitOn " " entrada) !! 0)) 
               else if (((splitOn " " entrada) !! 0) !! 3 /= 'x') && (((splitOn " " entrada) !! 1)  !! 3 == 'x')
                    then do
                    if (jugadaLegal tauler (crearJugada ((splitOn " " entrada) !! 0)))
                       then do
                             let taulerAmbNegresQueMatenPeca = llegirMoviment tauler ((splitOn " " entrada) !! 0)
                             if(escac taulerAmbNegresQueMatenPeca Blanc) then putStrLn ("Jugada invalida(auto-escac-blanques) " ++ ((splitOn " " entrada) !! 0))
                             else do
                                if (jugadaLegal taulerAmbNegresQueMatenPeca (crearJugada ((splitOn " " entrada) !! 1)))
                                then do
                                    let taulerAux = llegirMoviment taulerAmbNegresQueMatenPeca ((splitOn " " entrada) !! 1)
                                    if(escac taulerAux Negre) then putStrLn ("Jugada invalida(auto-escac-negres) " ++ ((splitOn " " entrada) !! 0))
                                    else do 
                                        putStrLn "Jugada valida" >> joc taulerAux lineas (numLinea + 1)
                                else putStrLn ("Jugada invalida " ++ ((splitOn " " entrada) !! 1))
                    else putStrLn ("Jugada invalida " ++ ((splitOn " " entrada) !! 0))
               else
                  if (((length (splitOn " " entrada)) == 2) && (jugadaLegal tauler (crearJugada ((splitOn " " entrada) !! 0))) && (jugadaLegal tauler (crearJugada ((splitOn " " entrada) !! 1))))
                    then do 
                      --
                     let taulerAux1 = llegirMoviment tauler ((splitOn " " entrada) !! 0)
                     if(escac taulerAux1 Blanc) then putStrLn ("Jugada invalida(auto-escac-blanques) " ++ ((splitOn " " entrada) !! 0))
                     else do
                         let taulerJugadaAnterior = tauler
                         let tauler = taulerAux1
                         let taulerAux2 = llegirMoviment tauler ((splitOn " " entrada) !! 1)
                         if(escac taulerAux2 Negre) then putStrLn ("Jugada invalida(auto-escac-negres) " ++ ((splitOn " " entrada) !! 1))
                         else if ( ('+' == (((splitOn " " entrada) !! 1) !! (length ((splitOn " " entrada) !! 1)-1) )) && ('+' == (((splitOn " " entrada) !! 1) !! (length ((splitOn " " entrada) !! 1)-2) )))                         
                         then if(escacMat taulerAux2 Blanc (convertCharToPeca (((splitOn " " entrada) !! 1) !! 0)) (convertPosicio (getColumnaFinalJugada(((splitOn " " entrada) !! 0))) (getFilaFinalJugada(((splitOn " " entrada) !! 0)))))
                              then do
                                putStrLn "Jugada valida"
                                mostrarTaulerActual taulerAux2
                                putStrLn "Fi de partida, negres guanyen!!!"
                              else do
                              putStrLn "Jugada invalida (escac mat invalid)" 
                         else do
                             if((('+' `elem` ((splitOn " " entrada) !! 0)) && (not (escac taulerAux1 Negre))) || (('+' `elem` ((splitOn " " entrada) !! 1)) && (not (escac taulerAux2 Blanc)))) then putStrLn "Escac NO valid" 
                             else putStrLn "Jugada valida" >> joc taulerAux2 lineas (numLinea + 1)
                  else putStrLn "Jugada invalida"

