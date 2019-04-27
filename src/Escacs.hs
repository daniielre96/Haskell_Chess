import Data.List
import Data.Char
import Data.List.Split
import System.IO (hFlush, stdout)

type Tauler2 = [[Casella]]

type Tauler = [Casella]

data Casella = Casella {peca:: Peca, posicio :: Posicio} deriving (Show,Eq)
data Peca = Peca Color Tipus deriving (Show, Eq)
data Color = Blanc | Negre deriving (Show, Eq)
data Tipus = Peo | Cavall | Alfil | Torre | Dama | Rei deriving (Show, Eq)
data Jugada = Jugada {pecaJugada :: Peca, posIni :: Posicio, posFi :: Posicio, esMata :: Bool} deriving (Show, Eq)

type Posicio = (Int, Int)

taulerInicial = unlines ["tcadract",
                         "pppppppp",
                         "........",
                         "........",
                         "........",
                         "........",
                         "PPPPPPPP",
                         "TCADRACT"
                         ]


llegirTauler :: String -> Tauler2
llegirTauler = map llegirFila . lines
  where llegirFila = map llegirCasella

mostrarTauler :: Tauler -> [Posicio] -> String
mostrarTauler t [] = ""
mostrarTauler t (pos:posicionsTauler) = ([(mostrarCasella t pos)] ++ mostrarTauler t posicionsTauler)

mostrarTaulerActual :: Tauler -> IO()
mostrarTaulerActual tauler = putStr ((unlines (splitCaselles 8 (mostrarTauler tauler totesPosicions))))


mostrarCasella :: Tauler -> Posicio -> Char
mostrarCasella tauler pos =  if (hiHaPeca pos tauler) 
                        then mostrarPeca (getPeca tauler pos) 
                      else '.'


llegirCasella :: Char -> Casella
llegirCasella '.' = Casella {peca = (Peca Blanc Rei), posicio = (100,100)}
llegirCasella c = Casella {peca =(llegirPeca c), posicio = (0,0)}



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

-- Funciona
getPeca :: Tauler -> Posicio -> Peca
getPeca tauler pos = peca (head (filter (\p -> posicio p == pos) tauler))

-- Funciona
getPosicionsTauler :: Tauler -> [Posicio]
getPosicionsTauler tauler = [posicio (tauler !! i) | i <- [0..(length tauler)-1]]

-- Funciona
hiHaPeca :: Posicio -> Tauler -> Bool
hiHaPeca posicio tauler = posicio `elem` (getPosicionsTauler tauler)

-- Funciona
validInTheBoard :: Int -> Int -> Bool
validInTheBoard x y = x >= 0 && x <= 7 && y <= 7 && y >= 0

-- Funciona
validInTheBoard2 :: Posicio -> Bool
validInTheBoard2 (x, y) = x >= 0 && x <= 7 && y <= 7 && y >= 0

-- Funciona
esPeo :: Peca -> Bool
esPeo peca = peca == (Peca Blanc Peo) || peca == (Peca Negre Peo)

-- Funciona
esRei :: Peca -> Bool
esRei peca = peca == (Peca Blanc Rei) || peca == (Peca Negre Rei)

-- Funciona
esTorre :: Peca -> Bool
esTorre peca = peca == (Peca Blanc Torre) || peca == (Peca Negre Torre)

esAlfil :: Peca -> Bool
esAlfil peca = peca == (Peca Blanc Alfil) || peca == (Peca Negre Alfil)

esDama :: Peca -> Bool
esDama peca = peca == (Peca Blanc Dama) || peca == (Peca Negre Dama)

esCavall :: Peca -> Bool
esCavall peca = peca == (Peca Blanc Cavall) || peca == (Peca Negre Cavall)


-- Funciona
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


alguEnLesPosicions :: Tauler -> [Posicio] -> Bool
alguEnLesPosicions tauler [] = False
alguEnLesPosicions tauler (x:xs) =
                    if (hiHaPeca x tauler)
                      then True
                    else alguEnLesPosicions tauler xs


alguEntre :: Tauler -> Posicio -> Posicio -> Bool
alguEntre tauler pos1 pos2
  | fst(pos1) == fst(pos2) = miraEntreCasellesHoritzontal tauler pos1 pos2 --  "Horitzontal" 
  | snd(pos1) == snd(pos2) = miraEntreCasellesVertical tauler pos1 pos2 -- Vertical 
  | abs(fst(pos1)-fst(pos2)) == abs(snd(pos1)-snd(pos2)) = miraEntreCasellesDiagonals tauler pos1 pos2 --"Diagonal"
  | otherwise = error("moviment invalid")

miraEntreCasellesDiagonals :: Tauler -> Posicio -> Posicio -> Bool
miraEntreCasellesDiagonals t pos1 pos2
  | (fst(pos2) > fst(pos1)) && (snd(pos2) > snd(pos1)) = alguEnLesPosicions t [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) > (fst(pos1))) && ((snd(posicions)) > (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) < fst(pos2) && snd(posicions) < snd(pos2)]
  | (fst(pos1) > fst(pos2)) && (snd(pos1) > snd(pos2)) = alguEnLesPosicions t [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) < (fst(pos1))) && ((snd(posicions)) < (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) > fst(pos2) && snd(posicions) > snd(pos2)]
  | (fst(pos2) < fst(pos1)) && (snd(pos2) > snd(pos1)) = alguEnLesPosicions t [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) < (fst(pos1))) && ((snd(posicions)) > (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) > fst(pos2) && snd(posicions) < snd(pos2)]
  | (fst(pos2) > fst(pos1)) && (snd(pos2) < snd(pos1)) = alguEnLesPosicions t [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) > (fst(pos1))) && ((snd(posicions)) < (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) < fst(pos2) && snd(posicions) > snd(pos2)]
  | otherwise = False

miraEntreCasellesVertical:: Tauler -> Posicio -> Posicio -> Bool
miraEntreCasellesVertical t pos1 pos2 = alguEnLesPosicions t [(x,y) |  x <- [fst(posicio1)+1..fst(posicio2)-1], y <- [snd(posicio1)], x < fst(posicio2)]
  where
    posicio1 = if (fst(pos1) > fst(pos2)) then pos2 else pos1
    posicio2 = if (fst(pos1) > fst(pos2)) then pos1 else pos2


miraEntreCasellesHoritzontal :: Tauler -> Posicio -> Posicio -> Bool
miraEntreCasellesHoritzontal t pos1 pos2 = alguEnLesPosicions t [(y,x) |  x <- [snd(posicio1)+1..snd(posicio2)-1], y <- [fst(posicio1)], y < snd(posicio2)]
  where
    posicio1 = if (snd(pos1) > snd(pos2)) then pos2 else pos1
    posicio2 = if (snd(pos1) > snd(pos2)) then pos1 else pos2



 
-- rebem una posició del tauler i retornem una llista de posicions possibles de moviments
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

esBlanca :: Peca -> Bool
esBlanca peca = peca == (Peca Blanc Peo) || peca == (Peca Blanc Torre) || peca == (Peca Blanc Cavall) || peca == (Peca Blanc Alfil) || peca == (Peca Blanc Dama) || peca == (Peca Blanc Rei)

esNegre :: Peca -> Bool
esNegre peca = peca == (Peca Negre Peo) || peca == (Peca Negre Torre) || peca == (Peca Negre Cavall) || peca == (Peca Negre Alfil) || peca == (Peca Negre Dama) || peca == (Peca Negre Rei)

movimentsPosiblesRei :: Casella -> [Posicio] -> [Posicio]
movimentsPosiblesRei casella [] = []
movimentsPosiblesRei casella (x:xs) =  if(validInTheBoard2 (sumaPosicions (posicio casella) x)) 
                                       then [sumaPosicions (posicio casella) x ] ++ movimentsPosiblesRei casella xs
                                       else [] ++ movimentsPosiblesRei casella xs
                                    

movimentsPosiblesTorre :: Casella -> [Posicio]
movimentsPosiblesTorre casella = [(x,y) | x <- [0..7], y <- [0..7], x == fst(posicio casella) || y == snd(posicio casella), (x /= fst(posicio casella) || y /= snd(posicio casella))]

movimentsPosiblesAlfil :: Casella -> [Posicio]
movimentsPosiblesAlfil casella = [((fst(posicio casella)) + x, (snd(posicio casella)) + y) | x <- [0..7] , y <- [0..7] , x == y, validInTheBoard2 ((fst(posicio casella)) + x , (snd(posicio casella)) + y), ((fst(posicio casella) + x) /= fst(posicio casella) || (snd(posicio casella)) + y /= snd(posicio casella))] ++
                                 [((fst(posicio casella)) - x, (snd(posicio casella)) - y) | x <- [0..7] , y <- [0..7] , x == y, validInTheBoard2 ((fst(posicio casella)) - x , (snd(posicio casella)) - y), ((fst(posicio casella) - x) /= fst(posicio casella) || (snd(posicio casella)) - y /= snd(posicio casella))] ++
                                 [((fst(posicio casella)) + x, (snd(posicio casella)) - y) | x <- [0..7] , y <- [0..7] , x == y, validInTheBoard2 ((fst(posicio casella)) + x , (snd(posicio casella)) - y), ((fst(posicio casella) + x) /= fst(posicio casella) || (snd(posicio casella)) - y /= snd(posicio casella))] ++
                                 [((fst(posicio casella)) - x, (snd(posicio casella)) + y) | x <- [0..7] , y <- [0..7] , x == y, validInTheBoard2 ((fst(posicio casella)) - x , (snd(posicio casella)) + y), ((fst(posicio casella) - x) /= fst(posicio casella) || (snd(posicio casella)) + y /= snd(posicio casella))] 

movimentsPosiblesDama :: Casella -> [Posicio]
movimentsPosiblesDama casella = (movimentsPosiblesTorre Casella {peca = (Peca Blanc Torre), posicio = posicio casella}) ++ (movimentsPosiblesAlfil Casella {peca = (Peca Blanc Alfil), posicio = posicio casella})

movimentsPosiblesCavall :: Casella -> [Posicio]
movimentsPosiblesCavall casella = [(fst(posicio casella)+(fst(aux)),(snd(posicio casella))+(snd(aux))) | aux <- [(x,y) | x <- [-2..2], y <- [-2..2], abs(x) /= abs(y), x /=0, y /=0], validInTheBoard (fst(posicio casella)+(fst(aux))) ((snd(posicio casella))+(snd(aux)))]

fesJugada :: Tauler -> Jugada -> Tauler
fesJugada tauler jugada = if (jugadaLegal tauler jugada) then (mourePeca tauler jugada) else error ("jugada invalida")

mourePeca :: Tauler -> Jugada -> Tauler
mourePeca tauler jugada = if (existeixPosicio tauler (posFi jugada))
                          then delete (Casella{peca = (getPeca tauler (posFi jugada)) , posicio = (posFi jugada)}) (delete (Casella{peca = (pecaJugada jugada), posicio = (posIni jugada)}) tauler ++ [Casella{peca = (pecaJugada jugada), posicio = (posFi jugada)}])
                          else delete (Casella{peca = (pecaJugada jugada), posicio = (posIni jugada)}) tauler ++ [Casella{peca = (pecaJugada jugada), posicio = (posFi jugada)}]

existeixPosicio :: Tauler -> Posicio -> Bool
existeixPosicio [] pos = False
existeixPosicio (x:xs) pos = if((posicio x) == pos) then True else existeixPosicio xs pos



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

sumaPosicions :: Posicio -> Posicio -> Posicio
sumaPosicions x y = ((fst(x) + fst(y)) , (snd(x) + snd(y)))

posaPosicio :: Casella -> Posicio -> Casella
posaPosicio cas posicio = Casella{peca = peca cas, posicio = posicio}

pecesDunColor :: Tauler -> Color -> Tauler
pecesDunColor t col = case (col == Blanc) of
  True -> [cas | cas <- t, esBlanca (peca cas)]
  False -> [cas | cas <- t, esNegre (peca cas)]

noHiHaAlguEntreRei :: Tauler -> Casella -> Tauler -> Bool
noHiHaAlguEntreRei _ _ [] = False
noHiHaAlguEntreRei tauler casellaRei (x:pecesAltreColor) = if((jugadaLegal tauler Jugada{pecaJugada = (peca x), posIni = (posicio x), posFi = (posicio casellaRei), esMata = True}) && (esCavall(peca x) ||  (not (alguEntre tauler (posicio casellaRei) (posicio x))))) then True else noHiHaAlguEntreRei tauler casellaRei pecesAltreColor

splitCaselles :: Int -> [a] -> [[a]]
splitCaselles _ [] = []
splitCaselles n xs = as : splitCaselles n bs
  where (as,bs) = splitAt n xs

getPosicio :: Tauler -> Peca -> Posicio
getPosicio [] _ = (0,0)
getPosicio (x:t) peca2 = if ((peca x) == peca2) then posicio x else getPosicio t peca2

escac :: Tauler -> Color -> Bool
escac t col = if (col == Blanc)
                then noHiHaAlguEntreRei t Casella{peca = (Peca Blanc Rei), posicio = getPosicio t (Peca Blanc Rei)} (pecesDunColor t Negre)
              else noHiHaAlguEntreRei t Casella{peca = (Peca Negre Rei), posicio = getPosicio t (Peca Negre Rei)} (pecesDunColor t Blanc)




-- (FET!!!!!!!!!!) potFugirRei -> pot moure normal o moure matant algú, a on es mou tampoc es escac
-- (FET!!!!!!!!!!) teCoberturaRei -> hi ha alguna peça del meu bàndol que et pot cobrir, aixo no funciona si amenaça un cavall
-- (FET!!!!!!!!!!) esPotMatarPeca -> hi ha alguna peça del meu bàndol que pot matar la peca enemiga

escacMat :: Tauler -> Color -> Peca -> Bool
escacMat tauler color pecaQueFaEscac = (escac tauler color) && (not (potFugirRei tauler color)) && (not (teCoberturaRei tauler pecaQueFaEscac color)) && (not(esPotMatarPecaSenseEscac tauler pecaQueFaEscac (pecesDunColor tauler color)))


teCoberturaRei :: Tauler -> Peca -> Color -> Bool
teCoberturaRei tauler pecaQueFaEscac color = if (esCavall pecaQueFaEscac)
                                             then False
                                             else existeixMovimentQueEmSalvi tauler (generaPosicionsEntre tauler (getPosicio tauler (Peca color Rei)) (getPosicio tauler pecaQueFaEscac)) (pecesDunColor tauler color)


existeixMovimentQueEmSalvi :: Tauler -> [Posicio] -> Tauler -> Bool
existeixMovimentQueEmSalvi _ [] _ = False
existeixMovimentQueEmSalvi tauler (x:posicionsEntre) pecesMeves = if (hihaUnaPecaMevaQuePotAnar tauler x pecesMeves)
                                                                  then True
                                                                  else existeixMovimentQueEmSalvi tauler posicionsEntre pecesMeves

hihaUnaPecaMevaQuePotAnar :: Tauler -> Posicio -> Tauler -> Bool
hihaUnaPecaMevaQuePotAnar _ _ [] = False
hihaUnaPecaMevaQuePotAnar tauler posEntre (x:pecesMeves) = if (jugadaLegal tauler Jugada{pecaJugada = (peca x), posIni = (posicio x), posFi = posEntre ,esMata = False})
                                                           then True 
                                                           else hihaUnaPecaMevaQuePotAnar tauler posEntre pecesMeves

generaPosicionsEntre :: Tauler -> Posicio -> Posicio -> [Posicio]
generaPosicionsEntre tauler pos1 pos2
  | fst(pos1) == fst(pos2) = retornaCasellesHoritzontal tauler pos1 pos2 --  "Horitzontal" 
  | snd(pos1) == snd(pos2) = retornaCasellesVertical tauler pos1 pos2 -- Vertical 
  | abs(fst(pos1)-fst(pos2)) == abs(snd(pos1)-snd(pos2)) = retornaCasellesDiagonals tauler pos1 pos2 --"Diagonal"
  | otherwise = []

retornaCasellesDiagonals :: Tauler -> Posicio -> Posicio -> [Posicio]
retornaCasellesDiagonals t pos1 pos2
  | (fst(pos2) > fst(pos1)) && (snd(pos2) > snd(pos1)) = [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) > (fst(pos1))) && ((snd(posicions)) > (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) < fst(pos2) && snd(posicions) < snd(pos2)]
  | (fst(pos1) > fst(pos2)) && (snd(pos1) > snd(pos2)) = [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) < (fst(pos1))) && ((snd(posicions)) < (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) > fst(pos2) && snd(posicions) > snd(pos2)]
  | (fst(pos2) < fst(pos1)) && (snd(pos2) > snd(pos1)) = [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) < (fst(pos1))) && ((snd(posicions)) > (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) > fst(pos2) && snd(posicions) < snd(pos2)]
  | (fst(pos2) > fst(pos1)) && (snd(pos2) < snd(pos1)) = [posicions | posicions <- (moviment (Peca Negre Alfil) pos1), ((fst(posicions)) > (fst(pos1))) && ((snd(posicions)) < (snd(pos1))), (fst(posicions) /= fst(pos2)) && (snd(pos2) /= snd(posicions)), fst(posicions) < fst(pos2) && snd(posicions) > snd(pos2)]
  | otherwise = []

retornaCasellesVertical:: Tauler -> Posicio -> Posicio -> [Posicio]
retornaCasellesVertical t pos1 pos2 = [(x,y) |  x <- [fst(posicio1)+1..fst(posicio2)-1], y <- [snd(posicio1)], x < fst(posicio2)]
  where
    posicio1 = if (fst(pos1) > fst(pos2)) then pos2 else pos1
    posicio2 = if (fst(pos1) > fst(pos2)) then pos1 else pos2


retornaCasellesHoritzontal :: Tauler -> Posicio -> Posicio -> [Posicio]
retornaCasellesHoritzontal t pos1 pos2 = [(y,x) |  x <- [snd(posicio1)+1..snd(posicio2)-1], y <- [fst(posicio1)], y < snd(posicio2)]
  where
    posicio1 = if (snd(pos1) > snd(pos2)) then pos2 else pos1
    posicio2 = if (snd(pos1) > snd(pos2)) then pos1 else pos2




esPotMatarPecaSenseEscac :: Tauler -> Peca -> Tauler -> Bool
esPotMatarPecaSenseEscac _ _ [] = False
esPotMatarPecaSenseEscac tauler pecaQueFaEscac (x:pecesDunColor) = if jugadaLegal tauler (Jugada{pecaJugada = peca x, posIni = posicio x, posFi = (getPosicio tauler pecaQueFaEscac), esMata = True}) && not(escac (mourePeca tauler Jugada{pecaJugada = peca x, posIni = posicio x, posFi = (getPosicio tauler pecaQueFaEscac), esMata = True}) color)
                                                                   then True
                                                                   else esPotMatarPecaSenseEscac tauler pecaQueFaEscac pecesDunColor
                                                                     where color = if(esNegre (peca x)) then Negre else Blanc


potFugirRei :: Tauler -> Color -> Bool
potFugirRei tauler color = (esPotMoureRei tauler (moviment (Peca color Rei) posicioDeLaPeca) color) || (elReiPotMatar tauler (moviment (Peca color Rei) posicioDeLaPeca) color)
  where posicioDeLaPeca = getPosicio tauler (Peca color Rei)

esPotMoureRei :: Tauler -> [Posicio] -> Color -> Bool
esPotMoureRei _ [] _ = False
esPotMoureRei tauler (x:movimentsPossibles) color = if((not (hiHaPeca x tauler)) && not(escac (mourePeca tauler Jugada{pecaJugada = (Peca color Rei), posIni = getPosicio tauler (Peca color Rei), posFi = x, esMata = False}) color))
                                                    then True
                                                    else esPotMoureRei tauler movimentsPossibles color

elReiPotMatar :: Tauler -> [Posicio] -> Color -> Bool
elReiPotMatar _ [] _ = False
elReiPotMatar tauler (x:movimentsPossibles) color = if((hiHaPeca x tauler) && (posXesColorContrari tauler x color) && not(escac (mourePeca tauler Jugada{pecaJugada = (Peca color Rei), posIni = getPosicio tauler (Peca color Rei), posFi = x, esMata = True}) color))
                                                    then True
                                                    else elReiPotMatar tauler movimentsPossibles color

posXesColorContrari :: Tauler -> Posicio -> Color -> Bool
posXesColorContrari tauler posicio color = (esNegre (getPeca tauler posicio) && color == Blanc) ||  (esBlanca (getPeca tauler posicio) && color == Negre)

tauler = llegirTauler taulerInicial -- Tauler d'inici

movimentPosicioRei = [(x,y) | x <- [-1..1], y <- [-1..1], (x /= 0 || y /= 0)]

movimentPosicioTorre = [(0,1), (1,0), (0,-1), (-1,0)]

totesPosicions = [(x,y) | x <- [(0::Int)..(7::Int)], y <- [(0::Int)..(7::Int)]] -- totes les posicions possibles del tauler

posicions = [(x,y) | x <- [0..1]++[6..7], y <- [0..7]]

taulerFinal = [posaPosicio casella posicio | posicio <- posicions, casella <- [(tauler !! fst(posicio) !! snd(posicio))]] -- Tauler real


llegirMoviment :: Tauler -> String -> Tauler
llegirMoviment tauler input = if ((input !! 3) == 'x') then  mataPeca tauler input else ferMoviment tauler input

convertCharToPeca :: Char -> Peca
convertCharToPeca c = llegirPeca c

convertPosicio :: Char -> Char -> Posicio
convertPosicio columna fila = (abs((digitToInt fila) - 8), (ord columna) - 97) 

crearJugada :: String -> Jugada
crearJugada input = if ((input !! 3) == 'x') then jugadaMatant else juagadaSenseMatar
                      where jugadaMatant = Jugada{pecaJugada = p, posIni = posicioInicial, posFi = posicioFinal1, esMata = True}
                            posicioFinal1 = convertPosicio (input !! 4) (input !! 5)
                            juagadaSenseMatar = Jugada{pecaJugada = p, posIni = posicioInicial, posFi = posicioFinal2, esMata = False}
                            p = convertCharToPeca (input !! 0)
                            posicioInicial = convertPosicio (input !! 1) (input !! 2)
                            posicioFinal2 = convertPosicio (input !! 3) (input !! 4)


mataPeca :: Tauler -> String -> Tauler
mataPeca tauler input =   (fesJugada tauler jugada)
                     where jugada = Jugada{pecaJugada = p, posIni = posicioInicial, posFi = posicioFinal, esMata = True}
                           p = convertCharToPeca (input !! 0)
                           posicioInicial = convertPosicio (input !! 1) (input !! 2)
                           posicioFinal = convertPosicio (input !! 4) (input !! 5)

ferMoviment :: Tauler -> String -> Tauler
ferMoviment tauler input = (fesJugada tauler jugada)
                      where jugada = Jugada{pecaJugada = p, posIni = posicioInicial, posFi = posicioFinal, esMata = False}
                            p = convertCharToPeca (input !! 0)
                            posicioInicial = convertPosicio (input !! 1) (input !! 2)
                            posicioFinal = convertPosicio (input !! 3) (input !! 4)

validInput :: String -> Bool
validInput str = if ((length (splitOn " " str) /= 1) && (length (splitOn " " str) /= 2))
                  then False
                 else True

main :: IO()
main = joc taulerFinal

joc :: Tauler -> IO()
joc tauler = do
  putStrLn "========"
  mostrarTaulerActual tauler
  putStrLn "========"
  putStrLn "abcdefgh"
  putStr "Entrada> "
  hFlush stdout
  entrada <- getLine
  realitzaAccio tauler entrada

realitzaAccio :: Tauler -> String -> IO()
realitzaAccio _ "sortir" = putStrLn "Adeu gràcies per jugar"
realitzaAccio tauler entrada = case entrada of
  "" -> putStrLn "Entrada invalida" >> joc tauler
  otherwise -> if not $ validInput entrada -- si la entrada no es correcte
                  then putStrLn "Entrada invalida" >> joc tauler
               else
                  if (((length (splitOn " " entrada)) == 2) && (jugadaLegal tauler (crearJugada ((splitOn " " entrada) !! 0))) && (jugadaLegal tauler (crearJugada ((splitOn " " entrada) !! 1))))
                    then do 
                     let taulerAux = llegirMoviment tauler ((splitOn " " entrada) !! 0)
                     if(escac taulerAux Blanc) then putStrLn "Jugada invalida(auto-escac-blanques)" >> joc tauler
                     else do
                         let taulerJugadaAnterior = tauler
                         let tauler = taulerAux
                         let taulerAux = llegirMoviment tauler ((splitOn " " entrada) !! 1)
                         if(escac taulerAux Negre) then putStrLn "Jugada invalida(auto-escac-negres)" >> joc taulerJugadaAnterior
                         else if ( ('+' == (((splitOn " " entrada) !! 1) !! (length ((splitOn " " entrada) !! 1)-1) )) && ('+' == (((splitOn " " entrada) !! 1) !! (length ((splitOn " " entrada) !! 1)-2) )))                         
                         then if(escacMat taulerAux Blanc (convertCharToPeca (((splitOn " " entrada) !! 1) !! 0)))
                              then putStrLn "Fi de partida, negres guanyen!!!"
                              else do
                              putStrLn "Jugada invalida (escac invalid)" >> joc taulerJugadaAnterior
                         else do
                             if((('+' `elem` ((splitOn " " entrada) !! 0)) && (not (escac taulerAux Negre))) || (('+' `elem` ((splitOn " " entrada) !! 1)) && (not (escac taulerAux Blanc)))) then putStrLn "Escac NO valid" >> joc taulerAux
                             else putStrLn "Jugada valida" >> joc taulerAux
                  else if ((length (splitOn " " entrada) == 1) && (jugadaLegal tauler (crearJugada ((splitOn " " entrada) !! 0))))
                     then do
                      let taulerAux = llegirMoviment tauler ((splitOn " " entrada) !! 0)
                      if(escacMat taulerAux Negre (convertCharToPeca (((splitOn " " entrada) !! 0) !! 0)))
                      then putStrLn "Fi de partida, blanques guanyen!!!"
                      else do
                        putStrLn "Jugada invalida (escac invalid)" >> joc tauler
                  else putStrLn "Jugada invalida" >> joc tauler

