-- Auteurs:
-- 
-- Zacharie Chenail-Larcher (CHEZ24069706)

module Main where
    import System.Environment
    import Data.List ( elemIndex, sortBy, groupBy )
    import GHC.Float (int2Double)
    import Data.Char
    import Text.Printf
    import Control.Arrow
    import Distribution.Compat.CharParsing (CharParsing(string))


    -- Constantes :
    tcMinimum = 1
    tcParDefaut = 15

    positionArgumentNomFichier = 0
    positionArgumentTc = 1

    mssgErreurPremierArgument = "Il faut le nom du fichier comme premier argument."
    mssgErreurDeuxiemeArgument = "Le deuxieme argument doit être plus grand ou égal a " ++ ( show tcMinimum )

    -- main
    main = do
               argv <- getArgs
               argc <- return ( length argv )
               nomFichierEntrees <- return ( argv !! positionArgumentNomFichier )
               tc' <- return ( if positionArgumentTc < argc
                               then read (argv !! positionArgumentTc) :: Int
                               else tcParDefaut )
               tc <- return ( if tcMinimum <= tc'
                              then tc'
                              else error mssgErreurDeuxiemeArgument )
               contenu <- if positionArgumentNomFichier < argc
                          then readFile nomFichierEntrees
                          else error mssgErreurPremierArgument
               putStr ( traitement tc contenu )

    -- Votre programme ici :

    --Fonction qui prends la chaine et la convertis en liste de chaque ligne
    --"43525 5 2\n25545 7 5\n7455 3 4" --> [43525 5 2, 25545 7 5, 7455 3 4]
    formaterDonnes :: String -> [String]
    formaterDonnes = lines



    -- map words ["43525 5 2", "25545 7 5", "7455 3 4"]
    -- [["43525","5","2"],["25545","7","5"],["7455","3","4"]]
    formaterDonnes2 :: [String] -> [[String]]
    formaterDonnes2 = map words



    --Fonction qui prend la liste des string et la convertit en Int
    --[["43525","5","2"],["25545","7","5"]] --> [[43525,5,2],[25545,7,5]]
    parserInt :: [[String]] -> [[Int]]
    parserInt = map (map (read :: String -> Int))



    --Fonction qui va convertir les listes internes et les ordonne
    sortLGT :: Ord a => [a] -> [a] -> Ordering
    sortLGT x y = compare (x!!2) (y!!2) -- compare priorities 
        <> compare (y!!1) (x!!1) -- compare time in descending order



    --Fonction principale qui fait appel à toute les autres fonctions
    --elle prend une chaine et la convertit en une liste des listes Int ordonée [[Int]]
    -- "43525 5 2\n25545 7 5\n7455 3 4" --> [[43525,5,2],[7455,3,4],[25545,7,5]]
    sortStrings :: String -> [[Int]]
    sortStrings x = sortBy sortLGT (parserInt (formaterDonnes2 (formaterDonnes x)))

    ------------------------------------------------------------------------------------------------
    -- Vérifications
    ------------------------------------------------------------------------------------------------
    -- verifPatient : vérifie que toute les informations relatives au patient sont valides
    verifPatient :: [Int] -> [Int]
    verifPatient [] = error "erreur de format"
    verifPatient [x] = error "erreur de format"
    verifPatient xs
                | (1 <= (head xs)) && (1 <= xs!!1) && (2 <= xs!!2) && (xs!!2 <= 5) = xs
                | otherwise = error "Données non valides"

    -- verifAllPatient : vérifie les informations pour tous les patients
    verifAllPatient :: [[Int]] -> [[Int]]
    verifAllPatient = map verifPatient

    -- sortString : fonction qui fait simplement appeler sortStrings, mais qui vérifie également
    -- que les valeurs sont valides avec verifAllPatient, sinon une erreur est lancée
    sortString :: String -> [[Int]]
    sortString x = verifAllPatient (sortStrings x)
    -------------------------------------------------------------------------------------------------


    --Convertir ma liste des listes de Int en une liste de Tuples contenant les éléments à afficher
    --[[43525,5,2],[7455,3,4],[25545,7,5]] --> [(1,43525,2),(2,7455,4),(3,25545,5)]
    convertTuple :: [[Int]] -> [(Int, Int, Int)]
    --convertTuple (x:xs) = zipWith (curry (\x -> (fst x,head (snd x), snd x!!2))) [1..] (x:xs)
    convertTuple = zipWith (\i (a:b:c:_) -> (i, a, c)) [1..]



    --Fonction qui prend un tuple et le rend à une liste de string
    display :: (Int, Int, Int) -> String
    display (a,b,c) = printf "%s" (show a++" "++show b++" "++show c)


    --Fonction qui va afficher ma liste des patients ordonnée
    -- elle prend ma liste de Tuples et la converti en String
    --[(1,43525,5),(2,7455,3),(3,25545,7)] --> ["1 43525 5","2 7455 3","3 25545 7"]
    display' :: [(Int, Int, Int)] -> [String]
    display' = map display

    --fonction qui combine toute les autres fonctions
    --prend la chaine initial et retourne la liste des patients ordonné
    -- "43525 5 2\n25545 7 5\n7455 3 4" --> "1 43525 5\n2 7455 3\n3 25545 7\n"
    premiereRegle :: String -> String
    premiereRegle x = unlines (display' (convertTuple (sortString x)))



    --Pour tester :
    -- premiereRegle "43525 5 2\n25545 7 5\n7455 3 4"


    premDeuxiemeRegle :: String -> Int ->String
    premDeuxiemeRegle x tc = unlines (display' (convertTuple (regleDeuxFullInt (sortString x) tc)))

    -- **************************************************************************************
    --Creation d'un data Type Patient
    data Patient = Patient { patientId :: Int, patientTemps :: Int, patientPriorite :: Int } deriving (Show)


    --cette fonction prend la liste des lists de Int et la convertit en une liste des patients
    -- [[1234, 5, 2], [4568, 7, 3], [7897, 6, 5]] ==>[
                                                     --Patient {patientId = 1234, patientTemps = 5, patientPriorite = 2},
                                                     --Patient {patientId = 4568, patientTemps = 7, patientPriorite = 3},
                                                     --Patient {patientId = 7897, patientTemps = 6, patientPriorite = 5}
                                                     --]
    creerListPatient :: [[Int]] -> [Patient]
    creerListPatient = map (\[a, b, c] -> Patient a b c)

    -- **************************************************************************************




    --declaration des constantes: 
    --temps d'attente maximum:
    priorite2 = 15
    priorite3 = 30
    priorite4 = 60
    priorite5 = 120
    tempsAttenteParDefaut = 15

    --Fonction qui retourne une liste de tuples qui contient l'information de temps d'attente pour chaque patient
    -- [[43525,3,2],[43615,2,2],[41111,1,2],[74855,5,3],[25545,4,3],[83115,7,4],[31115,6,4],[10305,8,5]]
    -- [(3,2),(17,2),(31,2),(50,3),(64,3),(82,4),(96,4),(113,5)]
    -- f(i+1)=t_a(i+1) + i*tempsAttenteParDefaut 
    tupleInfo :: [[Int]] -> Int -> [(Int,Int)]
    tupleInfo [[]] a = []
    tupleInfo [[_,x,y]] a = [(x,y)]
    tupleInfo list a =  zipWith (\[_,x,y] i -> (x+a*i,y)) list [0..]


    --groupBy :: ((a0, a2) -> (a1, a2) -> Bool) -> t0 -> [[(Int, Int)]]

    --Grouper les elements par prioritée chaque priorité dans une liste à part
    --[(3,2),(17,2),(50,3),(64,3)] ==> [[(3,2),(17,2)],[(50,3),(64,3)]]
    groupElemMemePriorite :: [(Int,Int)] -> [[(Int,Int)]]
    groupElemMemePriorite = groupBy (\x y -> snd x == snd y)


    --Fonction pour comparer les temps d'attente avec les baremes posés pour chaque prioritée
    --(3,2) ==> True
    --(17,2) ==> False pcq 17<15
    comparerTup :: (Int,Int) -> Bool
    comparerTup (a, b)
                        | b == 2 = a <= priorite2
                        | b == 3 = a <= priorite3
                        | b == 4 = a <= priorite4
                        | b == 5 = a <= priorite5
                        | otherwise = error "Error"

    --Fonction plus generale de comparerTup pour une liste complete
    comparerParPriorite :: [[(Int, Int)]] -> [[Bool]]
    comparerParPriorite = map (map comparerTup)

    --Fonction qui prend une liste de boolean et compte le nombre
    -- de True dans cette liste
    -- [True,False,False] ==> 1
    listeNombreTrue :: [Bool] -> Int
    listeNombreTrue [] = 0
    listeNombreTrue [True] = 1
    listeNombreTrue [False] = 0
    listeNombreTrue (x:xs) = listeNombreTrue [x] + listeNombreTrue xs

    --Fonction qui prends la liste de Boolean et retourne le nombre de True
    --Ainsi la taille de la liste
    --[True,False,False] ==> (1,3)
    donneeFractile :: [Bool] -> (Int,Int)
    donneeFractile xs = (listeNombreTrue xs, length xs)

    --Fonction plus générale de donneeFractile
    --[[TRUE, False, False], [FALSE, FALSE], [FALSE, FALSE], [TRUE]] ==> [(1,3), (0,2) , (0,2) , (1,1)]
    donneeFractileTout :: [[Bool]] -> [(Int,Int)]
    donneeFractileTout = map donneeFractile

    --Fonction qui fait la division du premier element du tuple sur le deuxieme
    --(3,5) ==> 0.6 
    divisionTuple :: (Int,Int) -> Double
    divisionTuple (a,b) =  int2Double a/int2Double b

    --Fonction qui prend la liste des tuples et retourne les fractiles
    --[(1,3), (0,2) , (0,2) , (1,1)] ==> [0.3,0,0,1]
    calculFractil :: [(Int,Int)] -> [Double]
    calculFractil = map divisionTuple


    --Fonction qui calcule la moeyenne geometrique de la liste des fractile
    --[0.3,0,0,1] ==> (0.3*0*0.1)^0.25 = 0
    calculMoyenneGeo :: [Double] -> Double
    calculMoyenneGeo list = product list ** 0.25


    --Algorithme final pour trouver la moyenn geometrique
    algoTriage ::Int -> String -> Double
    algoTriage a chaine= calculMoyenneGeo(calculFractil (donneeFractileTout(comparerParPriorite (groupElemMemePriorite (tupleInfo (sortString chaine) a)))))



    -- ----------------------------------------------------------------------------------------
    convertTupleFrac :: [Double] -> [(Int,Double)]
    convertTupleFrac = zipWith (\i a -> (i, a)) [2..]



    displayFrac :: (Int,Double) -> String
    displayFrac (a,b) = printf "%s" (show a++" "++show b)



    displayFrac' :: [(Int, Double)] -> [String]
    displayFrac' = map displayFrac



    chaineFractile :: String -> Int -> String
    chaineFractile chaine a = unlines (displayFrac'(convertTupleFrac(calculFractil (donneeFractileTout(comparerParPriorite (groupElemMemePriorite (tupleInfo (sortString chaine) a)))))))
    -------------------------------------------------------------------------------------------------------------------------------
    -- premiereRegle "43525 5 2\n25545 7 5\n7455 3 4" *> print (algoTriage 20 "43525 5 2\n25545 7 5\n7455 3 4")
    printResult :: Int -> String -> String
    printResult a chaine = premDeuxiemeRegle chaine a ++ "--------\n" ++ (chaineFractile chaine a) ++ printf "%.4f" (algoTriage a chaine)


--------------------------------------------------------------------------------------------------
    -- Deuxieme regle
    --------------------------------------------------------------------------------------------------

    -- parserDouble: comme parserInt mais de String vers Double
    parserDouble :: [[String]] -> [[Double]]
    parserDouble = map (map (read :: String -> Double))

    -- getWaitMaxFromPrio
    getWaitMaxFromPrio :: Double -> Double
    getWaitMaxFromPrio x
                    | x == 2 = int2Double priorite2
                    | x == 3 = int2Double priorite3
                    | x == 4 = int2Double priorite4
                    | x == 5 = int2Double priorite5
                    | otherwise = error "Priorité non reconnue"

    -- passe une liste de double en Int 
    -- (devrait seulement être appliqué sur des Doules entiers pour pas perdre d'info)
    listDoubleToInt :: [Double] -> [Int]
    listDoubleToInt = map floor

    -- On donne un score a chaque patient qui servira à les ordonner
    -- giveWaitTime [1232, 5, 2] 15 = 0
    -- giveWaitTime [ident, wait, prio] consultTime
    giveWaitScore :: [Double] -> Double  -> Int
    giveWaitScore xs g = floor ( ((getWaitMaxFromPrio (xs!!2)) - (xs!!1)) / g )

    -- On ajoute le score (nbr de séance d'attente permise)
    -- ex.: [1222, 5, 2] -> [1222, 5, 2, 0]
    -- ex.: [1222, 0, 2] -> [1222, 0, 2, 1]
    addWaitScore :: [Double] -> Double -> [Int]
    addWaitScore xs g = listDoubleToInt xs ++ [giveWaitScore xs g]

    -- on ajoute les scores pour chaque
    addAllWaitScore :: [[Double ]] -> Double -> [[Int]]
    addAllWaitScore [] g = []
    addAllWaitScore [x] g = [addWaitScore x g]
    addAllWaitScore (x:xs) g = addWaitScore x g : addAllWaitScore xs g

    
    -----------------------------------------------------------------------------------------
    -- Grouper les patients selon les priorités
    -----------------------------------------------------------------------------------------
    -- creerGroupePrio: cré un groupe de patients de même prio
    -- ex.: creerGroupePrio 2 [[101, 1, 2, 0], [102, 2, 2, 0], [201, 3, 3, 1]] -> [[101, 1, 2, 0], [102, 2, 2, 0]]
    creerGroupePrio :: Int -> [[Int]] -> [[Int]]
    creerGroupePrio _ [] = []
    creerGroupePrio p [x]
                        | x!!2 == p = [x]
                        | otherwise = []
    creerGroupePrio p (x:xs)
                        | x!!2 == p = x : creerGroupePrio p xs
                        | otherwise = creerGroupePrio p xs

    -- grouperParPrio: regroupe les patients de meme prio ensemble
    -- ex.: grouperParPrio [[101, 1, 2, 0], [102, 2, 2, 0], [201, 3, 3, 1]] -> [[[101, 1, 2, 0], [102, 2, 2, 0]], [[201, 3, 3, 1]]]
    grouperParPrio :: [[Int]] -> [[[Int]]]
    grouperParPrio xs = creerGroupePrio 2 xs : [creerGroupePrio 3 xs] ++ [creerGroupePrio 4 xs] ++ [creerGroupePrio 5 xs]

    -- enleve les listes vides
    removeEmptyList :: [[[Int]]] -> [[[Int]]]
    removeEmptyList [] = []
    removeEmptyList [[]] = []
    removeEmptyList [x]
                    | x == [] = []
                    | otherwise = [x]
    removeEmptyList (x:xs)
                    | x == [] = removeEmptyList xs
                    | otherwise = x : removeEmptyList xs

    -- grouperParPrio mais sans les listes vides
    grouperParPrioNoEmpty :: [[Int]] -> [[[Int]]]
    grouperParPrioNoEmpty xs = removeEmptyList (grouperParPrio xs)
    ------------------------------------------------------------------------------------------

    ------------------------------------------------------------------------------------------
    -- Application de la première règle
    ------------------------------------------------------------------------------------------
    -- comparePremRegle: retourne entre deux patients de même prio, lequel a priorité sur l'autre
    comparePremRegle :: [Int] -> [Int] -> [Int]
    comparePremRegle xs ys
                        | xs!!1 >= ys!!1 = xs
                        | otherwise = ys

    -- appPremiereRegle: a partir d'un groupe de patients de même prio, retourne celui
    -- qui doit passer avant
    appPremiereRegle :: [[Int]] -> [Int]
    appPremiereRegle [] = []
    appPremiereRegle [xs] = xs
    appPremiereRegle (x:xs) = comparePremRegle x (appPremiereRegle xs)

    -- premierChaquePrio: a partir de liste de groupe, donne une liste avec celui qui devrait
    -- passer avant pour chaque groupe
    premierChaquePrio :: [[[Int]]] -> [[Int]]
    premierChaquePrio = map appPremiereRegle
    ------------------------------------------------------------------------------------------

    ------------------------------------------------------------------------------------------
    -- Algorithme deuxième règle
    ------------------------------------------------------------------------------------------
    -- aPrioSur A B: Indique si l'élément du tableau A va être ordonné avant celui
    -- du tableau B, si oui on retourne True sinon False, devrait être utilisé pour comparer
    -- des patients de priorités différentes
    -- aPrioSur [1222, 5, 2, 0] [1222, 0, 2, 1] = True
    aPrioSur :: [Int] -> [Int] -> Bool
    aPrioSur xs ys
                | (xs!!2 == ys!!2) && (xs!!1 >= ys!!1) = True
                | (xs!!2 /= ys!!2) && (xs!!3 >= 0) && (ys!!3 < 0) = True
                | (xs!!2 /= ys!!2) && (xs!!3 < ys!!3) && (xs!!3 >= 0) = True
                | (xs!!2 /= ys!!2) && (xs!!3 < ys!!3) && (ys!!3 < 0) = True
                | (xs!!2 < ys!!2) && (xs!!3 == ys!!3) = True
                | otherwise = False

    -- trouvePrio A B: Donne celui qui va passer avant entre A et B
    -- devrait être utilisé sur des patients de différentes priorités
    trouvePrio :: [Int] -> [Int] -> [Int]
    trouvePrio xs ys
                | aPrioSur xs ys = xs
                | otherwise = ys    

    -- trouveAllPrio A: Donne celui qui a la plus haute priorité dans tout A
    -- devrait être appliqué sur les patients qui passe avant pour chaque prio
    trouveAllPrio :: [[Int]] -> [Int]
    trouveAllPrio [] = []
    trouveAllPrio [xs] = xs
    trouveAllPrio (x:xs) = trouvePrio x (trouveAllPrio xs)
    ------------------------------------------------------------------------------------------

    ------------------------------------------------------------------------------------------
    -- regle 1 et 2 ensemble
    ------------------------------------------------------------------------------------------
    -- prochainAPasser: Choisi le prochain patient qui va passer
    -- applique la premiere regle et l'algo de la deuxieme
    prochainAPasser :: [[Int]] -> [Int]
    prochainAPasser xs = trouveAllPrio (premierChaquePrio (grouperParPrioNoEmpty xs))
    ------------------------------------------------------------------------------------------

    ------------------------------------------------------------------------------------------
    -- Regroupement du traitement d'ordonnancement et clean up
    ------------------------------------------------------------------------------------------
    -- deleteElementDeList: supprime une des listes de la liste de liste
    deleteElementDeList :: [[Int]] -> [Int] -> [[Int]]
    deleteElementDeList [] _ = []
    deleteElementDeList [x] y
                            | x == y = []
                            | otherwise = [x]
    deleteElementDeList (x:xs) y
                                | x == y = xs
                                | otherwise = x : deleteElementDeList xs y

    -- enleverUnAuScore: pour enlever 1 au score (correspond au nombre de séance d'attente possible)
    -- si l'élément n'est pas passé 
    -- la liste doit être de taille 4 soit [a, a, a, a] pour fonctionner
    enleverUnAuScore :: [Int] -> [Int]
    enleverUnAuScore xs = (init xs) ++ [(last xs) - 1]

    -- enleverUnAuScoreAll: appliquer enleverUnAuScore sur toute la liste de liste
    enleverUnAuScoreAll :: [[Int]] -> [[Int]]
    enleverUnAuScoreAll = map enleverUnAuScore

    -- enleverScore: on enleve le score de la liste
    -- doit suivre la forme [a, a, a, score]
    enleverScore :: [Int] -> [Int]
    enleverScore = init


    -- mettreEnOrdreDePrio: création de la liste selon l'ordre de priorité (règle 2 et règle 1)
    -- ex.: mettreEnOrdreDePrio [[1222, 0, 2, 1], [1211, 5, 2, 0]] => [[1211, 5, 2, 0], [1222, 0, 2, 0]]
    mettreEnOrdreDePrio :: [[Int]] -> [[Int]]
    mettreEnOrdreDePrio [] = []
    mettreEnOrdreDePrio [xs] = [xs]
    mettreEnOrdreDePrio xs = prochainAPasser xs : mettreEnOrdreDePrio (enleverUnAuScoreAll(deleteElementDeList xs (prochainAPasser xs)))

    -- enleverScoreAll: maintenant qu'on a une liste ordonnée selon le score on peut enlever le score
    enleverScoreAll :: [[Int]] -> [[Int]]
    enleverScoreAll = map enleverScore

    -- regleDeux: ordonnancement total selon la regle 2 et 1
    -- regleDeux [listes] consultTime
    regleDeux :: [[Double]] -> Double  -> [[Int]]
    regleDeux xs g = enleverScoreAll (mettreEnOrdreDePrio (addAllWaitScore xs g))

    -- regleDeuxTimeInt: regleDeux mais le temps est pris en Int
    regleDeuxTimeInt :: [[Double]] -> Int -> [[Int]]
    regleDeuxTimeInt xs g = regleDeux xs (int2Double g)

    -- regleDeuxFullInt: regleDeux mais toutes les valeurs sont prises en Int
    regleDeuxFullInt :: [[Int]] -> Int -> [[Int]]
    regleDeuxFullInt xs g = regleDeux (allInt2Double xs) (int2Double g)

    allInt2Double :: [[Int]] -> [[Double]]
    allInt2Double [] = []
    allInt2Double [xs] = [map int2Double xs]
    allInt2Double (x:xs) = (map int2Double x) : allInt2Double xs
    ------------------------------------------------------------------------------------------


    -- La fonction traitement est le point de départ de votre logiciel.
    -- @param tc :: Int
    --        Ce paramètre contient le temps moyen de consultation en minute.
    -- @param contenu :: String
    --        Ce paramètre contient le contenu du fichier en entrées.
    -- @return :: String
    --        Cette fonction retourne la chaîne de caractères qui sera affichée
    --        a la fin de l'exécution du programme.  Elle devra contenir le nouvel
    --        ordre des patients et la table des fractiles formatés pour l'affichage.
    traitement :: Int -> String -> String
    traitement tc contenu = printResult tc contenu