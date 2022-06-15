-- Auteurs:
-- OUSSAMA EL-FIGHA (ELFO74030209)
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



    -- fonction qui prend une liste de fractile et la convertit en liste de tuples pour chaque priorité son fractile
    -- [0.3, 0.88, 0.68, 0.12] ==> [(2,0.3),(3,0.88),(4,0.68),(5,0.12)]
    convertTupleFrac :: [Double] -> [(Int,Double)]
    convertTupleFrac = zipWith (\i a -> (i, a)) [2..]


    -- fonction qui prend un tuple de fractile avec leur priorité et la convertit en chaine pour les afficher
    -- (2,0.3),(3,0.88),(4,0.68),(5,0.12) ==> 2 0.3
                                            --3 0.88
                                            --4 0.68
                                            --5 0.12
    displayFrac :: (Int,Double) -> String
    displayFrac (a,b) = printf "%s" (show a++" "++show b)


    --Fonction qui prend la liste des tuple de fractiles avec leurs priorités et la convertit en une chaine 
    --[(2,0.3),(3,0.88),(4,0.68),(5,0.12)] ==> 2 0.3
                                            --3 0.88
                                            --4 0.68
                                            --5 0.12
    displayFrac' :: [(Int, Double)] -> [String]
    displayFrac' = map displayFrac


    --Fonction qui combine le tout pour afficher la chaine finale des fractiles avec leurs priorités
    -- elle prend la chaine initiale avec son temps consultation
    chaineFractile :: String -> Int -> String
    chaineFractile chaine a = unlines (displayFrac'(convertTupleFrac(calculFractil (donneeFractileTout(comparerParPriorite (groupElemMemePriorite (tupleInfo (sortString chaine) a)))))))
    
    
    -- Fontion qui affiche le resultat final les patients ordonnés selon la premiere regle ainsi leurs fractile et la moyenne geometrique
    printResult :: Int -> String -> String
    printResult a chaine = premiereRegle chaine ++ "--------\n" ++ (chaineFractile chaine a) ++ printf "%.4f" (algoTriage a chaine)




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