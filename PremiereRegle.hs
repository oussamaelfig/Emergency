    import Data.List ( elemIndex, sortBy, groupBy )
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
    sortString :: String -> [[Int]]
    sortString x = sortBy sortLGT (parserInt (formaterDonnes2 (formaterDonnes x)))


    --Convertir ma liste des listes de Int en une liste de Tuples contenant les éléments à afficher
    --[[43525,5,2],[7455,3,4],[25545,7,5]] --> [(1,43525,5),(2,7455,3),(3,25545,7)]
    convertTuple :: [[Int]] -> [(Int, Int, Int)]
    --convertTuple (x:xs) = zipWith (curry (\x -> (fst x,head (snd x), snd x!!2))) [1..] (x:xs)
    convertTuple = zipWith (\i (a:b:_) -> (i, a, b)) [1..]



    --Fonction qui va afficher ma liste des patients ordonnée
    -- elle prend ma liste de Tuples et la converti en affichage
    -- [(1,43525,5),(2,7455,3),(3,25545,7)] --> -- Indice IdPatient priorité
                                                -- 1 43525 2
                                                -- 2 7455 4
                                                -- 3 25545 5
    display :: [(Int, Int, Int)] -> IO ()
    display = mapM_ (\(a,b,c) -> putStrLn (show a++" "++show b++" "++show c))



    --fonction qui combine toute les autres fonctions
    --prend la chaine initial et retourne la liste des patients ordonné
    -- "43525 5 2\n25545 7 5\n7455 3 4" --> -- Indice IdPatient priorité
                                                -- 1 43525 2
                                                -- 2 7455 4
                                                -- 3 25545 5
    premiereRegle :: String -> IO ()
    premiereRegle x = display (convertTuple (sortString x))



    --Pour tester :
    -- premiereRegle "43525 5 2\n25545 7 5\n7455 3 4"


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
    tupleInfo :: [[Int]] -> [(Int,Int)]
    tupleInfo [[]] = []
    tupleInfo [[_,x,y]] = [(x,y)]
    tupleInfo list =  zipWith (\[_,x,y] i -> (x+tempsAttenteParDefaut*i,y)) list [0..]


    --groupBy :: ((a0, a2) -> (a1, a2) -> Bool) -> t0 -> [[(Int, Int)]]

    --Grouper les elements par prioritée chaque priorité dans une liste à part
    --[(3,2),(17,2),(50,3),(64,3)] ==> [[(3,2),(17,2)],[(50,3),(64,3)]]
    groupElemMemePriorite :: [(Int,Int)] -> [[(Int,Int)]]
    groupElemMemePriorite = groupBy (\x y -> snd x == snd y)


