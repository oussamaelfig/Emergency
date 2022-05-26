    import Data.List ( elemIndex, sortBy )

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



    --Creation d'un data Type Patient
    data Patient = Patient { patientId :: Int, patientTemps :: Int, patientPriorite :: Int } deriving (Show)



    --Convertir ma liste des listes de Int en une liste de Tuples contenant les éléments à afficher
    --[[43525,5,2],[7455,3,4],[25545,7,5]] --> [(1,43525,5),(2,7455,3),(3,25545,7)]
    convertTuple :: [[Int]] -> [(Int, Int, Int)]
    convertTuple (x:xs) = zipWith (curry (\x -> (fst x,(snd x)!!0, (snd x)!!2))) [1..] (x:xs)



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