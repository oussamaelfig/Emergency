    import Data.List


    formaterDonnes :: String -> [String]
    formaterDonnes = lines



    -- map words ["43525 5 2", "25545 7 5", "7455 3 4"]
    -- [["43525","5","2"],["25545","7","5"],["7455","3","4"]]
    formaterDonnes2 :: [String] -> [[String]]
    formaterDonnes2 = map words


    --parserInt :: [[String]] -> [[Int]]
    parserInt = map (map (read :: String -> Int))
    
    sortLGT :: Ord a => [a] -> [a] -> Ordering
    sortLGT x y = compare (x!!2) (y!!2) -- compare priorities 
        <> compare (y!!1) (x!!1) -- compare time in descending order
        


        --to execute:
        --sortBy sortLGT [[43525,5,2],[25545,7,5],[7455,3,4],[3586,8,2]]


    sortString :: String -> [[Int]]
    sortString x = sortBy sortLGT (parserInt (formaterDonnes2 (formaterDonnes x)))