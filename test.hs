    import Data.List ( elemIndex, sortBy )
    import Data.Maybe ( catMaybes )



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


    --f(i+1)=t(i+1)+i*15


    --display id 



--     when you have a small number of known shapes, you can extract it with pattern matching, eg
--   let f list =
--         case list of
--           ((_:x:_):_) -> show x
--           _           -> "bad data"
--   putStrLn $ f [[43525,5,2],[7455,3,4]] 


--[[43525,5,2],[7455,3,4]] !! 0 !! 1

--    getIndicies :: [Int] -> [Int]
--    getIndicies arr = catMaybes $ map (\elem -> elemIndex elem arr) arr



--Creating patient data type
    data Patient = Patient {
                        patientId :: Int,
                        patientTemps :: Int,
                        patientPriorite :: Int
                        } deriving (Show)  

  --  bar :: [[Int]] -> [[Patient]]
   -- bar cs = map foo cs