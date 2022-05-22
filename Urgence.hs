
module Main where
    import System.Environment ( getArgs )

    import Data.List
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

    -- La fonction traitement est le point de départ de votre logiciel.
    -- @param tc :: Int
    --        Ce paramètre contient le temps moyen de consultation en minute.
    -- @param contenu :: String
    --        Ce paramètre contient le contenu du fichier en entrées.
    -- @return :: String
    --        Cette fonction retourne la chaîne de caractères qui sera affichée
    --        a la fin de l'exécution du programme.  Elle devra contenir le nouvel
    --        ordre des patients et la table des fractiles formatés pour l'affichage.
    --traitement :: Int -> String -> String
    --traitement tc contenu = 




   -- data Patient {patientId :: Int, patienTime :: Int, patientPriority :: Int}

    --if i have a String like :
    -- "43525 5 2\n25545 7 5\n7455 3 4"
    -- this will convert it to : 
    -- ["43525 5 2", "25545 7 5", "7455 3 4"]
    formaterDonnes :: String -> [String]
    formaterDonnes = lines



    -- map words ["43525 5 2", "25545 7 5", "7455 3 4"]
    -- [["43525","5","2"],["25545","7","5"],["7455","3","4"]]
    formaterDonnes2 :: [String] -> [[String]]
    formaterDonnes2 = map words


    parserInt :: [[String]] -> [[Int]]
    parserInt = map (map (read :: String -> Int))
    
    sortLGT x y = compare (x!!2) (y!!2) -- compare priorities 
        <> compare (y!!1) (x!!1) -- compare time in descending order
        