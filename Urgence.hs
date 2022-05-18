
module Main where
    import System.Environment

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
    traitement :: Int -> String -> String
    traitement tc contenu = 