data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown deriving Show

data TMDirection = Past | Future | Both

data TimeMachine = TimeMachine String String Integer String TMDirection Float

clientName :: Client -> Maybe String
clientName client = case client of
                        GovOrg name                         -> Just name
                        Company name _ _ _                  -> Just name
                        Individual (Person fname lname _) _ -> Just $ fname ++ " " ++ lname
                        _                                   -> Nothing

genderCounter clients = printGender $ genderIdentify (0, 0) clients 
                            where
                                genderIdentify (m, f) [] = (m, f)
                                genderIdentify (m, f) (x:xs) = case x of
                                    Individual (Person _ _ g) _  -> 
                                        case g of
                                            Male   -> genderIdentify (m + 1, f) xs
                                            Female -> genderIdentify (m, f + 1) xs
                                    Company _ _ (Person _ _ g) _ -> 
                                        case g of
                                            Male   -> genderIdentify (m + 1, f) xs
                                            Female -> genderIdentify (m, f + 1) xs
                                    _                            -> genderIdentify (m, f) xs
                                printGender (m, f) = "we have " ++ show m ++ " male and " ++ show f ++ " female client"
main = do let clients = [GovOrg "NSA",
                        Company "CIA" 12 (Person "John" "Nash" Male) "Head",
                        Individual (Person "Eric" "Bana" Male) True] 
             in print $ genderCounter clients