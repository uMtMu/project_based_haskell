data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown deriving Show

data TMDirection = Past | Future | Both deriving Show

data TimeMachine = TimeMachine String Integer String TMDirection Float deriving Show

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

discountTimeMachine percentage tms = map (discount percentage) tms
                                        where discount percentage (TimeMachine manufacturer model name direction price) = TimeMachine manufacturer model name direction (price * percentage)
    
main = do let clients = [GovOrg "NSA",
                        Company "CIA" 12 (Person "John" "Nash" Male) "Head",
                        Individual (Person "Eric" "Bana" Male) True]
              timeMachines = [TimeMachine "EasyGo" 1999 "Go1" Past 100.50,
                              TimeMachine "EasyGo" 2050 "Go5" Future 2500.75]
             in print $ discountTimeMachine 0.80 timeMachines