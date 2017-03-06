import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
        ,(101,(Free,"JAH3I"))
        ,(103,(Free,"IQSA9"))
        ,(105,(Free,"QOTSA"))
        ,(109,(Taken,"893JJ"))
        ,(110,(Taken,"99292"))
        ]


data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"