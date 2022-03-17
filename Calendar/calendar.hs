module Main where

import System.Environment (getArgs)

type Year     = Int -- 0..
type Month    = Int -- 1..12
type Day      = Int -- 1..31
type Date     = (Day,Month,Year)
type Weekday  = Int -- 0..6

type Calendar = ( Year, [Weekday] ) -- Number for first weekday of that month
              
monthNames :: [String]
monthNames = [ "January"   , "February" , "March"    , "April"
             , "May"       , "June"     , "July"     , "August" 
             , "September" , "October"  , "November" , "December"
             ]        

weekdays :: [String]
weekdays = ["su","mo","tu","we","th","fr","sa"]

calendarForYear :: Year -> Calendar
calendarForYear year = ( year, [ firstDayOfMonth year m | m <- [1..12] ] )

isLeapYear :: Year -> Bool
isLeapYear year
    | 400 `devides` year = True
    | 100 `devides` year = False
    |   4 `devides` year = True
    | otherwise          = False
    where
        devides :: Int -> Int -> Bool
        devides n y = y `mod` n == 0

daysPerMonth :: Year -> [Day]
daysPerMonth year = [31,(febDays year),31,30,31,30,31,31,30,31,30,31] 
    where 
        febDays :: Year -> Day
        febDays year = if isLeapYear year then 29 else 28 
      
daysInMonth :: Month -> Year -> Day
daysInMonth month year = (daysPerMonth year) !! (month-1)
   
calenderMonth :: Month -> Year -> Weekday -> [String]
calenderMonth year month firstday = title : body
   where   
      title :: String
      title = cjustify 22 (monthNames !! (month-1) ++ " " ++ show year)
      
      body :: [String]
      body = take 6 . map (concat . separateBy " ") $ makeGroupsOf 7 boxes
             where 
               -- boxes are 2 character strings
               boxes      =  weekdays ++ startSpace ++ days ++ endSpace 
               startSpace = replicate firstday "  "
               days       = map (rjustify 2 . show) [1..daysInMonth month year] 
               endSpace   = repeat "  "
               
      -- String manipulation library
      rjustify :: Int -> String -> String
      rjustify i s = replicate (i - length s) ' ' ++ s

      cjustify :: Int -> String -> String
      cjustify i s = let sp :: String
                         sp = replicate ((i - length s) `div` 2) ' '
                     in take i (sp ++ s ++ repeat ' ')

firstDayOfMonth :: Year -> Month -> Int
firstDayOfMonth year month  = sum ( year : nrOfLeapYears : daysThisYear ) `mod` 7
    where 
        daysThisYear :: [ Day ]
        daysThisYear = [ daysInMonth m year | m <- [1..(month-1)] ]
      
        nrOfLeapYears :: Int
        nrOfLeapYears = (y `div` 4) - (y `div` 100) + (y `div` 400)
            where
                y = year -1 


separateBy :: a -> [a] -> [a]
separateBy sep xs = sep : concatMap (\x -> [x,sep]) xs

makeGroupsOf :: Int -> [a] -> [[a]]
makeGroupsOf _ [] = []
makeGroupsOf i xs = take i xs : makeGroupsOf i (drop i xs)        

-- could be instance of Show later
showCalender :: Calendar -> String
showCalender ( year, firstdays ) = unlines 
                         . concat 
                         . separateBy horizontal 
                         . map besides 
                         . makeGroupsOf 3 
                         $ map (\month -> calenderMonth year month $ firstdays !! (month-1) ) [1..12]
    where 
      besides :: [[String]] -> [String]
      besides xxs = foldr1 (zipWith (++)) $ separateBy (repeat "|") xxs

      horizontal :: [String]
      horizontal = [concat (separateBy "+" (replicate 3 (replicate 22 '-')))]
 
main :: IO ()
main = do 
          args <- getArgs
          let year     = (read $ head args)::Year
              calendar = calendarForYear year
          putStrLn (showCalender calendar)
           
