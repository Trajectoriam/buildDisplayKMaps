import Data.Char
import Data.List
import Data.Maybe


toBinary :: Int -> Int
toBinary n
    = toBinary' n 0
    where
        binBase = 2
        decBase = 10
        toBinary' x pow
            | x == 0          = 0
            | remainder == 1 = (decBase ^ pow) + nextVal
            | otherwise      = nextVal 
            where
                (x', remainder) = quotRem x binBase
                nextVal         = toBinary' x' (pow+1)
                
                
mySequence n 
    = toBinary (1 + (n `mod` 30))

---------------------------------------------------------

type DATA = Integer
type Q2   = Integer
type Q1   = Integer
type Q0   = Integer

type Dn = Integer
type D2 = Integer
type D1 = Integer
type D0 = Integer

type Row = (DATA, Q2, Q1, Q0, D2, D1, D0)
rowLength = 7
type STT = [Row]
--STT = State Transition Table

type PRow = ((DATA, Q2), ((Q1, Q0), (D2, D1, D0)))
type PSTT = [PRow]
--PRow = Parsed Row

lookUp :: (Eq a, Show a) => a -> [(a, b)] -> [b]
--return a list of all values with the same key value given
lookUp _ []
    = []
lookUp v ((v', i) : rs)
    | v == v' = i : lookUp v rs 
    | otherwise = lookUp v rs
	
lookUpUnique :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUpUnique
	= (fromJust .) . lookup

parseSTT :: STT -> PSTT
parseSTT rs
    = [((dat, q2), ((q1, q0), (d2, d1, d0))) | (dat, q2, q1, q0, d2, d1, d0) <- rs]
    

pairs :: [(DATA, DATA)]
pairs = [(0,0), (0,1), (1,1), (1,0)] 

numberKMaps = 3
buildKarnaughMaps :: STT -> [[DATA]]
buildKarnaughMaps stt
    --Pre: STT contains no duplicate rows
    = [[getDataOfDN c r pstt dn | c <- pairs, r <- pairs] | dn <- [0..numberKMaps-1]]
    where
        pstt = parseSTT stt
    
getDataOfDN :: (DATA, Q2) -> (Q1, Q0) -> PSTT -> Dn -> DATA 
getDataOfDN c r pstt dn 
    = getDVal dn dTriple
    where
        (dTriple : []) = lookUp r (lookUp c pstt)
        getDVal 0 (_, _, n)
            = n 
        getDVal 1 (_, n, _)
            = n 
        getDVal 2 (n, _, _)
            = n

printKMapRow :: [DATA] -> (DATA, DATA) -> IO()
printKMapRow row (rId1, rId2)
    = putStrLn (concat (rowName : (" | " : [showThis v ++ " | " | v <- row])))
    where
        showThis v
            | v == x    = "x"
            | otherwise = show v
        rId1' = show rId1
        rId2' = show rId2
        rowName = rId1' ++ rId2'

kMapDimension = 4
printKMap :: [DATA] -> IO()
printKMap kmap
    --Pre: each row has 4 values
    --4x4 KMap
    = do
    printKMapTop
    printKMap' (zip kMapRows pairs)
    where
        kMapRows = getKMapRows kmap
        getKMapRows :: [DATA] -> [[DATA]]
        getKMapRows kmap
            | null row = []
            | otherwise = row : getKMapRows rows
            where
                (row, rows) = splitAt kMapDimension kmap
                
        printKMap' []
            = putStr("")
        printKMap' ((r, rId) : otherRows)
            = do
            printKMapRow r rId
            printKMap' otherRows
            
printKMapTop :: IO()
printKMapTop
    = putStrLn (concat ("   | " : [(show cId1) ++ (show cId2)  ++  "| " | (cId1, cId2) <- pairs]))

        
printKMaps :: [[DATA]] -> IO()
printKMaps kmaps
    = printKMaps' kmaps 0
    where
        printKMaps' [] _
            = putStrLn("")
        printKMaps' (kmap : kmaps) n
            | n >= numberKMaps = putStrLn("")
            | otherwise = do
                        putStrLn ('D' : show n)
                        printKMap kmap
                        putStrLn ("")
                        printKMaps' kmaps (n+1)
    

displayKMapsForSTT stt
    = printKMaps kmaps
    where
        kmaps = buildKarnaughMaps stt
    
stringToSTT :: String -> STT
--Pre: String is well-formed <=> has 7 non-whitespace characters
stringToSTT s
    = read ('[' : ('(' : (x : stringToSTT' xs 1))) :: STT
    where
        (x : xs) = dropWhile isSpace s

        stringToSTT'    [] _
            = ")]"
        
        stringToSTT'    cs 7
            --rowLength == 7
            = ')' : ( ',' : ('(' : (c : stringToSTT'    cs' 1)))
            where
                (c : cs') = dropWhile isSpace cs
                
        stringToSTT' cs rl
            | c == xTableRep  = stringToSTT' (xCharRep : cs') rl
            | otherwise       = ','  : (c : stringToSTT' cs' rl')
            where
                (c : cs') = dropWhile isSpace cs
                rl'       = rl + 1
				

checkChange :: [(Integer, Integer)] -> (Maybe Integer, Maybe Integer)
--Nothing if value changed
checkChange (xy@(x, y) : xys)
	--Pre: list passed is non-empty
	= (return x x', return y y')
	where
		(x', y') = foldr (\(b1, b2) (b1', b2') -> (funX b1 b1', funY b2 b2')) xy xys
		checkFun n = lookUpUnique n [(0, (+)), (1, (*))]
		funX = checkFun x
		funY = checkFun y
		return n n'
			| n == n' = Just n
			| otherwise = Nothing

getMinExp :: [(DATA, Q2)] -> [(Q1, Q0)] -> String
getMinExp [] []
	= ""
getMinExp dq2 q1q0
	| null q1q0 = foldr1 format [conv d "DATA", conv q2 "Q2"]
	| null dq2  = foldr1 format [conv q1 "Q1", conv q0 "Q0"]
	| otherwise = foldr1 format [conv d "DATA", conv q2 "Q2", conv q1 "Q1", conv q0 "Q0"]
	where
		(d, q2)  = checkChange dq2 
		(q1, q0) = checkChange q1q0 
		
		conv (Just n) id
			| n == 1 = id 
			| otherwise = '~' : id
			
		conv Nothing _
			= ""
			
		format s1 s2
			| null s1 = s2
			| null s2 = s1
			| otherwise = concat [s1, " * ", s2]



    
toCharOffSet = 48
x = 3
xTableRep = 'x'
xCharRep = chr (fromInteger (x + toCharOffSet))
----
--STT 1
----
mySTT
    = [
        (0        ,0    ,0    ,0        ,1    ,1    ,1),
        (0        ,0    ,0    ,1        ,1    ,1    ,1),
        (0        ,0    ,1    ,0        ,0    ,1    ,1),
        (0        ,0    ,1    ,1        ,1    ,1    ,1),
        (0        ,1    ,0    ,0        ,1    ,1    ,1),
        (0        ,1    ,0    ,1        ,0    ,0    ,0),
        (0        ,1    ,1    ,0        ,x    ,x    ,x),
        (0        ,1    ,1    ,1        ,1    ,1    ,1),
        (1        ,0    ,0    ,0        ,0    ,0    ,1),
        (1        ,0    ,0    ,1        ,0    ,1    ,0),
        (1        ,0    ,1    ,0        ,1    ,1    ,1),
        (1        ,0    ,1    ,1        ,1    ,0    ,0),
        (1        ,1    ,0    ,0        ,1    ,0    ,1),
        (1        ,1    ,0    ,1        ,1    ,0    ,1),
        (1        ,1    ,1    ,0        ,x    ,x    ,x),
        (1        ,1    ,1    ,1        ,1    ,1    ,1)
      ]
     
mySTTString
  =  "0    0  0  0    1  1  1\
    \0    0  0  1    1  1  1\
    \0    0  1  0    0  1  1\
    \0    0  1  1    1  1  1\
    \0    1  0  0    1  1  1\
    \0    1  0  1    0  0  0\
    \0    1  1  0    x  x  x\
    \0    1  1  1    1  1  1\
    \1    0  0  0    0  0  1\
    \1    0  0  1    0  1  0\
    \1    0  1  0    1  1  1\
    \1    0  1  1    1  0  0\
    \1    1  0  0    1  0  1\
    \1    1  0  1    1  0  1\
    \1    1  1  0    x  x  x\
    \1    1  1  1    1  1  1"

        
mySTTStringCW1
    =     "0    0    0    0    1    1    1\
        \0    0    0    1    0    0    0\
        \0    0    1    0    0    0    0\
        \0    0    1    1    1    1    1\
        \0    1    0    0    1    1    1\
        \0    1    0    1    1    1    1\
        \0    1    1    0    0    0    0\
        \0    1    1    1    0    0    0\
        \1    0    0    0    0    0    0\
        \1    0    0    1    0    0    0\
        \1    0    1    0    1    1    1\
        \1    0    1    1    0    0    0\
        \1    1    0    0    1    1    1\
        \1    1    0    1    0    0    0\
        \1    1    1    0    1    1    1\
        \1    1    1    1    1    1    1"

sttSpecString
    =    "0  0 0 0  1 1 1\
        \0  0 0 1  1 1 1\
        \0 0 1 0  xxx\
        \0  0 1 1  1 1 0\
        \0 1 0 0  xxx\
        \0 1 0 1  xxx\
        \0  1 1 0  0 0 0\
        \0  1 1 1  1 1 1\
        \1  0 0 0  0 0 1\
        \1  0 0 1  0 1 1\
        \1 0 1 0  xxx\
        \1  0 1 1  1 1 1\
        \1 1 0 0  xxx\
        \1 1 0 1  xxx\
        \1  1 1 0  1 1 0\
        \1  1 1 1  1 1 1"


      
sttSpec
 = [
        (0  ,0 ,0 ,0  ,1 ,1 ,1),
        (0  ,0 ,0 ,1  ,1 ,1 ,1),
        (0 ,0 ,1 ,0 ,x ,x ,x),
        (0  ,0 ,1 ,1  ,1 ,1 ,0),
        (0 ,1 ,0 ,0 ,x ,x ,x),
        (0 ,1 ,0 ,1 ,x ,x ,x),
        (0  ,1 ,1 ,0  ,0 ,0 ,0),
        (0  ,1 ,1 ,1  ,1 ,1 ,1),
        (1  ,0 ,0 ,0  ,0 ,0 ,1),
        (1  ,0 ,0 ,1  ,0 ,1 ,1),
        (1 ,0 ,1 ,0 ,x ,x ,x),
        (1  ,0 ,1 ,1  ,1 ,1 ,1),
        (1 ,1 ,0 ,0 ,x ,x ,x),
        (1 ,1 ,0 ,1 ,x ,x ,x),
        (1  ,1 ,1 ,0  ,1 ,1 ,0),
        (1  ,1 ,1 ,1  ,1 ,1 ,1)
      ]
      
sttRowString1
    = "0  0    0 0   1  1 1"
sttRowString2
    = "0  1    0 0   x  1 x"
sttRowString3
    = sttRowString1 ++ sttRowString2


kmaps1 = [1,1,1,1,1,0,1,3,1,1,1,3,1,0,0,1]

