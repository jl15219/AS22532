import System.Environment
import System.Process
import qualified Data.ByteString as BS
--import System.Entropy

type ByteString = BS.ByteString

data Binary = Zero | One

--              n          x
euclidsInv :: Integer -> Integer -> Integer
euclidsInv n m' = (fst . snd) (foldr f (g (last l)) (init l)) where
  m = mod m' n
  l = (euclidsInv' n m)
  g :: (Integer,(Integer,Integer),Integer) -> ((Integer,Integer),(Integer,Integer))
  g (n,(d,m),one) | one == 1 = ((1,n),(-d,m))
                  | otherwise = undefined
  f :: (Integer,(Integer,Integer),Integer) -> ((Integer,Integer),(Integer,Integer)) -> ((Integer,Integer),(Integer,Integer))
  f (n,(d,m),r) ((t,o),(i,j)) = ((i,n),(t+i*((-1)*d),o))

euclidsInv' :: Integer -> Integer -> [(Integer,(Integer,Integer),Integer)]
euclidsInv' n m | mod n m == 1 = [(n,(div n m,m), 1)]
                | otherwise    = (n,(div n m,m),mod n m):(euclidsInv' m (mod n m))


znAdd :: Integer -> Integer -> Integer -> Integer
znAdd x y n | t > n = t - n
            | otherwise = t where
              t = x + y

znSub :: Integer -> Integer -> Integer -> Integer
znSub x y n | y > x = n - (y - x)
            | otherwise = x - y

montExpRed :: Integer -> [Binary] -> Integer -> Integer -> (Integer,Bool)
montExpRed x y n b = (rs',red) where
  (rs',_) = montMulRed res 1 n omega b
  (v,red) = montMulRed res res n omega b
  res     = montExp' xp y n rho omega b tp
  omega   = euclidsInv rho (n*(-1))
  rho     = rhoFunc b b n
  rho2    = mod (rho^2) n
  (tp,_)  = montMulRed 1 rho2 n omega b
  (xp,_)  = montMulRed x rho2 n omega b
  rhoFunc :: Integer -> Integer -> Integer -> Integer
  rhoFunc b bk n | bk > n    = bk
                 | otherwise = rhoFunc b (b*bk) n

montMulTest :: Integer -> Integer -> Integer -> Integer -> Integer
montMulTest x y n b = result where
  (result,_) = montMulRed v 1 n omega b
  (v,_)      = montMulRed xp yp n omega b
  omega      = mod (euclidsInv rho (n*(-1))) rho
  rho        = rhoFunc b b n
  rho2       = mod (rho^2) n
  (yp,_)     = montMulRed y rho2 n omega b
  (xp,_)     = montMulRed x rho2 n omega b
  rhoFunc :: Integer -> Integer -> Integer -> Integer
  rhoFunc b bk n | bk > n    = bk
                 | otherwise = rhoFunc b (b*bk) n

montExp' :: Integer -> [Binary] -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
montExp' x [] n rho omega b t     = t
montExp' x (One:ys)  n rho omega b t = fst (montMulRed (fst (montMulRed t t n omega b)) x n omega b)
montExp' x (Zero:ys) n rho omega b t = (fst (montMulRed t t n omega b))

montMulRed :: Integer -> Integer -> Integer -> Integer -> Integer -> (Integer,Bool)
montMulRed x y n omega b | res > n   = (res - n, True)
                         | otherwise = (res,    False)
                       where
                         res = montMul' x y n omega b 0 (ceiling (logBase (fromIntegral b) (fromIntegral n))) 0

montMul' :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
montMul' x y n omega b i ln r | i == ln   = r
                              | otherwise = montMul' x y n omega b (i+1) ln r_new
                          where
                            r_new = div (r + y_i*x + u_i * n) b
                            u_i   = ((r_0 + y_i*x_0) * omega)
                            x_0   = mod x b
                            y_i   = mod (div y b^i) b
                            r_0   = mod r b

-- Converts a binary string into an integer
-- NEEDS TO BE SWAPPED IN THE CASE THAT THE EXPANSIATION IS THE WRONG WAY ROUND
bs2Int :: [Binary] -> Integer
bs2Int = f {- . reverse -} where
  f :: [Binary] -> Integer
  f []        = 0
  f (One:bs)  = 1 + (f bs)*2
  f (Zero:bs) = (f bs)*2

-- Interacts with an executable via the string input
interactExe :: FilePath -> String -> IO String
interactExe f s = do (_,c,s) <- (readProcessWithExitCode f [] (s ++ "^C\n"))
                     return c

messageSamples = [2,6..3000]

-- Queries a file with the given string to recieve a list of the outputs
queryFile :: FilePath -> String -> IO [(Integer,Integer)]
queryFile fp msgs = do strVals <- interactExe fp msgs
                       return (undoD strVals)

-- Used to convert a list of integer inputs into the string used for file querys
valsForQuery :: [Integer] -> String
valsForQuery msgs = (concat ((map ((++ "\n") . toHex)) msgs))

-- Formats and groups the output from an exectuable file into correct format
-- recieved ouptput  time    value
undoD :: String -> [(Integer,Integer)]
undoD str = (reverse . tail . snd) (foldl (flip f) (True,[(0,0)]) str) where
  f :: Char -> (Bool,[(Integer,Integer)]) -> (Bool,[(Integer,Integer)])
  f '\n' (True, (time,value):as)  = (False,(time,value):as)
  f '\n' (False,as)               = (True, (0,0):as)
  f c    (True,  (time,value):as) = (True,((time*10) + (read [c]), value):as)
  f c    (False, (time,value):as) = (False,(time,(value*16) + (deHexChar c)):as)

-- A fail that needs to be updated to generate random numbers for Ms
randomNum :: IO ByteString
randomNum = do BS.readFile "/dev/urandom"

-- Conducts a time attack against the given executable files
-- meaning :: ${User}.D   ${User}.R   N          E          Current D    M          .D: time    result      IO Actual D, File interactions
timeAttack :: FilePath -> FilePath -> Integer -> Integer -> [Binary] -> [Integer] -> [(Integer,Integer)] -> IO (Integer,Integer)
timeAttack d r n e bs ms tvs = do rtvs <- queryFile r (genRQueries ms n (bs2Int bs))
                                  mis  <- return (calcI ms rtvs)
                                  (increaseMs,dist) <- testSize mis
                                  (ms2, rtvs2, mis2, tvs2) <- genMoreMs increaseMs dist
                                  indicator <- meanFTest (ms ++ ms2) (mis ++ mis2) (tvs ++ tvs2)
                                  (test,correctD) <- return (compDif n e (indicator:bs)) --compDif ms (indicator:bs) tvs n
                                  if test
                                    then return (correctD, (fromIntegral . length) (tvs ++ tvs2))
                                    else timeAttack d r n e (indicator:bs) (ms ++ ms2) (tvs ++ tvs2)

-- Compares the difference between the conisdered d and the real thing
compDif :: Integer -> Integer -> [Binary] -> (Bool,Integer)
compDif n e bs | f n e d1  = (True, d1)
               | f n e d2  = (True, d2)
               | otherwise = (False, 0) where
                 d1 = bs2Int (One:bs)
                 d2 = bs2Int (Zero:bs)
                 f :: Integer -> Integer -> Integer -> Bool
                 f n e d = (mod (d*e) n) == 1
{-
compDif :: [Integer] -> [Binary] -> [(Integer,Integer)] -> Integer -> IO (Bool,[Binary])
compDif as d cs n = do b1 <- (f as (bs2Int (One:d)) cs n 2)
                       if b1 then return (b1,(One:d))
                       b2 <- (f as (bs2Int (Zero:d)) cs n 2)
                       if b2 then return (b2,Zero:d)
                       return False where
  f :: [Integer] -> Integer -> [(Integer,Integer)] -> Integer -> Integer -> IO Bool
  f [] d []
  f ms d cs n taker = do
    rtvs <- queryFile r (genRQueries ms1 n d)
    cont <- g rtvs cs1
    if cont
      then f ms2 d cs2 n (taker*2)
      else return False
  g :: [(Integer,Integer)] -> [(Integer,Integer)] -> Bool
  g [] _ = True
  g _ [] = True
  g ((_,a):as) ((_,b):bs) | a == b    = g as bs
                          | otherwise = False
  (cs1,cs2) = splitAt taker cs
  (ms1,ms2) = splitAt taker ms
-}

-- Checks the Fi values and their means to find out what the kth bit is
meanFTest :: [Integer] -> [(Binary,Binary)] -> [(Integer,Integer)] -> IO Binary
meanFTest ms mis tvs = undefined

-- Tests the relevant values of the list to see if a mean is reasonable
-- it also provides outputs for i in position ((1,2),(3,4)) to know what needs
-- to be updated
testSize :: [(Binary,Binary)] -> IO (Bool,((Integer,Integer),(Integer,Integer)))
testSize as = return (test v, v) where
  v :: ((Integer,Integer),(Integer,Integer))
  v =  foldr f ((0,0),(0,0)) as
  f :: (Binary,Binary) -> ((Integer,Integer),(Integer,Integer)) -> ((Integer,Integer),(Integer,Integer))
  f (Zero,Zero) ((a,b),(c,d)) = ((a + 1,b),(c,d))
  f (Zero, One) ((a,b),(c,d)) = ((a,b + 1),(c,d))
  f (One, Zero) ((a,b),(c,d)) = ((a,b),(c + 1,d))
  f (One,  One) ((a,b),(c,d)) = ((a,b),(c,d + 1))
  test :: ((Integer,Integer),(Integer,Integer)) -> Bool
  test ((a,b),(c,d)) | a < lim   = False
                     | b < lim   = False
                     | c < lim   = False
                     | d < lim   = False
                     | otherwise = True
  lim = 5
  -- MEAN LIMIT

-- Generates more Ms such that the mean becomes reasonable
genMoreMs :: Bool -> ((Integer,Integer),(Integer,Integer)) -> IO ([Integer],[(Integer,Integer)],[(Binary,Binary)],[(Integer,Integer)])
genMoreMs = undefined

-- Queries the R exectuable file
genRQueries :: [Integer] -> Integer -> Integer -> String
genRQueries ms n bs = undefined

-- Calculates whether the m value should belong to group 1, 2, 3 or 4
calcI :: [Integer] -> [(Integer,Integer)] -> [(Binary,Binary)]
calcI [] []                     = []
calcI [] xs                     = undefined
calcI xs []                     = undefined
calcI (m:ms) ((time,mTemp):rts) = (f m mTemp):(calcI ms rts) where
  f :: Integer -> Integer -> (Binary,Binary)
  f m mTemp = undefined

-- Main exectuable that is being run
main :: IO ()
main = do (dTemp:(conf:args)) <- getArgs
          (n,e) <- getConf conf
          d <- return ("./" ++ dTemp)
          r <- return (init d ++ "R")
          out <- interactExe d "AB3425\n"
          putStr (toHex n ++ "\n")
          putStr (toHex e ++ "\n")
          --rand <- getEntropy 10
          --putStr (show rand)
          return ()

-- gets the config information from the given file
getConf :: FilePath -> IO (Integer,Integer)
getConf f = do s <- readFile f
               return (seperate s)

-- Used to split a string by the new line into two seperate values
seperate :: String -> (Integer,Integer)
seperate s = f s [] where
    f :: String -> String -> (Integer,Integer)
    f ('\n':s) k = (deHex (reverse k), deHex s)
    f (a:as) k = f (as) (a:k)

runTimes :: Integer -> IO a -> IO a
runTimes 0 a = a
runTimes n a = do f <- a
                  fs <- (runTimes (n-1) a)
                  return fs

--bigTest :: String
--bigTest = toHex ( mod (deHex "707EED4382875E2D581D39045A078BBFB506570097466D0C83E6641BFAE1F7F76F52293CD0EC659FF1606CFF8229C264BD36FA448F0DBE90F95C21EEC7279016DB7510381E4A5DE742E0CC1D2C760076D4D2F3419D3C66A1144205442938C3E19BC7D58028B75A16E19103741E4BF0E04AB56E7E36ABA0BA8F337B01DC554EFB" * (deHex "707EED4382875E2D581D39045A078BBFB506570097466D0C83E6641BFAE1F7F76F52293CD0EC659FF1606CFF8229C264BD36FA448F0DBE90F95C21EEC7279016DB7510381E4A5DE742E0CC1D2C760076D4D2F3419D3C66A1144205442938C3E19BC7D58028B75A16E19103741E4BF0E04AB56E7E36ABA0BA8F337B01DC554EFB")) (deHex "707EED4382875E2D581D39045A078BBFB506570097466D0C83E6641BFAE1F7F76F52293CD0EC659FF1606CFFA9C264BD36FA448F0DBE90F95C21EEC7279016DB7510381E4A5DE742E0CC1D2C760076D4D2F3419D3C66A1144205442938C3E19BC7D58028B75A16E19103741E4BF0E04AB56E7E36ABA0BA8F337B01DC554EF"))

deHex :: String -> Integer
deHex as = f (reverse as) where
    f :: String -> Integer
    f [] = 0
    f ('\n':as) = f as
    f (a:as) = deHexChar a + 16*(f as)

deHexChar :: Char -> Integer
deHexChar '0' = 0
deHexChar '1' = 1
deHexChar '2' = 2
deHexChar '3' = 3
deHexChar '4' = 4
deHexChar '5' = 5
deHexChar '6' = 6
deHexChar '7' = 7
deHexChar '8' = 8
deHexChar '9' = 9
deHexChar 'A' = 10
deHexChar 'B' = 11
deHexChar 'C' = 12
deHexChar 'D' = 13
deHexChar 'E' = 14
deHexChar 'F' = 15

toHex :: Integer -> String
toHex n = reverse (toHex' n)

toHex' :: Integer -> String
toHex' 0 = []
toHex' n = (f (mod n 16)):(toHex' (div n 16)) where
    f :: Integer -> Char
    f n | n == 10 = 'A'
        | n == 11 = 'B'
        | n == 12 = 'C'
        | n == 13 = 'D'
        | n == 14 = 'E'
        | n == 15 = 'F'
        | otherwise = head (show n)
