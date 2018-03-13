import System.Environment
import System.Process

type Info = (Integer, Integer, Integer, Integer, Integer, FilePath)

fastModExp :: Integer -> Integer -> Integer -> Integer
fastModExp x 1 n = mod x n
fastModExp x e n | mod e 2 == 1 = mod (fastModExp x (e-1) n) n
                 | otherwise    = mod ((fastModExp x (div e 2) n)^2) n

inc2Tup :: (a,Integer) -> (a,Integer)
inc2Tup (a,n) = (a,n+1)

-- n, e, d, k = log_256(n), B = 2^(8*(k-1))
-- access to (n,e) and oracle that says if y == x^d (mod n) is less than B
--               e          c          n
attackOAEP :: Info -> IO (Integer,Integer)
attackOAEP info = do (f_1, q1) <- attackStep1 info
                     putStr (show f_1 ++ "\n")
                     (f_2, q2) <- attackStep2 f_1 info
                     putStr (show f_2 ++ "\n")
                     ((mn,mx), q3) <- attackStep3 f_2 info
                     if mn == mx then return (mn, q1+q2+q3)
                     else undefined

attackStep1 :: Info -> IO (Integer,Integer)
attackStep1 = attackStep1' 2
--               f_1          e          c          n          B         f_1
attackStep1' :: Integer -> Info -> IO (Integer,Integer)
attackStep1' f_1 v@(e, c, n, b, l, f) = do ltb <- oracle (mod ((fastModExp f_1 e n)*c) n) v
                                           --putStr (show ltb ++ "\n")
                                           if (ltb == 2)
                                             then return (div f_1 2, 1)
                                             else do (f_1',inc) <- (attackStep1' (f_1*2) v)
                                                     return (f_1', inc + 1)

                                  --  | oracle (mod ((f_1^e)*c) n) = div f_1 2
                                  --  | otherwise = attackStep1' (f_1*2) v


attackStep2 :: Integer -> Info -> IO (Integer,Integer)
attackStep2 f v@(e, c, n, b, l, _) = attackStep2' ((div (n + b) b) * f) f v
--              f_2         f_1/2             f_2
attackStep2' :: Integer -> Integer -> Info -> IO (Integer,Integer)
attackStep2' f_2 f v@(e, c, n, b, l, _) = do ltb <- oracle f_2 v
                                             if (ltb == 2)
                                               then fmap inc2Tup (attackStep2' (f_2 + f) f v)
                                               else return (f_2,1)
                                  -- | oracle f_2 = attackStep2' (f_2 + f) f v
                                  -- | otherwise = f_2

--              f_2                 m_min   m_max
attackStep3 :: Integer -> Info -> IO ((Integer,Integer),Integer)
attackStep3 f_2 v@(e, c, n, b, l, f) = attackStep3' (((div n f_2) + 1), (div (n + b) f_2)) v

attackStep3' :: (Integer,Integer) -> Info -> IO ((Integer,Integer),Integer)
attackStep3' (mn,mx) v@(e, c, n, b, l, f) = do ltb <- oracle (f_3) v
                                               if (ltb == 2)
                                                 then if (mx /= mx' && mn /= mx')
                                                            then fmap inc2Tup (attackStep3' (mn, mx') v)
                                                            else return ((mn,mx'),1)
                                                 else if (mn /= mn' && mn' /= mx)
                                                            then fmap inc2Tup (attackStep3' (mn', mx) v)
                                                            else return ((mn',mx),1)
--  | oracle (f_3) = attackStep3' (mn, (div (i*n + b) f_3)) v
--                                       | otherwise = attackStep3' ((div (i*n + b) f_3) + 1,mx) v
                                  where
                                    f_tmp = div (2*b) (mx - mn)
                                    i     = div (f_tmp*mn) n
                                    f_3   = 1 + (div (i*n) mn)
                                    mx'   = div (i*n + b) f_3
                                    mn'   = (div (i*n + b) f_3) + 1

-- True iff '< B'
oracle :: Integer -> Info -> IO Integer
oracle c (_,ler,_,_,l,f) = do (a:as) <- interactExe f (toHex l ++ "\n" ++ (toOctet c lengthReq) ++ "\n")
                              return (read [a])
                    where
                      lengthReq = ceiling (logBase 16 (fromIntegral ler))

interactExe :: FilePath -> String -> IO String
interactExe f s = do (_,c,s) <- (readProcessWithExitCode f [] (s ++ "^C\n"))
                     return c

main :: IO ()
main = do (dTemp:(conf:args)) <- getArgs
          dFile <- return ("./" ++ dTemp)
          info <- getConf conf
          putStr (show info ++ "\n")
          ot <- attackOAEP (combInfo info dFile)
          prntScrn ot

test :: Integer -> IO ()
test n = do infoTemp <- getConf "22532.conf"
            info <- return (combInfo infoTemp "./22532.D")
            s <- oracle n info
            putStr (show s ++ "\n")

combInfo :: Info -> FilePath -> Info
combInfo (a,b,c,d,e,[]) fp = (a,b,c,d,e,fp)

prntScrn :: (Integer,Integer) -> IO ()
prntScrn (a,b) = do putStr (show a ++ "\n")
                    putStr (show b ++ "\n")

-- gets the config information from the given file
getConf :: FilePath -> IO Info
getConf f = do s <- readFile f
               return (arrToInfo (seperate s))

seperate :: String -> [Integer]
seperate s = f s 0 [] where
   f :: String -> Integer -> String -> [Integer]
   f [] _ k = []
   f ('\n':s) n k | n < 2 = (deHex (reverse k)):(f s (n+1) [])
                  | otherwise = (fromOctet (reverse k)):(f s (n+1) [])
   f (a:as) n k = f (as) n (a:k)

arrToInfo :: [Integer] -> Info
arrToInfo (n:e:l:c:xs) = (e,c,n,b,l,[]) where
  k = ceiling (logBase 256 (fromIntegral n))
  b = 2^(8*(k-1))


toOctet :: Integer -> Integer -> String
toOctet n len = (take (fromIntegral requiredSize) (repeat '0')) ++ (toHex n) where
  requiredSize = len - (ceiling ((logBase 16 (fromIntegral n)) + 1)) --reverse . toOctet'

toOctet' :: Integer -> String
toOctet' 0 = []
toOctet' n = ((head . show) (mod n 8)):(toOctet' (div n 8))

fromOctet :: String -> Integer
fromOctet = deHex -- fromOctet' . reverse

fromOctet' :: String -> Integer
fromOctet' [] = 0
fromOctet' (a:as) = (read [a]) + (8 * (fromOctet' as))


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
