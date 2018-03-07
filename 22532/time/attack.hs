import System.Environment
import System.Process

interactExe :: FilePath -> String -> IO String
interactExe f s = do (_,c,s) <- (readProcessWithExitCode f [] (s ++ "^C\n"))
                     return c

main :: IO ()
main = do (dTemp:(conf:args)) <- getArgs
          (n,e) <- getConf conf
          d <- return ("./" ++ dTemp)
          r <- return (init d ++ "R")
          out <- interactExe d "AB3425\n"
          putStr (toHex n ++ "\n")
          putStr (toHex e ++ "\n")
          return ()

getConf :: FilePath -> IO (Integer,Integer)
getConf f = do s <- readFile f
               return (seperate s)

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
    f (a:as) = g a + 16*(f as)
    g :: Char -> Integer
    g '0' = 0
    g '1' = 1
    g '2' = 2
    g '3' = 3
    g '4' = 4
    g '5' = 5
    g '6' = 6
    g '7' = 7
    g '8' = 8
    g '9' = 9
    g 'A' = 10
    g 'B' = 11
    g 'C' = 12
    g 'D' = 13
    g 'E' = 14
    g 'F' = 15

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
