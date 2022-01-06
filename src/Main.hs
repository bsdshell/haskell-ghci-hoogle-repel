-- {{{ begin_fold
-- script
-- #!/usr/bin/env runhaskell -i/Users/cat/myfile/bitbucket/haskelllib
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DuplicateRecordFields #-} 
-- import Turtle
-- echo "turtle"

{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE QuasiQuotes       #-} -- support raw string [r|<p>dog</p> |]
import Text.RawString.QQ       -- Need QuasiQuotes too 


-- import Data.Set   -- collide with Data.List 
import Control.Monad
import Data.Char
import Data.Typeable (typeOf) -- runtime type checker, typeOf "k"
import qualified Data.List as L
import Data.List.Split
import Data.Time
import Data.Time.Clock.POSIX
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Files
import System.Posix.Unistd
import System.Process
import Text.Read
import Text.Regex
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import Data.IORef 
import Control.Monad (unless, when)
import Control.Concurrent 
import qualified System.Console.Pretty as SCP

import qualified Text.Regex.TDFA as TD

import System.Console.Haskeline
import qualified System.Console.ANSI as AN

import System.IO (stdin, hReady, hSetEcho, hSetBuffering, BufferMode(..))
import Control.Monad (when)

import Control.Monad.Trans
import System.Console.Haskeline

import Rainbow()
import System.Console.Pretty (Color (..), color)

import AronModule 

p1 = "/Users/cat/myfile/bitbucket/testfile/test.tex"

-- zo - open
-- za - close


{-|
main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "% "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do outputStrLn $ "Input was: " ++ input
                                return $ writeFileListAppend "/tmp/xx2.x" [input]
                                outputStrLn $ "my input => " ++ input
                                loop
-}

{-|
myGetLine :: IO String
myGetLine = do
    c <- getChar
    case c of
        '\n' -> return "" -- don't echo newlines
        _ -> do
            putChar c -- do echo everything else
            AN.cursorForward 1
            fmap (c:) myGetLine

readEvalPrintLoop :: IO()
readEvalPrintLoop = do
    line <- myGetLine -- getLine changed to myGetLine
    case line of
            "bye" -> return ()
            -- line   -> do putStrLn $ interpret line
            line   -> do putStrLn line
                         readEvalPrintLoop


main = do 
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        readEvalPrintLoop
-}

{-|
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)


-- Simple menu controller
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  key <- getKey
  when (key /= "\ESC") $ do
    case key of
      "\ESC[A" -> putStr "↑"
      "\ESC[B" -> putStr "↓"
      "\ESC[C" -> putStr "→"
      "\ESC[D" -> putStr "←"
      "\n"     -> putStr "⎆"
      "\DEL"   -> putStr "⎋"
      _        -> return ()
    main
-}


{-|
    --    data System.Console.Pretty.Color
    --      = Black
    --      | Red
    --      | Green
    --      | Yellow
    --      | Blue
    --      | Magenta
    --      | Cyan
    --      | White
    --      | Default
    --
-}
shellHighlight2::String -> [String]
shellHighlight2 s = s19
    where
        repList = [
                    ("sed",      (color Red "\\0")    ),
                    ("grep",     (color White "\\0")  ),
                    ("awk",      (color Red "\\0")    ),

                    ("Int",      (color Green "\\0")  ),
                    ("Integer",  (color Green "\\0")  ),
                    ("String",   (color Green "\\0")  ),
                    ("Char",     (color Green "\\0")  ),
                    ("Bool",     (color Green "\\0")  ),
                    ("Float",     (color Green "\\0")  ),
                    ("Double",     (color Green "\\0")  ),

                    ("void",     (color Blue "\\0")   ),
                    ("int",      (color Blue "\\0")   ),
                    ("string",   (color Blue "\\0")   ),
                    ("char",     (color Blue "\\0")  ),
                    ("bool",     (color Blue "\\0")  ),
                    ("Applicative",   (color Cyan "\\0")  ),
                    ("Functor",   (color Cyan "\\0")  ),
                    ("Monad",     (color Cyan "\\0")  ),
                    ("class",    (color Cyan "\\0")  ),
                    ("forall",    (color Green "\\0")  ),
                    ("m",        (color Yellow "\\0")  )
                   ]

        repWord = searchReplaceWord  -- word only
        repAny  = searchReplaceAnyTup  -- any String
        s00 = lines s
        s01 = map (\x -> repAny x ("\\[",      (color White "-LLLL-") )) s00 
        s02 = map (\x -> repAny x ("\\]",      (color White "-RRRR-") )) s01
        s03 = map (\x -> repAny x  ("->",       (color White "\\0") )) s02
        s04 = map (\x -> repAny x  ("=>",       (color Red "\\0")   )) s03

        s05x = map (\x -> repAny x  ("::",       (color Red " \\0 ") )) s04
        s05 = map (\x -> repAny x  ("\\.",       (color Cyan "\\0") )) s05x

        s050 = lines $ searchReplaceListWord repList (unlines s05)

        s14 = map (\x -> repAny x ("\\(",      (color Cyan "\\0")   )) s050 
        s15 = map (\x -> repAny x ("\\)",      (color Cyan "\\0")   )) s14
        s16 = map (\x -> repAny x ("<",        (color Yellow "\\0") )) s15
        s17 = map (\x -> repAny x (">",        (color Yellow "\\0") )) s16
        s18 = map (\x -> repAny x ("-LLLL-",   (color White "[")      )) s17
        s19 = map (\x -> repAny x ("-RRRR-",   (color White "]")      )) s18

type Repl a = InputT IO a

process :: String -> IO ()
process = putStrLn

repl :: Repl ()
repl = do
  minput <- getInputLine "|> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just input -> do 
       -- (liftIO $ process input) 
       liftIO $ writeFileListAppend "/tmp/x5.x" [input]
       liftIO  $ do 
                 if | hasPrefix ":ho" $ trim input  -> do
                         s2 <- run $ "ho " ++ (drop 3 $ input)
                         let cx = shellHighlight2 $ unlines s2
                         mapM_ putStrLn cx 
                    | otherwise -> do 
                         s1 <- run $ "gd.sh '"  ++ input ++ "'"
                         mapM_ putStrLn s1
                              
       repl

main :: IO ()
main = runInputT defaultSettings repl

















