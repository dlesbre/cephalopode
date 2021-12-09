module Preprocessor (loadFile) where

import Data.List
import Data.Char (isSpace)
import System.FilePath

loadFile :: FilePath -> IO String
loadFile file = do
  s <- readFile file
  lines' <- mapM (expandLine file) $ zip [1..] $ lines s
  return $ unlines lines'

expandLine :: FilePath -> (Int, String) -> IO String
expandLine file (num, line) =
  if "#" `isPrefixOf` line
    then doCmd $ splitWord $ tail line
    else return line
  where
    doCmd (cmd, rest) =
      case cmd of
        "include" ->
          case reads rest :: [(String, String)] of
            [(path, s)] -> do
              let relpath = takeDirectory file </> path
              let fullpath = if "/" `isPrefixOf` path then path else relpath
              foo <- loadFile fullpath
              return $ foo ++ s
            _ -> bad $ "bad invocation of #include (did you forget quotes around the file name, or forget the name entirely?)"
        _ -> bad $ "unintelligible preprocessor directive"
    splitWord s =
      case findIndex isSpace s of
        Just i -> splitAt i s
        Nothing -> (s, "")
    bad s = error $ file ++ " line " ++ show num ++ ": " ++ s ++ "\n  " ++ line
