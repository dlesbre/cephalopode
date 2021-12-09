import System.Environment (getArgs)
import System.Exit (die)
import Data.List (isSuffixOf, replicate)
import Control.Monad (forM, forM_, when)

import Types
import Environment
import Message
import Compile

-- BNFC
import qualified Lang.AbsLang as Lang
import qualified Lang.ParLang as ParLang
import qualified Lang.ErrM as ErrLang

-- Stages
import Preprocessor
import ReadAST
import RefCheck
import Typecheck
import TypecheckStructuredProgram
import ControlFlowCheck
import Decomplexify
import Blockify
import Chomp
import Liveness
import Unshare
import Hardware
import FLOut

main :: IO ()
main = do
  args <- getArgs
  statuses <- forM args $ \fname -> do
    if source_extension `isSuffixOf` fname
      then processFile fname (fname ++ ".fl")
      else die $ "To avoid unhappy accidents, only files with extension " ++ source_extension ++ " are allowed."
  let count = length statuses
  let sucs = length $ filter id statuses
  let remark = if sucs == count then "Yay!" else "ALERT! Only"
  putStrLn ""
  putStrLn $ replicate 80 '-'
  putStrLn $ remark ++ " " ++ show sucs ++ "/" ++ show count ++
    " program(s) compiled successfully."

source_extension :: String
source_extension = ".prog"

printIntermediate :: Bool
printIntermediate = True

processFile :: String -> String -> IO Bool
processFile fileIn fileOut = do
  s <- loadFile fileIn
  putStrLn "\n-------- original source code (with #includes expanded) --------\n"
  putStrLn s
  putStrLn "\n"
  case ParLang.pProgram (ParLang.myLexer s) of
    ErrLang.Ok lprog -> do
      let pr = printIntermediate
      let no = False
      (mres, msgs) <-
        begin lprog emptyEnvironment $
        stage pr "readAST" readAST $
        stage no "refcheck" refCheck $
        stage no "typecheck" typecheckStructuredProgram $
        stage no "controlflowcheck" controlFlowCheck $
        stage pr "decomplexify" decomplexify $
        stage pr "blockify" blockify $
        stage pr "chomp" chomp $
        stage pr "liveness" liveness $
        stage pr "unshare" unshare $
        stage no "hardware" hardware $
        stage no "fl" genFL $
        done
      putStrLn $ replicate 60 '-'
      putStrLn $ "Compilation: " ++ maybe "failed" (\_ -> "succeeded!") mres
      putStrLn "Message log:"
      mapM_ print msgs
      putStrLn ""
      case mres of
        Nothing -> return False
        Just s -> do
          writeFile fileOut s
          putStrLn $ "Compiled program written to " ++ fileOut
          putStrLn ":)"
          return True
    ErrLang.Bad err -> error $ "Parse error: " ++ err

type Stage r a = a -> Environment -> IO (Maybe r, [Message])

begin :: a -> Environment -> (Stage r a) -> IO (Maybe r, [Message])
begin prog env stg = stg prog env

stage :: Show b => Bool -> String -> (a -> Compile b) -> Stage r b -> Stage r a
stage printProg name c next = \prog env -> do
  let (msgs, res) = runCompile (c prog) env
  case res of
    Just (env', prog') -> do
      printStatus "succeeded"
      when printProg $ do
        putStrLn ""
        print prog'
        putStrLn ""
      printMessages msgs
      (res', msgs') <- next prog' env'
      return (res', msgs ++ msgs')
    Nothing -> do
      printStatus "failed"
      printMessages msgs
      return (Nothing, msgs)
  where
    printStatus s = putStrLn $
      "-------- Compilation stage " ++ show name ++ " " ++ s ++ "! --------"
    printMessages msgs =
      if not $ null msgs
        then do
          putStrLn $ "\nMessages:"
          mapM_ print msgs
          putStrLn ""
        else return ()

done :: Stage r r
done = \res _ -> return $ (Just res, [])
