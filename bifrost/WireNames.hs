module WireNames where

-- This module has everything related to names of wires (and more, actually) for
-- the hardware generation stage.

import Data.Char (isAlphaNum, toUpper)
import Data.List (isInfixOf)

import Types
import PrettyPrint (joinWith)

----------------------------------------
-- Functions
----------------------------------------

-- Make a wire name from several pieces in a manner that is *injective*.
wireName :: [String] -> Wire
wireName = safeNameJoin

-- Make a general name from several pieces in a manner that is *injective*.
safeNameJoin :: [String] -> String
safeNameJoin = checkName . joinWith delim . map (escape . checkName)

-- Make sure all the characters in a name are alright.
checkName :: String -> String
checkName s =
  if all ok s
    then s
    else error $ "illegal name in hardware generation: " ++ show s
  where
    ok c = isAlphaNum c || c `elem` ['_','\'']

-- Delimeter for sections of wire names.
delim :: String
delim = "'"

-- Escape any occurrence of the delimiter by doubling it.
escape :: String -> String
escape s =
  if delim `isInfixOf` s
    then error "illegal name to escape: " ++ s
    else checkName s


----------------------------------------
-- Wire names for main module
----------------------------------------

rawClockWire :: Wire
rawClockWire = "clk"

rawResetWire :: Wire
rawResetWire = "reset"

sleepClockWire :: Wire
sleepClockWire = "eclk"

sleepResetWire :: Wire
sleepResetWire = "ereset"

powerReqWire :: Wire
powerReqWire = "pwrreq"

powerAckWire :: Wire
powerAckWire = "pwrack"

reqWire :: Wire
reqWire = "req"

ackWire :: Wire
ackWire = "ack"

inputWire :: Var -> Wire
inputWire arg =
  case arg of
    LocalVar _ name -> wireName ["i", name]
    _ -> error $
      "assumption violated: inputs expected to be local vars (of main)"

outputWire :: Var -> Wire
outputWire res =
  case res of
    LocalVar _ name -> wireName ["o", name]
    _ -> error $
      "assumption violated: outputs expected to be local vars (of main)"

actionReqWire :: ActionName -> Wire
actionReqWire act = wireName ["a", act, "req"]

actionAckWire :: ActionName -> Wire
actionAckWire act = wireName ["a", act, "ack"]

actionInputWire :: ActionName -> VarName -> Wire
actionInputWire act arg = wireName ["a", act, "i", arg]

actionOutputWire :: ActionName -> VarName -> Wire
actionOutputWire act res = wireName ["a", act, "o", res]

stateWire :: Wire
stateWire = "state"

runningWire :: Wire
runningWire = "running"

advanceWire :: Wire
advanceWire = "advance"

updateWire :: Wire
updateWire = "update"

holdWire :: Wire
holdWire = "hold"

-- for vanilla
startWire :: Wire
startWire = "start"

-- for chocolate
initialWire :: Wire
initialWire = "initial"

-- for chocolate
notInitialWire :: Wire
notInitialWire = wireName ["not", "initial"]

-- for chocolate
notDoneWire :: Wire
notDoneWire = wireName ["not", "done"]

-- for two-phase
wasReqWire :: Wire
wasReqWire = wireName ["was", "req"]

-- for two-phase
wasAckWire :: Wire
wasAckWire = wireName ["was", "ack"]

-- for pulse-echo
wasRunningWire :: Wire
wasRunningWire = wireName ["was", "running"]

varWire :: Var -> Wire
varWire v = wireName $
  case v of
    GlobalVar name -> ["g", name]
    LocalVar sub name -> ["l", sub, name]
    ReturnAddr sub -> ["ret", sub]
    MiscVar index -> ["misc", show index]

varTmpWire :: Var -> Wire
varTmpWire v = wireName $
  case v of
    GlobalVar name -> ["tmp","g", name]
    LocalVar sub name -> ["tmp","l", sub, name]
    ReturnAddr sub -> ["tmp","ret", sub]
    MiscVar index -> ["tmp","misc", show index]

sharedWire :: SharedRef -> Wire
sharedWire index = wireName ["s", show index]

actionWantWire :: ActionName -> Wire
actionWantWire act = wireName ["a", act, "want"]

actionRunWire :: ActionName -> Wire
actionRunWire act = wireName ["a", act, "run"]

actionDoneWire :: ActionName -> Wire
actionDoneWire act = wireName ["a", act, "done"]

actionPowerReqWire :: ActionName -> Wire
actionPowerReqWire act = wireName ["a", act, "pwrreq"]

actionPowerAckWire :: ActionName -> Wire
actionPowerAckWire act = wireName ["a", act, "pwrack"]

managerInputWire :: ActionName -> VarName -> Wire
managerInputWire act input = wireName ["a", act, "hi", input]

managerOutputWire :: ActionName -> VarName -> Wire
managerOutputWire act output = wireName ["a", act, "ho", output]


----------------------------------------
-- Names of wires within a manager module
----------------------------------------

managerInsideClockWire :: Wire
managerInsideClockWire = "clk"

managerInsideResetWire :: Wire
managerInsideResetWire = "reset"

managerInsideRunWire :: Wire
managerInsideRunWire = "run"

managerInsideDoneWire :: Wire
managerInsideDoneWire = "done"

managerInsideHoldWire :: Wire
managerInsideHoldWire = wireName ["hold"]

managerInsideManagedInputWire :: VarName -> Wire
managerInsideManagedInputWire input = wireName ["hi", input]

managerInsideManagedOutputWire :: VarName -> Wire
managerInsideManagedOutputWire output = wireName ["ho", output]

managerInsideReqWire :: Wire
managerInsideReqWire = "req"

managerInsideAckWire :: Wire
managerInsideAckWire = "ack"

managerInsideInputWire :: VarName -> Wire
managerInsideInputWire input = wireName ["i", input]

managerInsideOutputWire :: VarName -> Wire
managerInsideOutputWire output = wireName ["o", output]

managerInsideLatchWire :: Wire
managerInsideLatchWire = "latch"

managerInsideWasReqWire :: Wire
managerInsideWasReqWire = "was_req"

managerInsideWasAckWire :: Wire
managerInsideWasAckWire = "was_ack"

managerInsideStillDoneWire :: Wire
managerInsideStillDoneWire = "still_done"

managerInsideSentWire :: Wire
managerInsideSentWire = "sent"


----------------------------------------
-- State names
----------------------------------------

doneState :: EnumValue
doneState = "DONE"

idleState :: EnumValue
idleState = "IDLE"

biteState :: BiteLabel -> EnumValue
biteState (name, index) = safeNameJoin ["S", map toUpper name, show index]


----------------------------------------
-- Misc names (warning: fast and loose)
----------------------------------------

mainModuleName :: Environment -> ModuleName
mainModuleName = checkName . env_name

managerModuleName :: Environment -> ActionName -> CommProtocol -> ModuleName
managerModuleName env action protocol = checkName $
  mainModuleName env ++ "_" ++ action ++ "_" ++ protocol'
  where
    protocol' = case protocol of
      FourPhase -> "fourphase"
      TwoPhase -> "twophase"
      PulseEcho -> "pulseecho"
      Combinational i -> "combinational_" ++ show i

fsmStateTypeName :: Environment -> TypeName
fsmStateTypeName env = checkName $
  mainModuleName env ++ "_state"

-- Module to use for clock gating.
clockGateModuleName :: ModuleName
clockGateModuleName = "clockgate"
