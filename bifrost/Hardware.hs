module Hardware (hardware) where

{-

ACTION MANAGER MODULES

Note: the "release" wire has since been replaced by its complement "hold".

For each handshaked action we make a "manager module". The manager module takes
responsibility for the action's req, ack, arguments, and results signals, and
exposes a more abstract interface to our main module: run, done, release,
h-arguments, and h-results signals. The "run" signal tells the manager module
that we wish to run the action, the "done" signal indicates that the current run
of the action has completed, and the "release" signal tells the manager that as
of the next cycle we are done with the entire transaction (and a new one might
begin). The h-argument and h-result signals are for communicating the arguments
and results respectively with the manager, as opposed to directly with the
action (same contents, different timing).

Semantic information (ignoring the case of a reset signal):
- A transaction begins when the "run" signal is high, and on the previous cycle
  either: (a) the "run" signal was low, or (b) the "release" signal was high.
- A transaction corresponds to exactly one run of the associated action, using
  the arguments supplied via the h-argument signals.
- The "done" signal indicates that the relevant run of the action has completed,
  and that the h-result signals carry the action's results until the transaction
  is finished.
- A transaction finishes on the cycle where the "release" signal is high.
- The release signal may also be raised outside of a transaction, in which case
  it is ignored.

Precise timing information (ignoring the case of a reset signal):
- Once raised, the "run" signal must remain high through the cycle where the
  "release" signal is raised.
- If the "run" signal is high, the "done" signal will eventually become high
  (possibly immediately).
- Once high, the "done" signal will remain high through the cycle where the
  "release" signal is raised (possibly immediate).
- The "release" signal may only be high when the "done" signal is high, or when
  neither "run" nor "done" are high (n.b. outside of the transaction, no
  effect).
- When the "run" signal is high, the h-argument signals must be valid and
  constant through the cycle where the "release" signal is raised.
- When the "done" signal is high, the h-result signals are valid and
  remain constant through the cycle where the "release" signal is raised.

It is noted that "done" may depend without delay upon "run", and "release" may
depend without delay upon "done", allowing an entire transaction to take place
in a single cycle, and be followed by a new transaction on the next cycle.

-}

import Control.Monad
import Data.List (delete)
import Data.Maybe (catMaybes, isJust, fromJust)
import qualified Data.Map as M
import qualified Data.Set as S

import Types
import Expression
import ExpressionExtra
import Environment
import CombOpt
import Message
import KitchenSink
import PrettyPrint
import Compile
import WireNames

type HW = KitchenSink Message HWState

data HWState = HWState
  { hws_prog :: BitefulProgram
  , hws_env :: Environment
  , hws_flavor :: Flavor
  , hws_mods :: [HardwareModule]
  , hws_work :: HardwareModule  -- work in progress
  }

data Flavor
  = Vanilla        -- Design with IDLE state.
  | Chocolate      -- Design without IDLE state (faster but deeper logic).
  | Strawberry     -- Combinational design.
  deriving (Eq, Show)

-- The root of all evil.
hardware :: BitefulProgram -> Compile Hardware
hardware prog = mkCompile $ \env ->
  let initial = HWState prog env Vanilla [] (emptyModule "")
      (mname, messages, final) = runKitchenSink makeHardware initial
  in case (mname, filter isError messages) of
    (Just name, []) ->
      let hw = Hardware name (hws_mods final)
          env' = hws_env final
      in (messages, Just (env', hw))
    _ -> (messages, Nothing)

makeHardware :: HW ModuleName
makeHardware = do
  proto <- getProtocol
  let flavor = case proto_comm proto of
        Combinational _ -> Strawberry
        _               -> Chocolate  -- living in the fast lane
  modify $ \st -> st { hws_flavor = flavor }
  makeMainModule

-- Create the main module, and the manager modules in the process.
makeMainModule :: HW ModuleName
makeMainModule = do
  env <- getEnv
  flavor <- getFlavor
  newModule $ mainModuleName env
  makeClockResetPower
  case flavor of
    Strawberry -> makeStrawberry
    _ -> makeNonStrawberry
  -- yay
  moduleDone

makeClockResetPower :: HW ()
makeClockResetPower = do
  proto <- getProtocol
  flavor <- getFlavor
  let hasClock = proto_clock proto
  let hasReset = proto_reset proto
  let rawClock = bitwire rawClockWire
  let rawReset = Just $ bitwire rawResetWire
  when (not hasClock && flavor /= Strawberry) $ problem $
    show flavor ++ " flavor needs a clock of some sort"
  when (not hasReset && flavor /= Strawberry) $ problem $
    show flavor ++ " flavor needs a reset of some sort"
  when hasClock $ void $ do
    addWire InputWire rawClockWire bit
    addWire InternalWire sleepClockWire bit
  when hasReset $ void $ do
    addWire InputWire rawResetWire bit
    addWire InternalWire sleepResetWire bit
  -- since we're only clock-gating there's no need to do fancy reset stuff yet
  when hasReset $ void $ equateWire sleepResetWire $ bitwire rawResetWire
  -- do power protocol stuff
  case proto_power proto of
    AlwaysOn -> do
      when hasClock $ void $ equateWire sleepClockWire $ bitwire rawClockWire
    PowerShake -> void $ do
      when (not hasClock) $ problem $
        show (proto_power proto) ++ " power protocol needs a clock"
      addWire InputWire powerReqWire bit
      addWire OutputWire powerAckWire bit
      addFlop powerAckWire
        (bitwire powerReqWire) rawClock Nothing rawReset False
      -- equateWire resetWire $ mkOr
      --  (bitwire resetInWire)
      --  (mkNot $ bitwire powerAckWire)
      instantiate clockGateModuleName [rawClockWire, powerReqWire, sleepClockWire]
      promiseDefined sleepClockWire

makeStrawberry :: HW ()
makeStrawberry = do
  prog <- getProg
  env <- getEnv
  let bites = M.elems $ bitp_bites prog
  when (length bites /= 1) $ nonono $
    "program cannot be made into a combinational circuit " ++
    "as it has multiple bites"
  let [bite] = bites
  forM_ (bitp_inputs prog) $ \var -> do
    let (Just typ) = findVarType env var
    typ' <- wirifyType typ
    addWire InputWire (inputWire var) typ'
    addEquateWire InternalWire (varWire var) $ EWire (inputWire var) `ofType` typ'
  forM_ (bitp_outputs prog) $ \var -> do
    let (Just exp) = M.lookup var $ bite_updates bite
    varWireExp <- wirify exp
    addEquateWire OutputWire (outputWire var) varWireExp
  forM_ (M.toList $ bitp_shared prog) $
    \(ref, exp) -> do
      exp' <- wirify exp
      addEquateWire InternalWire (sharedWire ref) exp'

makeNonStrawberry :: HW ()
makeNonStrawberry = do
  prog <- getProg
  env <- getEnv
  -- make the handshake stuff and FSM
  makeControl
  -- create input var wires
  forM_ (bitp_inputs prog) $ \var -> do
    let (Just typ) = findVarType env var
    typ' <- wirifyType typ
    addWire InputWire (inputWire var) typ'
  -- create ouput var wires and connect them to corresponding variable wires
  forM_ (bitp_outputs prog) $ \var -> do
    let (Just typ) = findVarType env var
    varWireExp <- wirify $ EVar var `ofType` typ
    addEquateWire OutputWire (outputWire var) varWireExp
  -- create the wires, additional modules, and logic for all of the actions
  mapM_ makeAction $ env_actionOrder env
  -- create the wires, flops, and logic for the variables
  mapM_ makeVar $ M.keys $ env_vars env
  -- create and define the shared expression wires
  forM_ (M.toList $ bitp_shared prog) $
    \(ref, exp) -> do
      exp' <- wirify exp
      addEquateWire InternalWire (sharedWire ref) exp'

makeControl :: HW ()
makeControl = do
  flavor <- getFlavor
  proto <- getProtocol
  makeAdvance
  addEquateWire InternalWire holdWire $ mkNot $ bitwire advanceWire
  addWire InternalWire runningWire bit
  addWire InternalWire updateWire bit
  case flavor of
    Vanilla -> makeControlVanilla
    Chocolate -> makeControlChocolate

makeControlVanilla :: HW ()
makeControlVanilla = do
  equateWire updateWire $ mkOr (bitwire startWire) (bitwire advanceWire)
  addWire InternalWire startWire bit
  proto <- getProtocol
  case proto_comm proto of
    FourPhase -> makeControlVanillaFour
    TwoPhase -> makeControlVanillaTwo
    PulseEcho -> makeControlVanillaPulseEcho
    ValidReady -> nonono "Cannot create a module that speaks valid-ready"
  makeFSMVanilla

makeControlVanillaFour :: HW ()
makeControlVanillaFour = do
  idle <- isIdle
  done <- isDone
  addWire InputWire reqWire bit
  addWire OutputWire ackWire bit
  equateWire ackWire done
  equateWire startWire $ mkAnd idle $ bitwire reqWire
  equateWire runningWire $ optNot $ optOr bit [idle, done]
  return ()

makeControlVanillaTwo :: HW ()
makeControlVanillaTwo = do
  done <- isDone
  addWire InputWire reqWire bit
  addWire OutputWire ackWire bit
  addWire InternalWire wasReqWire bit
  addWire InternalWire wasAckWire bit
  addFlop wasReqWire (bitwire reqWire) sleepClock Nothing sleepReset False
  addFlop wasAckWire (bitwire ackWire) sleepClock Nothing sleepReset False
  equateWire ackWire $ bitwire wasAckWire `mkXor` done
  equateWire startWire $ bitwire reqWire `mkXor` bitwire wasReqWire
  equateWire runningWire $ bitwire reqWire `mkXor` bitwire ackWire
  return ()

makeControlVanillaPulseEcho :: HW ()
makeControlVanillaPulseEcho = do
  idle <- isIdle
  done <- isDone
  addWire InputWire reqWire bit
  addWire OutputWire ackWire bit
  equateWire ackWire $ done
  equateWire startWire $ bitwire reqWire
  equateWire runningWire $ optNot $ optOr bit [idle, done]
  return ()

makeControlChocolate :: HW ()
makeControlChocolate = do
  -- Common
  done <- isDone
  equateWire updateWire $ bitwire advanceWire
  addEquateWire InternalWire notDoneWire $ mkNot done
  addWire InternalWire notInitialWire bit
  let notInitial' =
        (bitwire notInitialWire `mkAnd` bitwire notDoneWire)  -- down after done
        `mkOr` bitwire updateWire                             -- up after update
  addFlop notInitialWire notInitial' sleepClock Nothing sleepReset False
  addEquateWire InternalWire initialWire $ mkNot $ bitwire notInitialWire
  -- Protocol-specific
  proto <- getProtocol
  case proto_comm proto of
    FourPhase -> makeControlChocolateFour
    TwoPhase -> makeControlChocolateTwo
    PulseEcho -> makeControlChocolatePulseEcho
    ValidReady -> nonono "Cannot create a module that speaks valid-ready"
  -- Finite state machine
  makeFSMChocolate

makeControlChocolateFour :: HW ()
makeControlChocolateFour = void $ do
  done <- isDone
  addWire InputWire reqWire bit
  addWire OutputWire ackWire bit
  equateWire ackWire done
  equateWire runningWire $ bitwire reqWire `mkAnd` bitwire notDoneWire

makeControlChocolateTwo :: HW ()
makeControlChocolateTwo = void $ do
  done <- isDone
  addWire InputWire reqWire bit
  addWire OutputWire ackWire bit
  addWire InternalWire wasReqWire bit
  addWire InternalWire wasAckWire bit
  addFlop wasReqWire (bitwire reqWire) sleepClock Nothing sleepReset False
  addFlop wasAckWire (bitwire ackWire) sleepClock Nothing sleepReset False
  equateWire ackWire $ bitwire wasAckWire `mkXor` done
  equateWire runningWire $ bitwire reqWire `mkXor` bitwire ackWire

makeControlChocolatePulseEcho :: HW ()
makeControlChocolatePulseEcho = void $ do
  done <- isDone
  addWire InputWire reqWire bit
  addWire OutputWire ackWire bit
  equateWire ackWire done
  addWire InternalWire wasRunningWire bit
  addFlop wasRunningWire (bitwire runningWire) sleepClock Nothing sleepReset False
  equateWire runningWire $
    (bitwire wasRunningWire `mkOr` bitwire reqWire)  -- up on req
    `mkAnd` (bitwire notDoneWire)                    -- down on done

-- Create the state wire and main FSM.
makeFSMVanilla :: HW ()
makeFSMVanilla = do
  prog <- getProg
  env <- getEnv
  proto <- getProtocol
  typ <- stateType
  let biteStates = map biteState $ M.keys $ bitp_bites prog
  let allStates = idleState:doneState:biteStates
  createEnumType (fsmStateTypeName env) allStates
  addWire InternalWire stateWire typ
  let idleNext = EEnum (biteState $ bitp_entry prog) `ofType` typ
  let doneNext = EEnum idleState `ofType` typ
  done <- isDone
  let enable = mkOr (bitwire updateWire) $
        case proto_comm proto of
          TwoPhase -> done
          FourPhase -> mkAnd done $ mkNot $ bitwire reqWire
          PulseEcho -> done
  next <- liftM simplify $ muxAcrossStatesVanilla typ idleNext doneNext $
    \(_, bite) -> wirify $ bite_next bite
  addFlop stateWire next sleepClock (Just enable) sleepReset False

-- Create the state wire and main FSM.
makeFSMChocolate :: HW ()
makeFSMChocolate = do
  prog <- getProg
  env <- getEnv
  proto <- getProtocol
  typ <- stateType
  let biteStates = map biteState $ M.keys $ bitp_bites prog
  let allStates = biteStates ++ [doneState]
  let entry = biteState $ bitp_entry prog
  let allStates' = entry : delete entry allStates  -- entry needs to be 0
  createEnumType (fsmStateTypeName env) allStates'
  addWire InternalWire stateWire typ
  done <- isDone
  let enable = mkOr (bitwire updateWire) $
        case proto_comm proto of
          TwoPhase -> done
          FourPhase -> mkAnd done $ mkNot $ bitwire reqWire
          PulseEcho -> done
  let entryExp = EEnum entry `ofType` typ
  next <- liftM simplify $ muxAcrossStatesChocolate typ entryExp $
    \(_, bite) -> wirify $ bite_next bite
  addFlop stateWire next sleepClock (Just enable) sleepReset False

-- Create the advance wire and its logic.
makeAdvance :: HW ()
makeAdvance = do
  env <- getEnv
  handled <- filterM needsManager $ M.keys $ env_actions env
  let readies = map
        (\action -> optImpl bit
          (bitwire $ actionWantWire action)
          (bitwire $ actionDoneWire action))
        handled
  let ready = optAnd bit readies
  let running = bitwire runningWire
  void $ addEquateWire InternalWire advanceWire $ optAnd bit [running, ready]

-- Create the circuitry for a variable and its updates.
makeVar :: Var -> HW ()
makeVar var = do
  flavor <- getFlavor
  case flavor of
    Vanilla -> makeVarVanilla var
    Chocolate -> makeVarChocolate var

makeVarVanilla :: Var -> HW ()
makeVarVanilla var = do
  prog <- getProg
  env <- getEnv
  styp <- stateType
  let (Just typ) = findVarType env var
  typ' <- wirifyType typ
  maybeInitialValue <- getInitialValue var
  let initialValue = maybe (dontCare typ') id maybeInitialValue
  keep <- wirify $ EVar var `ofType` typ
  next <- muxAcrossStatesVanilla typ' initialValue keep
    (\(_, bite) -> maybe
      (return keep)
      wirify $
      M.lookup var $ bite_updates bite)
  let next' = simplify $ bake $ simplify next
  let wire = varWire var
  addWire InternalWire wire typ'
  addFlop wire next' sleepClock (Just $ bitwire updateWire) Nothing False

-- Makes the wiring for a variable in the Chocolate architecture style. Unlike
-- with the Vanilla architecture we want to be able to begin immediately, which
-- means not spending a cycle initializing input variables. To accomplish this
-- a mux is added after each input variable's flop, allowing it to be bypassed
-- in favor of the raw input. Care is also taken to initialize the variable if
-- it is not set when leaving the entry biteful.
makeVarChocolate :: Var -> HW ()
makeVarChocolate var = do
  prog <- getProg
  env <- getEnv
  styp <- stateType
  let (Just typ) = findVarType env var
  typ' <- wirifyType typ

  -- Determine next value cases and enable disjunction, ignoring initialization.
  let biteNexts = catMaybes $ flip map (M.toList $ bitp_bites prog) $
        \(label, bite) -> do
          next <- M.lookup var $ bite_updates bite
          return (label, next)
  nexts <- forM biteNexts $ \(label, exp) -> do
    cond <- isInBiteful label
    exp' <- wirify exp
    return (cond, exp')
  enables <- forM biteNexts $ isInBiteful . fst

  -- Adjust nexts and enables to do initialization if needed.
  maybeInitialValue <- getInitialValue var
  let setByEntryBite = M.member var $ bite_updates $ fromJust $ M.lookup
        (bitp_entry prog) (bitp_bites prog)
  let needsInit = isJust maybeInitialValue && not setByEntryBite
  let nexts' = if needsInit
        then (bitwire initialWire, fromJust maybeInitialValue):nexts
        else nexts
  let enables' = if needsInit
        then (bitwire initialWire) : enables
        else enables

  -- Turn into single expressions.
  let next = simplify $ bake $ simplify $ mutexConds typ' nexts'
  let enable = simplify $ optOr bit enables' `mkAnd` bitwire updateWire

  -- Skip the enable signal if it's always 1. (TODO: if it's always 0?)
  let maybeEnable = if isOnes enable then Nothing else Just enable

  -- Build the circuit.
  let wire = varWire var
  addWire InternalWire wire typ'
  let needsBypass = isJust maybeInitialValue -- TODO: check flavor
  if needsBypass
    then do
      let tmpWire = varTmpWire var
      addWire InternalWire tmpWire typ'
      addFlop tmpWire next sleepClock maybeEnable Nothing False
      void $ equateWire wire $ mkIfThenElse
        (bitwire initialWire)
        (fromJust maybeInitialValue)
        (EWire tmpWire `ofType` typ')
    else do
      addFlop wire next sleepClock maybeEnable Nothing False

-- The initial (note: already wirified) value for a variable, if one exists.
getInitialValue :: Var -> HW (Maybe Exp)
getInitialValue var = do
  prog <- getProg
  env <- getEnv
  styp <- stateType
  let (Just typ) = findVarType env var
  typ' <- wirifyType typ
  return $ if var == bitp_ret prog
    then Just $ EEnum doneState `ofType` styp
    else if var `elem` bitp_inputs prog
      then Just $ EWire (inputWire var) `ofType` typ'
      else Nothing

-- Create the circuitry for an action: its implementation, its manager (if
-- needed), and its invocation.
makeAction :: ActionName -> HW ()
makeAction action = do
  prog <- getProg
  env <- getEnv
  let (Just info) = findActionInfo env action
  let (Just atyp) = findActionType env $ ai_type_name info
  let protocol = ai_protocol info
  let comm = proto_comm protocol
  let power = proto_power protocol
  let provider = ai_provider info
  let fp = at_io atyp
  let inputParams  = flattenPat =<< fp_arguments fp
  let outputParams = flattenPat $ fp_result fp
  -- possible wires we may create/use (note: types here not wirified)
  let req = actionReqWire action
  let ack = actionAckWire action
  let inputs   = map (\(inp, typ) -> (actionInputWire action inp, typ)) inputParams
  let outputs  = map (\(out, typ) -> (actionOutputWire action out, typ)) outputParams
  let hinputs  = map (\(inp, typ) -> (managerInputWire action inp, typ)) inputParams
  let houtputs = map (\(out, typ) -> (managerOutputWire action out, typ)) outputParams
  let want = actionWantWire action
  let done = actionDoneWire action
  let pwrReq = actionPowerReqWire action
  let pwrAck = actionPowerAckWire action
  let run = actionRunWire action
  -- think a bit
  let (toAction, fromAction) = case provider of
        External -> (OutputWire, InputWire)
        Instantiate _ -> (InternalWire, InternalWire)
  let clocks = if proto_clock protocol then [sleepClockWire] else []
  let resets = if proto_reset protocol then [sleepResetWire] else []
  let (powerReqs, powerAcks) = case power of
        AlwaysOn -> ([],[])
        PowerShake -> ([pwrReq],[pwrAck])
  let (shakeReqs, shakeAcks, maybeMakeManager) = case comm of
        TwoPhase -> ([req],[ack], Just makeTwoPhaseManager)
        FourPhase -> ([req],[ack], Just makeFourPhaseManager)
        PulseEcho -> ([req],[ack], Just makePulseEchoManager)
        ValidReady -> ([req],[ack], Just makeValidReadyManager)
        Combinational i -> ([],[], if power == AlwaysOn && i == 0
                             then Nothing
                             else Just $ makeCombinationalManager i)
  -- create the wires to and from the action implementation
  forM_ powerReqs $ \w -> addWire toAction w bit
  forM_ powerAcks $ \w -> addWire fromAction w bit
  forM_ shakeReqs $ \w -> addWire toAction w bit
  forM_ shakeAcks $ \w -> addWire fromAction w bit
  forM_ inputs    $ \(w,t) -> do
    t' <- wirifyType t
    addWire toAction w t'
  forM_ outputs   $ \(w,t) -> do
    t' <- wirifyType t
    addWire fromAction w t'
  -- create the action implementation if needed
  case provider of
    Instantiate implName -> do
      instantiate implName $
        clocks ++ resets ++
        powerReqs ++ powerAcks ++
        shakeReqs ++ shakeAcks ++
        map fst (inputs ++ outputs)
      mapM_ promiseDefined $ shakeAcks ++ powerAcks ++ map fst outputs
    _ -> return ()
  -- create power logic
  makeActionPower action power
  -- create the manager and associated wires
  case maybeMakeManager of
    Nothing -> return ()
    Just makeManager -> do
      addWire InternalWire want bit
      addWire InternalWire done bit
      forM_ hinputs  $ \(w,t) -> do
        t' <- wirifyType t
        addWire InternalWire w t'
      forM_ houtputs $ \(w,t) -> do
        t' <- wirifyType t
        addWire InternalWire w t'
      wantExp <- computeActionWant action
      void $ equateWire want wantExp
      addEquateWire InternalWire run $
        case power of
          AlwaysOn -> bitwire want
          PowerShake -> bitwire want `mkAnd` bitwire pwrAck
      managerName <- diversion $ makeManager action inputParams outputParams
      instantiate managerName $
        [sleepClockWire, sleepResetWire, run, done, holdWire] ++
        map fst (hinputs ++ houtputs) ++
        shakeReqs ++ shakeAcks ++ map fst (inputs ++ outputs)
      mapM_ promiseDefined $
        [done] ++ shakeReqs ++ (map fst $ inputs ++ houtputs)
  -- hook up the action's inputs
  forM_ inputParams $ \(name,_) -> do
    wire <- actionEffectiveInputWire action name
    exp <- computeActionInput action name
    equateWire wire exp

-- TODO: make smarter. Right now only powers on the action when we want it.
makeActionPower :: ActionName -> PowerProtocol -> HW ()
makeActionPower action power =
  case power of
    AlwaysOn -> return ()
    PowerShake ->
      let pwr = actionPowerReqWire action
      in void $ equateWire pwr (bitwire $ actionWantWire action)

-- Determine when an action should be run (its "want" signal, if it has a
-- manager).
computeActionWant :: ActionName -> HW Exp
computeActionWant action = do
  prog <- getProg
  flavor <- getFlavor
  disjuncts <- liftM catMaybes $ mapM doBiteful $ M.toList $ bitp_bites prog
  let wantVanilla = simplify $ optOr bit disjuncts
  let wantChocolate = mkAnd (bitwire runningWire) wantVanilla
  let want = case flavor of
        Vanilla -> wantVanilla
        Chocolate -> wantChocolate
  return want
  where
    doBiteful (label, bite) =
      case M.lookup action $ bite_actions bite of
        Nothing -> return Nothing
        Just tree -> do
          inBiteful <- isInBiteful label
          want <- treeWant tree
          return $ Just $ optAnd bit [inBiteful, want]
    treeWant tree =
      case tree of
        ADepends exp tree1 tree2 -> do
          want1 <- treeWant tree1
          want2 <- treeWant tree2
          exp' <- wirify exp
          return $ if want1 == want2
            then want1
            else (exp' `mkAnd` want1) `mkOr` ((mkNot exp') `mkAnd` want2)
        ACall _ -> return $ ones bit
        ADontCall -> return $ zero bit

-- Determine the value for a given action input wire.
computeActionInput :: ActionName -> VarName -> HW Exp
computeActionInput action inp = do
  prog <- getProg
  env <- getEnv
  let (Just info) = findActionInfo env action
  let (Just atyp) = findActionType env $ ai_type_name info
  let fp = at_io atyp
  let argPatterns = fp_arguments fp
  let argPattern = PTuple argPatterns  -- pretend there's one big argument tuple
  let [(path, typ)] = paramPaths inp argPattern  -- where our input var occurs
  typ' <- wirifyType typ
  let extract = flip (foldl $ flip mkProj) path
  muxed <- muxAcrossBites typ' (doBiteful typ' extract)
  return $ simplify $ bake muxed
  where
    -- Return a (wirified!) expression for the action input when in a particular
    -- bite.
    doBiteful typ' extract (label, bite) =
      case M.lookup action $ bite_actions bite of
        Nothing -> return $ dontCare typ'
        Just tree -> doTree typ' extract tree
    -- Given a call tree, compute an *un-wirified* expression for the action
    -- input. `extract` is used to pick out the input's value from the action's
    -- arguments, and `typ` is the type of the input.
    doTree typ' extract tree = do
      res <- case tree of
        ADepends exp tree1 tree2 -> do
          exp' <- wirify exp
          exp1 <- doTree typ' extract tree1
          exp2 <- doTree typ' extract tree2
          return $ optIfThenElse typ' (simplify exp') exp1 exp2
        ACall args -> do
          let arg = mkTuple args
          wirify $ simplify $ extract arg
        ADontCall -> return $ dontCare typ'
      return $ simplify res

makeCombinationalManager :: Integer -> ActionName -> [Param] -> [Param] -> HW ModuleName
makeCombinationalManager delay action inputParams outputParams = do
  when (delay /= 0) $ error "slow combinational actions not yet supported"
  env <- getEnv
  newModule $ managerModuleName env action (Combinational delay)
  -- create wires
  run <- addWire InputWire managerInsideRunWire bit
  done <- addWire OutputWire managerInsideDoneWire bit
  hold <- addWire InputWire managerInsideHoldWire bit
  minputs <- forM inputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire InputWire (managerInsideManagedInputWire name) typ'
  moutputs <- forM outputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire OutputWire (managerInsideManagedOutputWire name) typ'
  inputs <- forM inputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire OutputWire (managerInsideInputWire name) typ'
  outputs <- forM outputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire InputWire (managerInsideOutputWire name) typ'
  -- done = run
  equateWire done $ bitwire run
  -- pass inputs through
  forM_ inputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    let incoming = managerInsideManagedInputWire name
    let outgoing = managerInsideInputWire name
    equateWire outgoing $ EWire incoming `ofType` typ'
  -- pass outputs through
  forM_ outputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    let incoming = managerInsideOutputWire name
    let outgoing = managerInsideManagedOutputWire name
    equateWire outgoing $ EWire incoming `ofType` typ'
  moduleDone

{-
  Manager for two-phase actions. Circuit implements the following:
  req = was_req ^ (run & ~sent)
  done = run & ~(req ^ ack)
  latch = ack ^ was_ack
  was_req' = req
  was_ack' = ack
  sent' = (run | sent) & hold
-}
makeTwoPhaseManager :: ActionName -> [Param] -> [Param] -> HW ModuleName
makeTwoPhaseManager action inputParams outputParams = do
  prog <- getProg
  env <- getEnv
  newModule $ managerModuleName env action TwoPhase
  makeManagerGenericParts action inputParams outputParams
  -- find/create wires
  let run = managerInsideRunWire
  let done = managerInsideDoneWire
  let hold = managerInsideHoldWire
  let req = managerInsideReqWire
  let ack = managerInsideAckWire
  let latch = managerInsideLatchWire
  let clk = bitwire managerInsideClockWire
  let reset = Just $ bitwire managerInsideResetWire
  was_req <- addWire InternalWire managerInsideWasReqWire bit
  was_ack <- addWire InternalWire managerInsideWasAckWire bit
  sent <- addWire InternalWire managerInsideSentWire bit
  -- signals
  equateWire req $ bitwire was_req `mkXor` (bitwire run `mkAnd` (mkNot $ bitwire sent))
  equateWire done $ bitwire run `mkAnd` (mkNot $ bitwire req `mkXor` bitwire ack)
  equateWire latch $ bitwire ack `mkXor` bitwire was_ack
  -- flops
  addFlop was_req (bitwire req) clk Nothing reset False
  addFlop was_ack (bitwire ack) clk Nothing reset False
  let sent' = ((bitwire run `mkOr` bitwire sent) `mkAnd` bitwire hold)
  addFlop sent sent' clk Nothing reset False
  -- yay
  moduleDone

{-
  Manager for four-phase actions. Circuit implements the following:
  req = run & ~was_ack & (ack | ~done)
  done = still_done | latch
  latch = ack & ~was_ack
  was_ack' = ack
  still_done' = done & hold
-}
makeFourPhaseManager :: ActionName -> [Param] -> [Param] -> HW ModuleName
makeFourPhaseManager action inputParams outputParams = do
  prog <- getProg
  env <- getEnv
  newModule $ managerModuleName env action FourPhase
  makeManagerGenericParts action inputParams outputParams
  -- find/create wires
  let run = managerInsideRunWire
  let done = managerInsideDoneWire
  let hold = managerInsideHoldWire
  let req = managerInsideReqWire
  let ack = managerInsideAckWire
  let latch = managerInsideLatchWire
  let clk = bitwire managerInsideClockWire
  let reset = Just $ bitwire managerInsideResetWire
  was_ack <- addWire InternalWire managerInsideWasAckWire bit
  still_done <- addWire InternalWire managerInsideStillDoneWire bit
  -- signals
  equateWire req $
    (bitwire run `mkAnd` (mkNot $ bitwire was_ack)) `mkAnd`
    (bitwire ack `mkOr` (mkNot $ bitwire done))
  equateWire done $ bitwire still_done `mkOr` bitwire latch
  equateWire latch $ bitwire ack `mkAnd` (mkNot $ bitwire was_ack)
  -- flops
  addFlop was_ack (bitwire ack) clk Nothing reset False
  addFlop still_done (bitwire done `mkAnd` bitwire hold) clk Nothing reset False
  -- yay
  moduleDone

{-
  Manager for pulse-echo actions. Circuit implements the following:
  req = run & ~sent
  done = still_done | ack
  latch = ack
  sent' = (sent | req) & hold
  still_done' = done & hold
-}
makePulseEchoManager :: ActionName -> [Param] -> [Param] -> HW ModuleName
makePulseEchoManager action inputParams outputParams = do
  prog <- getProg
  env <- getEnv
  newModule $ managerModuleName env action PulseEcho
  makeManagerGenericParts action inputParams outputParams
  -- find/create wires
  let run = managerInsideRunWire
  let done = managerInsideDoneWire
  let hold = managerInsideHoldWire
  let req = managerInsideReqWire
  let ack = managerInsideAckWire
  let latch = managerInsideLatchWire
  let clk = bitwire managerInsideClockWire
  let reset = Just $ bitwire managerInsideResetWire
  sent <- addWire InternalWire managerInsideSentWire bit
  still_done <- addWire InternalWire managerInsideStillDoneWire bit
  -- signals
  equateWire req $ bitwire run `mkAnd` (mkNot $ bitwire sent)
  equateWire done $ bitwire still_done `mkOr` bitwire ack
  equateWire latch $ bitwire ack
  -- flops
  let sent' = (bitwire sent `mkOr` bitwire req) `mkAnd` bitwire hold
  addFlop sent sent' clk Nothing reset False
  let still_done' = bitwire done `mkAnd` bitwire hold
  addFlop still_done still_done' clk Nothing reset False
  -- yay
  moduleDone

{-
  Manager for valid-ready actions, where ack is the "valid" signal and req is
  the "ready" signal. Circuit implements the following:
  req = run & ~still_done
  latch = req & ack
  done = still_done | latch
  still_done' = done & hold
-}
makeValidReadyManager :: ActionName -> [Param] -> [Param] -> HW ModuleName
makeValidReadyManager action inputParams outputParams = do
  prog <- getProg
  env <- getEnv
  newModule $ managerModuleName env action PulseEcho
  makeManagerGenericParts action inputParams outputParams
  -- find/create wires
  let run = managerInsideRunWire
  let done = managerInsideDoneWire
  let hold = managerInsideHoldWire
  let req = managerInsideReqWire
  let ack = managerInsideAckWire
  let latch = managerInsideLatchWire
  let clk = bitwire managerInsideClockWire
  let reset = Just $ bitwire managerInsideResetWire
  still_done <- addWire InternalWire managerInsideStillDoneWire bit
  -- signals
  equateWire req $ bitwire run `mkAnd` (mkNot $ bitwire still_done)
  equateWire latch $ bitwire req `mkAnd` bitwire ack
  equateWire done $ bitwire still_done `mkOr` bitwire latch
  -- flops
  let still_done' = bitwire done `mkAnd` bitwire hold
  addFlop still_done still_done' clk Nothing reset False
  -- yay
  moduleDone
  
-- Create the signals that all of the action managers (other than the
-- combinational one) use, and set up the transparent flops used for grabbing
-- the outputs.
makeManagerGenericParts :: ActionName -> [Param] -> [Param] -> HW ()
makeManagerGenericParts action inputParams outputParams = do
  -- create wires
  clk <- addWire InputWire managerInsideClockWire bit
  reset <- addWire InputWire managerInsideResetWire bit
  run <- addWire InputWire managerInsideRunWire bit
  done <- addWire OutputWire managerInsideDoneWire bit
  hold <- addWire InputWire managerInsideHoldWire bit
  minputs <- forM inputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire InputWire (managerInsideManagedInputWire name) typ'
  moutputs <- forM outputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire OutputWire (managerInsideManagedOutputWire name) typ'
  req <- addWire OutputWire managerInsideReqWire bit
  ack <- addWire InputWire managerInsideAckWire bit
  inputs <- forM inputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire OutputWire (managerInsideInputWire name) typ'
  outputs <- forM outputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire InputWire (managerInsideOutputWire name) typ'
  latch <- addWire InternalWire managerInsideLatchWire bit
  -- pass inputs through
  forM_ inputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    let incoming = managerInsideManagedInputWire name
    let outgoing = managerInsideInputWire name
    equateWire outgoing $ EWire incoming `ofType` typ'
  -- latch outputs
  forM_ outputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    let incoming = managerInsideOutputWire name
    let outgoing = managerInsideManagedOutputWire name
    let enable = EWire latch `ofType` bit
    addFlop outgoing (EWire incoming `ofType` typ') (bitwire clk) (Just enable) Nothing True


----------------------------------------
-- Helpers
----------------------------------------

-- Note: assumes all of the involved expressions are already wirified.
muxAcrossStatesVanilla :: Type -> Exp -> Exp -> ((BiteLabel, Biteful) -> HW Exp) -> HW Exp
muxAcrossStatesVanilla typ ifIdle ifDone ifBiteful = do
  prog <- getProg
  let labelsAndBites = M.toList $ bitp_bites prog
  biteExps <- mapM ifBiteful labelsAndBites
  biteConds <- mapM (isInBiteful . fst) labelsAndBites
  idleCond <- isIdle
  doneCond <- isDone
  let pairs = (idleCond, ifIdle) : (doneCond, ifDone) : zip biteConds biteExps
  return $ mutexConds typ pairs

-- Note: assumes all of the involved expressions are already wirified.
muxAcrossStatesChocolate :: Type -> Exp -> ((BiteLabel, Biteful) -> HW Exp) -> HW Exp
muxAcrossStatesChocolate typ ifDone ifBiteful = do
  prog <- getProg
  let labelsAndBites = M.toList $ bitp_bites prog
  biteExps <- mapM ifBiteful labelsAndBites
  biteConds <- mapM (isInBiteful . fst) labelsAndBites
  doneCond <- isDone
  let pairs = (doneCond, ifDone) : zip biteConds biteExps
  return $ mutexConds typ pairs

-- Note: assumes all of the involved expressions are already wirified.
muxAcrossBites :: Type -> ((BiteLabel, Biteful) -> HW Exp) -> HW Exp
muxAcrossBites typ ifBiteful = do
  prog <- getProg
  let labelsAndBites = M.toList $ bitp_bites prog
  biteExps <- mapM ifBiteful labelsAndBites
  biteConds <- mapM (isInBiteful . fst) labelsAndBites
  let pairs = zip biteConds biteExps
  return $ mutexConds typ pairs

-- Turn variables, bitelabels, shared expression references, and action results
-- into their hardware-equivalent wires.
wirify :: Exp -> HW Exp
wirify exp = do
  inner' <- case getInner exp of
    EVar v -> do
      return $ EWire $ varWire v
    ETypeAnn t exp1 -> do
      t' <- wirifyType t
      exp1' <- wirify exp1
      return $ ETypeAnn t' exp1'
    EBiteLabel label -> do
      let stname = biteState label
      return $ EEnum stname
    EShared ref -> do
      return $ EWire $ sharedWire ref
    EActionResult action res -> do
      wire <- actionEffectiveOutputWire action res
      return $ EWire wire
    _ -> do
      tmp <- untoChildren wirify exp
      return $ getInner tmp
  typ' <- wirifyType $ typeOf exp
  return $ inner' `ofType` typ'

-- Change the type Address to the actual address type we're using.
wirifyType :: Type -> HW Type
wirifyType typ = do
  styp <- stateType
  return $ foo styp typ
  where
    foo styp t =
      case t of
        Address -> styp
        _ -> uponTypeChildren (foo styp) t

isDone :: HW Exp
isDone = isInState (doneState)

isIdle :: HW Exp
isIdle = isInState (idleState)

isInBiteful :: BiteLabel -> HW Exp
isInBiteful = isInState . biteState

isInState :: EnumValue -> HW Exp
isInState st = do
  t <- stateType
  return $
    EOpApp Eq [EWire stateWire `ofType` t, EEnum st `ofType` t] `ofType` bit

-- Get the argument wire we should be putting arguments on. If the action has a
-- manager then we use the one to the manager, otherwise we use the raw one.
actionEffectiveInputWire :: ActionName -> VarName -> HW Wire
actionEffectiveInputWire action arg = do
  nh <- needsManager action
  return $ if nh
    then managerInputWire action arg
    else actionInputWire action arg

-- Get the result wire we should be taking results from. If the action has a
-- manager then we use the one from the manager, otherwise we use the raw one.
actionEffectiveOutputWire :: ActionName -> VarName -> HW Wire
actionEffectiveOutputWire action res = do
  nh <- needsManager action
  return $ if nh
    then managerOutputWire action res
    else actionOutputWire action res

-- Does an action need a manager?
needsManager :: ActionName -> HW Bool
needsManager action = do
  proto <- actionProtocol action
  let comm = proto_comm proto
  let power = proto_power proto
  return $ case (comm, power) of
    (Combinational 0, AlwaysOn) -> False
    _ -> True

-- Used by most flops in the main module.
sleepClock :: Exp
sleepClock = bitwire sleepClockWire

-- Used by most flops in the main module.
sleepReset :: Maybe Exp
sleepReset = Just $ bitwire sleepResetWire


----------------------------------------
-- Lookups of program stuff
----------------------------------------

actionInfo :: ActionName -> HW ActionInfo
actionInfo action = do
  env <- getEnv
  let (Just info) = M.lookup action $ env_actions env
  return info

actionProvider :: ActionName -> HW ActionProvider
actionProvider = liftM ai_provider . actionInfo

actionProtocol :: ActionName -> HW Protocol
actionProtocol = liftM ai_protocol . actionInfo

-- Type of states for the main module's fsm.
stateType :: HW Type
stateType = do
  env <- getEnv
  return $ NamedType $ fsmStateTypeName env


----------------------------------------
-- Monad mutation misery & co
----------------------------------------

-- Clear the module under construction and begin a fresh one.
newModule :: ModuleName -> HW ()
newModule name = putModule $ emptyModule name

-- The current module under construction is done; add it to the modules list.
moduleDone :: HW ModuleName
moduleDone = do
  st <- get
  let mod = hws_work st
  let undef = mod_dbg_undefined mod
  when (not $ S.null undef) $ problem $
    "wires were left without well-defined signals: " ++
    (joinWith ", " $ map show $ S.toList undef)
  let name = mod_name mod
  when (name `elem` (map mod_name $ hws_mods st)) $ problem $
    "multiple modules created with same name: " ++ show name
  put $ st { hws_mods = (hws_mods st) ++ [mod] }
  return $ name

-- Set aside the module we're currently working on, do something else (like
-- building a totally different module), and then restore the original module
-- so that we can continue working on it. The result of the "something else"
-- task (for example, a module name) is returned.
diversion :: HW a -> HW a
diversion task = do
  work <- getModule
  res <- task
  putModule work
  return res

getFlavor :: HW Flavor
getFlavor = liftM hws_flavor get

getProtocol :: HW Protocol
getProtocol = liftM env_protocol getEnv


-- Get the module we're building at the moment.
getModule :: HW HardwareModule
getModule = liftM hws_work get

-- Set the module we're building at the moment.
putModule :: HardwareModule -> HW ()
putModule mod = modify $ \st -> st { hws_work = mod }

-- Modify the module we're building at the moment.
modifyModule :: (HardwareModule -> HardwareModule) -> HW ()
modifyModule f = modify $ \st -> st { hws_work = f (hws_work st) }

getEnv :: HW Environment
getEnv = liftM hws_env get

putEnv :: Environment -> HW ()
putEnv env = modify $ \st -> st { hws_env = env }

getProg :: HW BitefulProgram
getProg = liftM hws_prog get

-- Add a wire to the module we're building at the moment.
addWire :: WireRole -> Wire -> Type -> HW Wire
addWire role wire typ = do
  debug $ "adding wire: " ++ wire
  mod <- getModule
  checkTypeWirified typ
  when (M.member wire $ mod_wires mod) $ problem $
    "wire " ++ show wire ++ " created/added multiple times"
  putModule $
    let wires = mod_wires mod
        wires' = M.insert wire (WireInfo role typ) wires
        io = mod_ordered_io mod
        int = mod_ordered_internal mod
        undef = mod_dbg_undefined mod
        io' = if role == InternalWire then io else io ++ [wire]
        int' = if role == InternalWire then int ++ [wire] else int
        undef' = if role == InputWire then undef else S.insert wire undef
        mod' = mod
          { mod_wires = wires'
          , mod_ordered_io = io'
          , mod_ordered_internal = int'
          , mod_dbg_undefined = undef'
          }
    in mod'
  return wire

-- Equate a wire to an expression. Don't forget to wirify the expression first.
equateWire :: Wire -> Exp -> HW Wire
equateWire wire exp = do
  debug $ "equating wire: " ++ wire
  checkAwaitingDef wire
  checkWirified exp
  mod <- getModule
  when (M.notMember wire $ mod_wires mod) $ problem $
    "attempted to define signal of nonexistent wire " ++ show wire
  let (Just info) = M.lookup wire $ mod_wires mod
  when (wi_role info == InputWire) $ problem $
    "attempted to define signal of input wire " ++ show wire
  let wireType = wi_type info
  let expType = typeOf exp
  when (wireType /= expType) $ problem $
    "type mismatch between wire " ++ show wire ++ " of type " ++
    show wireType ++ " and expression " ++ show exp ++ " of type " ++
    show expType
  let equations' = M.insert wire exp $ mod_equations mod
  let mod' = mod { mod_equations = equations' }
  putModule mod'
  promiseDefined wire
  return wire

addEquateWire :: WireRole -> Wire -> Exp -> HW Wire
addEquateWire role wire exp = do
  addWire role wire (typeOf exp)
  equateWire wire exp

addFlop :: Wire -> Exp -> Exp -> Maybe Exp -> Maybe Exp -> Bool -> HW ()
addFlop wire exp clk enable reset transparent = do
  debug $ "adding flop with output wire: " ++ wire
  checkAwaitingDef wire
  checkWirified exp
  let flop = Floppish wire exp clk enable reset transparent
  mod <- getModule
  when (not $ wire `elem` mod_dbg_undefined mod) $ problem $
    "attmpted to connect flop to nonexistent or already well-defined wire " ++
    show wire
  let (Just info) = M.lookup wire $ mod_wires mod
  let wtyp = wi_type info
  let etyp = typeOf exp
  when (wtyp /= etyp) $ problem $
    "flip-flop output and input have different types " ++
    show wire ++ " of type " ++ show wtyp ++ ", " ++
    show exp ++ " of type " ++ show etyp
  let mod' = mod { mod_flops = (mod_flops mod) ++ [flop] }
  putModule mod'
  promiseDefined wire

{-
addFSM :: FSMName -> FSM -> HW ()
addFSM name fsm = do
  checkAwaitingDef $ fsm_state fsm
  mod <- getModule
  when (M.member name $ mod_fsms mod) $ problem $
    "attmpted to redefine fsm " ++ show name
  putModule $ mod { mod_fsms = M.insert name fsm $ mod_fsms mod }
  promiseDefined $ fsm_state fsm
-}

-- Instantiate some module (by name) within the current one.
instantiate :: ModuleName -> [Wire] -> HW ()
instantiate name wires = do
  debug $ "Instantiating module: " ++ name
  modifyModule $ \mod ->
    let inst = ModuleInstantiation name wires
    in mod { mod_others = (mod_others mod) ++ [inst] }

-- Create a new enum type. The given type name is also used for the compiled FL.
-- The first value will correspond to 0 (useful for flops with reset).
createEnumType :: TypeName -> [EnumValue] -> HW Type
createEnumType name values = do
  env <- getEnv
  when (M.member name $ env_types env) $ problem $
    "redefinition of type: " ++ show name
  let types' = M.insert name (Just name) $ env_types env
  let enums' = M.insert name values $ env_enums env
  let env' = env { env_types = types', env_enums = enums' }
  putEnv env'
  return $ NamedType name

-- Make sure that a wire does not yet have a well-defined signal (anti-bug
-- measure). Note: will also produce an error if the wire doesn't yet exist.
checkAwaitingDef :: Wire -> HW ()
checkAwaitingDef wire = do
  mod <- getModule
  when (not $ wire `elem` mod_dbg_undefined mod) $ problem $
    "wire not in set awaiting definition (either it doesn't yet exist, or " ++
    "has already been defined): " ++ show wire

-- Indicate a wire has received a well-defined signal (anti-bug measure, should
-- only be called by functions that actually define the signal of the wire, i.e.
-- equateWire, addFlop, addFSM, and also when ).
promiseDefined :: Wire -> HW ()
promiseDefined wire = modifyModule $ \mod ->
  mod { mod_dbg_undefined = S.delete wire $ mod_dbg_undefined mod }

-- Cause an error if an expression isn't wirified.
checkWirified :: Exp -> HW ()
checkWirified exp =
  if isWirified exp
    then return ()
    else problem $ "expression supposed to be wirified but isn't: " ++ show exp

-- Cause an error if a type isn't wirified.
checkTypeWirified :: Type -> HW ()
checkTypeWirified typ =
  if isTypeWirified typ
    then return ()
    else problem $ "type supposed to be wirified but isn't: " ++ show typ


-- Indicate that something went very wrong (indicating a bug in the compiler).
problem :: String -> HW a
problem msg = logMessagesAndFail [Error $ "internal error: " ++ msg]

-- Indicate that something went very wrong but it's not our fault.
nonono :: String -> HW a
nonono msg = logMessagesAndFail [Error $ "internal error: " ++ msg]

debug :: String -> HW ()
debug msg = return () -- logMessages [Warning $ "debug: " ++ msg]

----------------------------------------
-- Non-monadic helpers
----------------------------------------

isWirified :: Exp -> Bool
isWirified exp =
  case getInner exp of
    EInt _ -> True
    EBlob _ -> True
    EIfThenElse _ _ _ -> recurse
    EOpApp _ _ -> recurse
    EApp _ _ -> recurse
    EField _ _ -> recurse
    EFieldUpdate _ _ _ -> recurse
    ETuple _ -> recurse
    EProj _ _ -> recurse
    ETypeAnn _ _ -> recurse
    EWire _ -> True
    EEnum _ -> True
    EDontCare -> True
    ESignExtend _ -> recurse
    _ -> False
  where
    recurse = all isWirified $ children exp

isTypeWirified :: Type -> Bool
isTypeWirified typ =
  case typ of
    Address -> False
    _ -> all isTypeWirified $ typeChildren typ

-- An empty module, except for the name.
emptyModule :: ModuleName -> HardwareModule
emptyModule name = HardwareModule name M.empty [] [] M.empty [] {- M.empty -} [] S.empty

-- Save some typing.
bitwire :: Wire -> Exp
bitwire w = EWire w `ofType` bit

