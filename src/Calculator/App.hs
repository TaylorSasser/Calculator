{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Calculator.App (start) 
where 

import Calculator.Parser.Lex (Token(..), lexer)
import Calculator.Parser.Parse (Expr(..), parse)
import System.Exit (exitFailure)
import Data.Function ((&))
import Calculator.Solver.Solver (eval)
import Prelude hiding (lex, init)
import qualified Brick.Main as M
import Calculator.UserInterface.MainComponent (drawUi)
import Control.Monad (void)
import Brick
    ( attrMap,
      getVtyHandle,
      halt,
      showFirstCursor,
      on,
      AttrMap,
      App(..),
      EventM,
      BrickEvent(VtyEvent),
      zoom )
import Graphics.Vty (Key(..), setMode, Vty (outputIface))
import Control.Monad.Cont (liftIO)
import Graphics.Vty.Input (Event(..))
import Graphics.Vty.Attributes ( defAttr, black, white )
import Brick.Widgets.Edit (editAttr, editFocusedAttr)
import Graphics.Vty.Output.Interface (Mode(..))
import Lens.Micro ((^.))
import Lens.Micro.Platform (use, (.=))
import Data.Text (pack, unpack)
import Calculator.Types (Result(..), AppState, Name, form, getExpression, defaultAppState, history, defaultExpression, mkForm, input)
import Lens.Micro.Mtl ((%=))
import Brick.Forms (handleFormEvent)


data Error = Error String String

init :: Show e => String -> Either e a -> Either Error a
init n m = case m of
  Left e -> Left (Error n (show e))
  Right a -> Right a

printError :: Error -> IO ()
printError (Error n err) = putStrLn (n ++ " Error:" ++ err)

data Step a b = Step {
  name :: String,
  runStage :: a -> Either Error b
}

(>=>) :: Step a b -> Step b c -> Step a c
(>=>) (Step _ r1) (Step n2 r2) = Step n2 
  (\x -> do
    b <- r1 x
    r2 b
  )

result :: Show b => Step a b -> a -> IO ()
result (Step _ r) a = case r a of
  Left err -> do
    printError err
    exitFailure
  Right b -> do
    print (show b)
  

iota :: Show b => Step a b -> a -> String
iota (Step _ r) a = case r a of
  Right b -> do
    show b

makeStep :: Show e => String -> (a -> Either e b) -> Step a b
makeStep n r = Step n (init n . r)

lexStep :: Step String [Token]
lexStep = makeStep "Lexer" lexer

parseStep :: Step [Token] Expr
parseStep = makeStep "Parser" parse

solveStep :: Step Expr Rational
solveStep = makeStep "Solve" eval

calc :: String -> String
calc = lexStep >=> parseStep >=> solveStep & iota

theMap :: AttrMap
theMap = attrMap defAttr
  [ 
    (editAttr, white `on` black)
  , (editFocusedAttr, black `on` white)
  ]

tuiApp :: App (AppState e Name) e Name
tuiApp = App { appDraw         = drawUi
             , appChooseCursor = showFirstCursor
             , appStartEvent = do
                vty <- getVtyHandle
                liftIO $ setMode (outputIface vty) Mouse False
             , appHandleEvent  = appEvent
             , appAttrMap      = const theMap
             }

appEvent :: BrickEvent Name e -> EventM Name (AppState e Name) ()
appEvent ev =
  case ev of
    (VtyEvent (EvKey KEsc  [])) -> halt
    (VtyEvent (EvKey KEnter [])) -> do 
        fs <- use form
        let i = getExpression fs^.input
        if i == ""
          then return ()
        else do
            let res = calc (unpack i)
            do
              history %= (++[Result i (pack res)])
              form .= mkForm defaultExpression
              return ()
            return ()
    (VtyEvent _) -> zoom form $ handleFormEvent ev

start :: IO ()
start = do
  void $ M.defaultMain tuiApp defaultAppState