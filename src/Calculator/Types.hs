{-# LANGUAGE InstanceSigs #-}
module Calculator.Types (
    Name(..),
    SolveType(..),
    Expression(..),
    Result(..), AppState(..),
    input,
    solveMode,
    expr,
    solution,
    history,
    form,
    defaultExpression,
    getExpression,
    defaultAppState,
    mkForm
) where

import GHC.Generics (Generic)
import Data.Text as T ( Text, unpack, )
import Brick.Forms ( Form, newForm, (@@=), editTextField, formState )
import Lens.Micro.TH ( makeLenses )
import Brick (padBottom)
import Brick.Types ()
import Brick.AttrMap ()
import Brick.Util ()
import Brick.Widgets.Core ( (<+>), fill, hLimit, str, vLimit, Padding(Pad) )
import Lens.Micro ((^.))

data Name = ExprField | EvaluateField | SimplifyField | FactorField | ResultsViewport | InputViewport | None deriving (Show, Eq, Generic, Ord)
data SolveType = SolveType | Simplify | Evaluate | Factor  deriving (Show, Eq, Generic, Ord)

data Expression = Expression {
    _input :: Text,
    _solveMode :: SolveType
} deriving (Show, Generic, Eq)

data Result = Result {
    _expr :: Text,
    _solution :: Text
}

instance Show Result where
  show :: Result -> String
  show r = unpack (_expr r) ++ "=" ++ unpack (_solution r)

data AppState e n = AppState {
    _history :: [Result],
    _form :: Form Expression e n
}


makeLenses ''Expression
makeLenses ''Result
makeLenses ''AppState

mkForm :: Expression -> Form Expression e Name
mkForm =
    let label s w = padBottom (Pad 1) $ vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [
        label "Expression" @@= editTextField input ExprField (Just 1)
    ]

defaultExpression :: Expression
defaultExpression = Expression {
    _input = "",
    _solveMode = Evaluate
}

getExpression :: Form Expression e Name -> Expression
getExpression f = Expression {
  _input = formState f^.input,
  _solveMode = formState f^.solveMode
}

defaultAppState :: AppState e Name
defaultAppState = AppState {
    _history = [],
    _form = mkForm defaultExpression
}



