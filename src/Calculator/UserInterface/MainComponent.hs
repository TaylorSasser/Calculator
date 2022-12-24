{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Calculator.UserInterface.MainComponent(drawUi) where

import Brick.Widgets.Core ( vBox, txtWrap )
import Brick
    ( str,
      Widget,
      hLimitPercent,
      vLimitPercent,
      padTop,
      (<=>),
      Padding(Pad),
      (<+>),
      ViewportType(Vertical),
      viewport )
import Brick.Widgets.Border (border)
import Brick.Forms (Form, renderForm)
import Lens.Micro ((^.))
import Brick.Widgets.List (List)
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import Calculator.Types (AppState, Name (..), Expression, form, history)
import Data.Text (pack)


drawResult :: (Show a) => Bool -> a -> Widget Name
drawResult _ a =
  let selStr = txtWrap
    in str "" <+> selStr (pack (show a))


drawUI :: (Show a) => List Name a -> Widget Name
drawUI l = ui
    where
        box = hLimitPercent 100 $
              vLimitPercent 8 $
              L.renderList drawResult True l
        ui = viewport ResultsViewport Vertical $ vLimitPercent 80 $ vBox [box]

drawInputForm :: Form Expression e Name -> Widget Name
drawInputForm f = input
     where
        input = padTop (Pad 1) $ renderForm f


drawUi :: AppState e Name -> [Widget Name]
drawUi st = [
    border (
      drawUI (L.list None (Vec.fromList (st^.history)) 1)
    )
    <=>
    border (drawLayer st)
  ]

drawLayer :: AppState e Name -> Widget Name
drawLayer st = widget
  where widget = drawInputForm (st^.form)

