module Component (component) where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
import Prim.Row (class Cons)

type StateRows =
  ( foo :: String
  , bar :: String
  , baz :: String
  )

type State = Record StateRows

data Action = UpdateState State

handleAction ∷ forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  UpdateState s -> H.put s

mkInput :: forall r l a. IsSymbol l => Cons l String r StateRows =>
  SProxy l -> State -> HH.HTML a Action
mkInput sym st =
  HH.input
  [ HP.type_ HP.InputNumber
  , HP.value $ Record.get sym st
  , HE.onValueChange \v -> Just $ UpdateState $ Record.set sym v st
  ]

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.div_ [ mkInput (SProxy::_"foo") state ]
    , HH.div_ [ mkInput (SProxy::_"bar") state ]
    , HH.div_ [ mkInput (SProxy::_"baz") state ]
    -- Print back to verify state
    , HH.div_ [ HH.text $ foldMap (\s -> s <> " ") [ state.foo, state.bar, state.baz ] ]
    ]

initialState :: forall i. i -> State
initialState _ =
  { foo: ""
  , bar: ""
  , baz: ""
  }

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }