module Component (component) where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { foo :: String
  , bar :: String
  , baz :: String
  }

data Action = UpdateState (State -> State)

handleAction ∷ forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  UpdateState updateFunc ->
    H.modify_ updateFunc

mkInput :: forall a. String -> (String -> State -> State) -> HH.HTML a Action
mkInput value inlineUpdate =
  HH.input
  [ HP.type_ HP.InputNumber
  , HP.value value
  , HE.onValueChange \v -> Just $ UpdateState $ inlineUpdate v
  ]

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.div_ [ mkInput state.foo ( \v s -> s{ foo = v } ) ]
    , HH.div_ [ mkInput state.bar ( \v s -> s{ bar = v } ) ]
    , HH.div_ [ mkInput state.baz ( \v s -> s{ baz = v } ) ]
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