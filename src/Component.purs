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

data InputSource
  = Foo
  | Bar
  | Baz

data Action = UpdateInput InputSource String

handleAction ∷ forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  UpdateInput Foo v ->
    H.modify_ \s -> s { foo = v }
  UpdateInput Bar v ->
    H.modify_ \s -> s { bar = v }
  UpdateInput Baz v ->
    H.modify_ \s -> s { baz = v }

mkInput :: forall a. String -> InputSource -> HH.HTML a Action
mkInput value source =
  HH.input
  [ HP.type_ HP.InputNumber
  , HP.value value
  , HE.onValueChange \v -> Just $ UpdateInput source v
  ]

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.div_ [ mkInput state.foo Foo ]
    , HH.div_ [ mkInput state.bar Bar ]
    , HH.div_ [ mkInput state.baz Baz ]
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