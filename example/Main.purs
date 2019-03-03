module Main where

import Prelude

import Data.Array ((..), foldl)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign.Object (Object, empty, insert, update, values)
import Freedom as Freedom
import Freedom.Markup as H
import Freedom.Portal (portal)
import Freedom.TransformF.Simple (VQueryF, transformF, reduce)
import Freedom.VNode (VNode)

type Item =
  { id :: String
  , opened :: Boolean
  }

type State = Object Item

type Html = VNode VQueryF State

main :: Effect Unit
main = Freedom.run
  { selector: "#app"
  , initialState
  , subscriptions: []
  , transformF
  , view
  }

-- State

initialState :: State
initialState = foldl genItem empty (0 .. 3)
  where
    genItem obj i =
      insert (show i) { id: show i, opened: false } obj

open :: String -> State -> State
open id = update (Just <<< _ { opened = true }) id

close :: String -> State -> State
close id = update (Just <<< _ { opened = false }) id

-- View

view :: State -> Html
view state =
  H.el $ H.div # H.kids
    [ H.el $ H.h1 # H.kids [ H.t "Portal Demo" ]
    , H.el $ H.ul # H.kids (item <$> values state)
    ]

item :: Item -> Html
item x =
  H.el $ H.li
    # H.css style
    # H.onClick openDialog
    # H.kids
      [ H.el $ H.span # H.kids
          [ H.t $ "Item " <> x.id ]
      , dialog x
      ]
  where
    openDialog = const $ reduce $ open x.id
    style =
      """
      .& { cursor: pointer; }
      .&:hover { opacity: 0.5; }
      """

dialog :: Item -> Html
dialog x =
  if not x.opened
    then H.el $ H.div
    else
      portal' $ H.el $ H.div
        # H.css overlayStyle
        # H.onClick closeDialog
        # H.kids
            [ H.el $ H.div # H.css boxStyle # H.kids
                [ H.t $ "Dialog: Item " <> x.id ]
            ]
  where
    portal' = portal { id: "dialog-portal", z: 0 }
    closeDialog = const $ reduce $ close x.id
    overlayStyle =
      """
      .& {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(0,0,0,.1);
        display: flex;
        justify-content: center;
        align-items: center;
        animation-fill-mode: both;
        animation: & 0.2s ease-in;
      }
      @keyframes & {
        from { opacity: 0 }
        to { opacity: 1 }
      }
      """
    boxStyle =
      """
      .& {
        width: 60%;
        height: 80%;
        background-color: white;
        border: 1px solid #DDD;
        border-radius: 8px;
        display: flex;
        justify-content: center;
        align-items: center;
        font-weight: bold;
        font-size: 24px;
      }
      """
