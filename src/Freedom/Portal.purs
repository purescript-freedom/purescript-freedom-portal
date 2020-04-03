module Freedom.Portal
  ( PortalRoot
  , portal
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Freedom.Markup as H
import Freedom.UI (VNode, Operation)
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (createElement)
import Web.DOM.Element as E
import Web.DOM.Node (Node, appendChild, removeChild)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument, toParentNode)
import Web.HTML.HTMLElement as HE
import Web.HTML.Window (document)

-- | The type of portal root.
-- |
-- | - `id`: The id of portal root dom
-- | - `z`: The z-index of portal root dom
type PortalRoot =
  { id :: String
  , z :: Int
  }

-- | Render a `VNode` to the portal root.
-- |
-- | The portal root will be created automatically.
portal
  :: forall state
   . PortalRoot
  -> VNode state
  -> VNode state
portal portalRoot child =
  H.div
    # H.renderingManually
    # H.didCreate (const $ renderChildren portalRoot child)
    # H.didUpdate (const $ renderChildren portalRoot child)
    # H.didDelete (const $ deleteChildren portalRoot)

renderChildren
  :: forall state
   . PortalRoot
  -> VNode state
  -> Operation state
  -> Effect Unit
renderChildren portalRoot child { renderer } = do
  node <- getPortalRoot portalRoot
  renderer.renderChildren node [ child ]

deleteChildren
  :: forall state
   . PortalRoot
  -> Operation state
  -> Effect Unit
deleteChildren portalRoot { renderer } = do
  node <- getPortalRoot portalRoot
  renderer.renderChildren node []
  body' <- unsafePartial
    $ fromJust
    <$> (window >>= document >>= body)
    <#> HE.toNode
  void $ removeChild node body'

getPortalRoot :: PortalRoot -> Effect Node
getPortalRoot portalRoot = do
  maybeNode <- window
    >>= document
    <#> toParentNode
    >>= querySelector (QuerySelector $ "#" <> portalRoot.id)
    <#> map E.toNode
  case maybeNode of
    Just node -> pure node
    Nothing -> do
      el <- window
        >>= document
        <#> toDocument
        >>= createElement "div"
      E.setId portalRoot.id el
      E.setAttribute "style" (getPortalStyle portalRoot) el
      body' <- unsafePartial $ fromJust <$> (window >>= document >>= body) <#> HE.toNode
      appendChild (E.toNode el) body'

getPortalStyle :: PortalRoot -> String
getPortalStyle { z } = "position: absolute; z-index: " <> show z <> ";"
