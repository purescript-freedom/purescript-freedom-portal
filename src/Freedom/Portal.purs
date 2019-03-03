module Freedom.Portal
  ( PortalRoot
  , portal
  ) where

import Prelude

import Control.Monad.Free.Trans (FreeT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Class (liftEffect)
import Freedom.Markup as H
import Freedom.VNode (VNode, VRender, getPrevOriginChildren, getCurrentOriginChildren, renderChildren)
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (createElement)
import Web.DOM.Element as E
import Web.DOM.Node (Node, appendChild, removeChild)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode, toDocument, body)
import Web.HTML.HTMLElement as HE
import Web.HTML.Window (document)

type PortalRoot =
  { id :: String
  , z :: Int
  }

portal
  :: forall f state
   . Functor (f state)
  => PortalRoot
  -> VNode f state
  -> VNode f state
portal portalRoot child =
  H.op $ H.div
    # H.didCreate (const $ didCreatePortal portalRoot)
    # H.didUpdate (const $ didUpdatePortal portalRoot)
    # H.didDelete (didDeletePortal portalRoot)
    # H.kids [ child ]

didCreatePortal
  :: forall f state
   . Functor (f state)
  => PortalRoot
  -> FreeT (f state) (VRender f state) Unit
didCreatePortal portalRoot = do
  node <- liftEffect $ getPortalRoot portalRoot
  lift do
    currents <- getCurrentOriginChildren
    renderChildren node [] currents

didUpdatePortal
  :: forall f state
   . Functor (f state)
  => PortalRoot
  -> FreeT (f state) (VRender f state) Unit
didUpdatePortal portalRoot = do
  node <- liftEffect $ getPortalRoot portalRoot
  lift do
    prevs <- getPrevOriginChildren
    currents <- getCurrentOriginChildren
    renderChildren node prevs currents

didDeletePortal
  :: forall f state
   . Functor (f state)
  => PortalRoot
  -> FreeT (f state) (VRender f state) Unit
didDeletePortal portalRoot = do
  node <- liftEffect $ getPortalRoot portalRoot
  lift do
    prevs <- getPrevOriginChildren
    renderChildren node prevs []
  body' <- liftEffect
    $ unsafePartial
    $ fromJust
    <$> (window >>= document >>= body)
    <#> HE.toNode
  void $ liftEffect $ removeChild node body'

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
