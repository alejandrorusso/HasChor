{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module defines the interface to HasChor. The client of the library is
-- highly recommended to only use constructs exported by this module.
module Choreography (
  -- * Locations and Located Values
  LocTm,
  LocTy,
  type (@),
  At(..),
  toLocTm,

  -- * The Choreo monad
  Choreo,
  -- ** Choreo operations
  locally,
  (~>),
--  (~~>),
  cond,
--  cond',

  -- * Message transport backends
  -- ** The HTTP backend
  Host,
  Port,
  HttpConfig,
   mkHttpConfig,

  -- * Network stuff
  run,
  send,
  recv,
  broadcast,
  unwrap,
  wrap,
  mkEmpty,
  compileFor,

  -- * Running choreographies
--  runChoreo,
  runChoreography,
  genSpec -- NEW
  ) where

import Choreography.Location
import Choreography.Choreo
import Choreography.Network
import Choreography.Network.Http
import Choreography.Network.Local
import Control.Monad.IO.Class

import Choreography.Rulegen
import Data.Proxy

-- | Run a choreography with a message transport backend.
runChoreography :: (Backend config, MonadIO m) => config -> Choreo m a -> LocTm -> m a
runChoreography cfg choreo l = runNetwork cfg l choreo --(epp choreo l)
