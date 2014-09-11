{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards       #-}
-- |
-- Module : Network.Tracker.Request
-- Copyright : AUTHORS
-- License : BD3
-- Maintainer : Quentin Le Guennec <quentin.leguennec1@gmail.com>
-- Stability : experimental
-- Portability : portable
--
-- 
-- Handle tracker requests, from the client to the tracker
--
module Network.HsTorrent.Request
where

import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow (second)
import           Control.Lens.TH (makeLenses)
import           Control.Monad (liftM5)
import qualified Data.ByteString as B (ByteString, pack, cons, append, empty, init)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import           Data.Foldable (foldMap)
import           Data.Map as M (Map, fromList, foldl)
import           Data.Monoid ((<>))
import           Data.Typeable (Typeable)
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances

class Query e where
  -- | Map of key/values of the query string
  queries :: e -> M.Map B.ByteString B.ByteString

  -- | Produces a ByteString representing the query string
  query :: e -> B.ByteString
  query = B.init . M.foldl build B.empty . queries 
    where build k a = k <> "=" <> a <> "&"

-- | Values that can be inserted in a query string
class Value a where
  val :: a -> B.ByteString

-- | download status
data TEvent = Started | Stopped | Completed | Empty
  deriving (Show, Eq, Typeable, Generic)

-- | data type representing the tracker HTTP GET request
data TRequest = TRequest
  { _info_hash  :: B.ByteString -- ^ bencoded info key from the Metainfo file
  , _peer_id    :: B.ByteString -- ^ string client id
  , _port       :: Int -- ^ the port client is listening on
  , _uploaded   :: Int -- ^ number of bytes uploaded since the Started Event
  , _downloaded :: Int -- ^ number of bytes downloaded since the Started Event
  , _left       :: Int -- ^ number of bytes remaining for the client to complete download
  , _compact    :: Bool -- ^ does the client accept a compact response ?
  , _no_peer_id :: Bool -- ^ can the tracker ommit peer id field ?
  , _event      :: TEvent
  , _ip         :: Maybe B.ByteString
  , _numwant    :: Maybe Int
  , _key        :: Maybe Int
  , _trackerid  :: B.ByteString
  } deriving (Show, Eq, Typeable, Generic)

instance Value B.ByteString where val = id

instance Value Int where val = C.pack . show

instance Value Bool 
  where val True  = "1"
        val False = "0" 

instance (Value a) => Value (Maybe a)
  where val (Just a) = val a
        val Nothing  = B.empty

instance Value TEvent
  where val Started   = "started"
        val Stopped   = "stopped"
        val Completed = "completed"
        val Empty     = B.empty

instance Query TRequest where
  queries (TRequest {..}) = M.fromList
    [ ("info_hash"     ,  val _info_hash)
    , ("peer_id"       ,  val _peer_id)
    , ("port"          ,  val _port)
    , ("uploaded"      ,  val _uploaded)
    , ("downloaded"    ,  val _downloaded)
    , ("left"          ,  val _left)
    , ("compact"       ,  val _compact)
    , ("no_peer_id"    ,  val _no_peer_id)
    , ("event"         ,  val _event)
    , ("ip"            ,  val _ip)
    , ("numwant"       ,  val _numwant)
    , ("key"           ,  val _key)
    , ("trackerid"     ,  val _trackerid)
    ]

-- * Arbitrary instances

instance Arbitrary TEvent where
  arbitrary = elements [Started, Stopped, Completed, Empty]

instance Arbitrary TRequest where
  arbitrary = TRequest <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                       <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                       <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                       <*> arbitrary

arb = head <$> sample' (arbitrary :: Gen TRequest)

-- * Lenses
makeLenses ''TRequest
