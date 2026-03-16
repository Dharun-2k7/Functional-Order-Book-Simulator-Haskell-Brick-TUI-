module OrderBook.State
  ( AppState(..)
  , AppName
  , initialState
  , addTradeLog
  , maxTradeLogSize
  ) where

import OrderBook.Types

type AppName = ()

data AppState = AppState
  { appOrderBook  :: !OrderBook
  , appTradeLog   :: ![String]
  , appNextId     :: !OrderId
  } deriving (Show)

maxTradeLogSize :: Int
maxTradeLogSize = 20

initialState :: AppState
initialState = AppState
  { appOrderBook  = emptyOrderBook
  , appTradeLog   = ["Welcome to Haskell Order Book Simulator"
                    ,"Press B / S to add orders, M to match, Q to quit"]
  , appNextId     = 1
  }

addTradeLog :: String -> AppState -> AppState
addTradeLog msg st =
  st { appTradeLog = take maxTradeLogSize (msg : appTradeLog st) }
