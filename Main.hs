{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Brick
import qualified Brick.Main as M
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP

import           System.Random (randomRIO)
import           Numeric (showFFloat)

import OrderBook
import OrderBook.UI

app :: M.App AppState e AppName
app = M.App
  { M.appDraw         = drawUI
  , M.appChooseCursor = M.neverShowCursor
  , M.appHandleEvent  = handleEvent
  , M.appStartEvent   = return ()
  , M.appAttrMap      = const theMap
  }

handleEvent :: BrickEvent AppName e -> EventM AppName AppState ()
handleEvent (VtyEvent (V.EvKey key mods)) = handleKey key mods
handleEvent _ = return ()

handleKey :: V.Key -> [V.Modifier] -> EventM AppName AppState ()
handleKey (V.KChar 'q') _ = M.halt
handleKey (V.KChar 'Q') _ = M.halt

handleKey (V.KChar 'b') _ = do
  st <- get
  order <- liftIO (randomOrder Buy (appNextId st))
  let book'  = insertOrder order (appOrderBook st)
      msg    = "Buy  #" ++ show (appNextId st)
             ++ " added  @ " ++ fmtP (orderPrice order)
             ++ " x " ++ show (orderQty order)
  put $ addTradeLog msg $ st
    { appOrderBook = book'
    , appNextId    = appNextId st + 1
    }

handleKey (V.KChar 'B') _ = handleKey (V.KChar 'b') []

handleKey (V.KChar 's') _ = do
  st <- get
  order <- liftIO (randomOrder Sell (appNextId st))
  let book'  = insertOrder order (appOrderBook st)
      msg    = "Sell #" ++ show (appNextId st)
             ++ " added  @ " ++ fmtP (orderPrice order)
             ++ " x " ++ show (orderQty order)
  put $ addTradeLog msg $ st
    { appOrderBook = book'
    , appNextId    = appNextId st + 1
    }

handleKey (V.KChar 'S') _ = handleKey (V.KChar 's') []

handleKey (V.KChar 'm') _ = do
  st <- get
  let (trades, book') = matchOrders (appOrderBook st)
      tradeMessages   = map formatTrade trades
      st'             = foldr addTradeLog st { appOrderBook = book' } tradeMessages
      noMatchMsg      = if null trades
                          then addTradeLog "No matching orders (spread too wide)" st { appOrderBook = book' }
                          else st'
  put noMatchMsg

handleKey (V.KChar 'M') _ = handleKey (V.KChar 'm') []

handleKey (V.KChar 'c') _ = do
  st <- get
  put $ addTradeLog "Order book cleared" $ st
    { appOrderBook = clearBook (appOrderBook st) }

handleKey (V.KChar 'C') _ = handleKey (V.KChar 'c') []

handleKey (V.KChar 'a') _ = do
  st <- get
  let nextId = appNextId st
  orders <- liftIO (generateSeedOrders nextId)
  let (book', logs) = foldl applyOrder (appOrderBook st, []) orders
      applyOrder (b, ls) order =
        let b'  = insertOrder order b
            msg = show (orderType order) ++ " #" ++ show (orderId order)
                ++ " seeded @ " ++ fmtP (orderPrice order)
        in (b', msg : ls)
      (trades, finalBook) = matchOrders book'
      tradeMsgs = map formatTrade trades
      allMsgs   = tradeMsgs ++ reverse logs
      newId     = nextId + length orders
  put $ foldr addTradeLog (st { appOrderBook = finalBook, appNextId = newId }) allMsgs

handleKey (V.KChar 'A') _ = handleKey (V.KChar 'a') []

handleKey _ _ = return ()

randomOrder :: OrderType -> OrderId -> IO Order
randomOrder ot oid = do
  basePrice <- randomRIO (90.0, 110.0 :: Double)
  let price = fromIntegral (round (basePrice * 4) :: Int) / 4.0
  qty <- randomRIO (1, 50 :: Int)
  return $ mkOrder oid ot price qty

generateSeedOrders :: OrderId -> IO [Order]
generateSeedOrders startId = do
  let buyPrices  = [98.0, 99.0, 100.0, 101.0, 102.0]
  let sellPrices = [99.0, 100.0, 101.0, 102.0, 103.0]
  buyQtys  <- mapM (\_ -> randomRIO (5, 30 :: Int)) buyPrices
  sellQtys <- mapM (\_ -> randomRIO (5, 30 :: Int)) sellPrices
  let buys  = zipWith3 (\i p q -> mkOrder i  Buy  p q)
                       [startId..]       buyPrices  buyQtys
      sells = zipWith3 (\i p q -> mkOrder i  Sell p q)
                       [startId+5..] sellPrices sellQtys
  return (buys ++ sells)

formatTrade :: Trade -> String
formatTrade t =
  "Trade: " ++ show (tradeQty t)
  ++ " units @ " ++ fmtP (tradePrice t)
  ++ "  (B#" ++ show (tradeBuyId t)
  ++ " × S#" ++ show (tradeSellId t) ++ ")"

fmtP :: Price -> String
fmtP p = "$" ++ showFFloat (Just 2) p ""

main :: IO ()
main = do
  seedOrders <- generateSeedOrders 1
  let bookWithSeeds = foldr insertOrder emptyOrderBook (reverse seedOrders)
      initSt = initialState
        { appOrderBook = bookWithSeeds { totalOrders = length seedOrders }
        , appNextId    = length seedOrders + 1
        , appTradeLog  =
            [ "Press A to auto-seed + match, M to run matcher"
            , "Order book seeded with " ++ show (length seedOrders) ++ " initial orders"
            , "Welcome to Haskell Order Book Simulator"
            ]
        }
  let buildVty = VCP.mkVty V.defaultConfig
  vty <- buildVty
  _finalSt <- M.customMain vty buildVty Nothing app initSt
  putStrLn "Order Book Simulator exited cleanly."
