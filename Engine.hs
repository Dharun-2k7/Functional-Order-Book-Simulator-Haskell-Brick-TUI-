module OrderBook.Engine
  ( insertOrder
  , insertBuy
  , insertSell
  , matchOrders
  , addAndMatch
  , clearBook
  ) where

import OrderBook.Types

insertOrder :: Order -> OrderBook -> OrderBook
insertOrder order book =
  let book' = book { totalOrders = totalOrders book + 1 }
  in case orderType order of
    Buy  -> book' { buyOrders  = insertBuy  order (buyOrders  book') }
    Sell -> book' { sellOrders = insertSell order (sellOrders book') }

insertBuy :: Order -> [Order] -> [Order]
insertBuy order [] = [order]
insertBuy order (x:xs)
  | orderPrice order >= orderPrice x = order : x : xs
  | otherwise = x : insertBuy order xs

insertSell :: Order -> [Order] -> [Order]
insertSell order [] = [order]
insertSell order (x:xs)
  | orderPrice order <= orderPrice x = order : x : xs
  | otherwise = x : insertSell order xs

matchOrders :: OrderBook -> ([Trade], OrderBook)
matchOrders book = go (buyOrders book) (sellOrders book) [] book
  where
    go [] _ trades b = (reverse trades, b)
    go _ [] trades b = (reverse trades, b)
    go (bid:bids) (ask:asks) trades b
      | orderPrice bid < orderPrice ask =
          (reverse trades, b)
      | otherwise =
          let execPrice = orderPrice ask
              execQty   = min (orderQty bid) (orderQty ask)
              trade     = Trade
                { tradeQty    = execQty
                , tradePrice  = execPrice
                , tradeBuyId  = orderId bid
                , tradeSellId = orderId ask
                }
              newBids = updateQty execQty bid bids
              newAsks = updateQty execQty ask asks
              b'      = b { buyOrders      = newBids
                          , sellOrders     = newAsks
                          , lastTradePrice = Just execPrice
                          }
          in go newBids newAsks (trade : trades) b'

updateQty :: Quantity -> Order -> [Order] -> [Order]
updateQty execQty order rest
  | orderQty order <= execQty = rest
  | otherwise = order { orderQty = orderQty order - execQty } : rest

addAndMatch :: Order -> OrderBook -> ([Trade], OrderBook)
addAndMatch order book =
  let book' = insertOrder order book
  in matchOrders book'

clearBook :: OrderBook -> OrderBook
clearBook book = emptyOrderBook { totalOrders = totalOrders book }
