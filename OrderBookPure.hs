module Main (main) where

import Data.List  (sortBy, intercalate)
import Data.Ord   (comparing, Down(..))
import Data.Maybe (fromMaybe)

data OrderType = Buy | Sell
  deriving (Show, Eq, Ord)

type OrderId  = Int
type Price    = Double
type Quantity = Int

data Order = Order
  { orderId    :: !OrderId
  , orderType  :: !OrderType
  , orderPrice :: !Price
  , orderQty   :: !Quantity
  } deriving (Eq)

instance Show Order where
  show o = "#" ++ show (orderId o)
        ++ " " ++ show (orderType o)
        ++ " @ $" ++ showP (orderPrice o)
        ++ " x "  ++ show (orderQty o)

data OrderBook = OrderBook
  { buyOrders        :: ![Order]
  , sellOrders       :: ![Order]
  , executedTrades   :: ![(Price, Quantity, OrderId, OrderId)]
  , totalOrdersSeen  :: !Int
  } deriving (Show)

data Trade = Trade
  { tradePrice  :: !Price
  , tradeQty    :: !Quantity
  , tradeBuyId  :: !OrderId
  , tradeSellId :: !OrderId
  } deriving (Show)

emptyBook :: OrderBook
emptyBook = OrderBook [] [] [] 0

mkOrder :: OrderId -> OrderType -> Price -> Quantity -> Order
mkOrder = Order

insertOrder :: Order -> OrderBook -> OrderBook
insertOrder o book =
  let book' = book { totalOrdersSeen = totalOrdersSeen book + 1 }
  in case orderType o of
       Buy  -> book' { buyOrders  = insertBuy  o (buyOrders  book') }
       Sell -> book' { sellOrders = insertSell o (sellOrders book') }

insertBuy :: Order -> [Order] -> [Order]
insertBuy x []     = [x]
insertBuy x (y:ys)
  | orderPrice x >= orderPrice y = x : y : ys
  | otherwise                     = y : insertBuy x ys

insertSell :: Order -> [Order] -> [Order]
insertSell x []     = [x]
insertSell x (y:ys)
  | orderPrice x <= orderPrice y = x : y : ys
  | otherwise                     = y : insertSell x ys

runMatchingEngine :: OrderBook -> (OrderBook, [Trade])
runMatchingEngine book = go book []
  where
    go b@(OrderBook [] _ _ _) acc = (b, reverse acc)
    go b@(OrderBook _ [] _ _) acc = (b, reverse acc)
    go b@(OrderBook (bid:bids) (ask:asks) _ _) acc
      | orderPrice bid < orderPrice ask  = (b, reverse acc)
      | otherwise =
          let execPrice = orderPrice ask
              execQty   = min (orderQty bid) (orderQty ask)
              trade     = Trade execPrice execQty (orderId bid) (orderId ask)
              newBids   = partialFill execQty bid bids
              newAsks   = partialFill execQty ask asks
              logEntry  = (execPrice, execQty, orderId bid, orderId ask)
              b'        = b { buyOrders      = newBids
                            , sellOrders     = newAsks
                            , executedTrades = logEntry : executedTrades b
                            }
          in go b' (trade : acc)

partialFill :: Quantity -> Order -> [Order] -> [Order]
partialFill qty order rest
  | orderQty order <= qty = rest
  | otherwise              = order { orderQty = orderQty order - qty } : rest

processOrders :: [Order] -> OrderBook -> OrderBook
processOrders []     book = book
processOrders (o:os) book =
  let book'          = insertOrder o book
      (book'', _)    = runMatchingEngine book'
  in processOrders os book''

showP :: Price -> String
showP p =
  let (i, _) = properFraction (p * 100) :: (Int, Double)
      cents  = abs i `mod` 100
  in show (abs i `div` 100) ++ "." ++ pad2 cents
  where pad2 n = if n < 10 then "0" ++ show n else show n

green, red, yellow, cyan, bold, reset :: String
green  = "\ESC[32m"
red    = "\ESC[31m"
yellow = "\ESC[33m"
cyan   = "\ESC[36m"
bold   = "\ESC[1m"
reset  = "\ESC[0m"

colWidth :: Int
colWidth = 44

padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '

header :: String -> String
header title = bold ++ cyan ++ replicate colWidth '═' ++ reset ++ "\n"
            ++ bold ++ cyan ++ "  " ++ title ++ reset ++ "\n"
            ++ bold ++ cyan ++ replicate colWidth '─' ++ reset

tableHeader :: String
tableHeader = "  " ++ padRight 8 "ID"
           ++ padRight 12 "PRICE"
           ++ padRight 10 "QTY"
           ++ padRight 14 "NOTIONAL"

formatOrderRow :: String -> Order -> String
formatOrderRow color o =
  color
  ++ "  "
  ++ padRight 8  ("#" ++ show (orderId o))
  ++ padRight 12 ("$" ++ showP (orderPrice o))
  ++ padRight 10 (show (orderQty o))
  ++ padRight 14 ("$" ++ showP (orderPrice o * fromIntegral (orderQty o)))
  ++ reset

formatTrade :: (Price, Quantity, OrderId, OrderId) -> String
formatTrade (p, q, bid, sid) =
  yellow ++ "  ▸ Trade: " ++ show q ++ " units @ $" ++ showP p
         ++ "  (Buy #" ++ show bid ++ " × Sell #" ++ show sid ++ ")"
         ++ reset

printOrderBook :: OrderBook -> IO ()
printOrderBook book = do
  putStrLn ""
  putStrLn $ header "ORDER BOOK STATE"
  putStrLn ""

  putStrLn $ bold ++ green ++ "  ▲ BUY ORDERS (Bids, descending price)" ++ reset
  putStrLn tableHeader
  putStrLn (replicate colWidth '-')
  case buyOrders book of
    [] -> putStrLn $ "  " ++ yellow ++ "(empty)" ++ reset
    os -> mapM_ (putStrLn . formatOrderRow green) os

  putStrLn ""

  putStrLn $ bold ++ red ++ "  ▼ SELL ORDERS (Asks, ascending price)" ++ reset
  putStrLn tableHeader
  putStrLn (replicate colWidth '-')
  case sellOrders book of
    [] -> putStrLn $ "  " ++ yellow ++ "(empty)" ++ reset
    os -> mapM_ (putStrLn . formatOrderRow red) os

  putStrLn ""

  putStrLn $ header "EXECUTION LOG"
  case executedTrades book of
    [] -> putStrLn $ "  " ++ yellow ++ "(no trades executed)" ++ reset
    ts -> mapM_ (putStrLn . formatTrade) (reverse ts)

  putStrLn ""

  putStrLn $ header "STATISTICS"
  putStrLn $ "  Orders processed : " ++ show (totalOrdersSeen book)
  putStrLn $ "  Trades executed  : " ++ show (length (executedTrades book))
  putStrLn $ "  Remaining buys   : " ++ show (length (buyOrders book))
  putStrLn $ "  Remaining sells  : " ++ show (length (sellOrders book))
  let lastTrade = case executedTrades book of
                    [] -> "none"
                    ((p,_,_,_):_) -> "$" ++ showP p
  putStrLn $ "  Last trade price : " ++ lastTrade
  putStrLn $ bold ++ cyan ++ replicate colWidth '═' ++ reset
  putStrLn ""

sampleOrders :: [Order]
sampleOrders =
  [ mkOrder 1  Sell 102.00 20
  , mkOrder 2  Sell 101.50 15
  , mkOrder 3  Sell 101.00 10
  , mkOrder 4  Sell 100.50 25
  , mkOrder 5  Buy  100.50 8
  , mkOrder 6  Buy  101.00 10
  , mkOrder 7  Buy  103.00 50
  , mkOrder 8  Sell 99.00  12
  , mkOrder 9  Sell 99.50  18
  , mkOrder 10 Sell 98.00  5
  , mkOrder 11 Buy  97.00  30
  , mkOrder 12 Buy  97.50  20
  , mkOrder 13 Buy  98.00  15
  , mkOrder 14 Sell 97.50  25
  , mkOrder 15 Buy  102.00 8
  ]

main :: IO ()
main = do
  putStrLn ""
  putStrLn $ bold ++ cyan ++ "╔══════════════════════════════════════════╗" ++ reset
  putStrLn $ bold ++ cyan ++ "║   HASKELL ORDER BOOK SIMULATOR           ║" ++ reset
  putStrLn $ bold ++ cyan ++ "║   Pure Functional — No External Deps     ║" ++ reset
  putStrLn $ bold ++ cyan ++ "╚══════════════════════════════════════════╝" ++ reset

  putStrLn $ "\n" ++ yellow
          ++ "  Processing " ++ show (length sampleOrders)
          ++ " sample orders..." ++ reset ++ "\n"

  let finalBook = processOrders sampleOrders emptyBook

  printOrderBook finalBook

  putStrLn $ green ++ bold ++ "  Simulation complete." ++ reset ++ "\n"
