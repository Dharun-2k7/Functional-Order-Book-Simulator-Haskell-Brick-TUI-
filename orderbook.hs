-- Order Book Simulator in Haskell

import Data.List (sortBy)
import Data.Ord (comparing)


data OrderType = Buy | Sell deriving (Show, Eq)


data Order = Order {
    orderId :: Int,
    orderType :: OrderType,
    price :: Double,
    quantity :: Int
} deriving (Show)


data OrderBook = OrderBook {
    buys :: [Order],
    sells :: [Order]
} deriving (Show)



emptyBook :: OrderBook
emptyBook = OrderBook [] []



insertBuy :: Order -> [Order] -> [Order]
insertBuy o [] = [o]
insertBuy o (x:xs)
    | price o > price x = o : x : xs
    | otherwise = x : insertBuy o xs


insertSell :: Order -> [Order] -> [Order]
insertSell o [] = [o]
insertSell o (x:xs)
    | price o < price x = o : x : xs
    | otherwise = x : insertSell o xs


matchOrders :: [Order] -> [Order] -> ([Order], [Order])
matchOrders [] sells = ([], sells)
matchOrders buys [] = (buys, [])
matchOrders (b:bs) (s:ss)

    | price b >= price s =
        let tradeQty = min (quantity b) (quantity s)
            bRem = quantity b - tradeQty
            sRem = quantity s - tradeQty
        in
        case (bRem, sRem) of

            (0,0) -> matchOrders bs ss

            (0,_) -> matchOrders bs (Order (orderId s) Sell (price s) sRem : ss)

            (_,0) -> matchOrders (Order (orderId b) Buy (price b) bRem : bs) ss

            _ -> matchOrders
                    (Order (orderId b) Buy (price b) bRem : bs)
                    (Order (orderId s) Sell (price s) sRem : ss)

    | otherwise = (b:bs, s:ss)


processOrder :: OrderBook -> Order -> OrderBook
processOrder (OrderBook b s) order =
    case orderType order of

        Buy ->
            let newBuys = insertBuy order b
                (b', s') = matchOrders newBuys s
            in OrderBook b' s'

        Sell ->
            let newSells = insertSell order s
                (b', s') = matchOrders b newSells
            in OrderBook b' s'


printBook :: OrderBook -> IO ()
printBook (OrderBook b s) = do
    putStrLn "\nBuy Orders:"
    mapM_ print b
    putStrLn "\nSell Orders:"
    mapM_ print s


orders :: [Order]
orders =
    [ Order 1 Buy 100 10
    , Order 2 Sell 99 5
    , Order 3 Buy 101 3
    , Order 4 Sell 100 8
    , Order 5 Buy 102 6
    ]


simulate :: OrderBook -> [Order] -> IO ()
simulate book [] = printBook book
simulate book (o:os) = do
    putStrLn ("\nProcessing Order: " ++ show o)
    let newBook = processOrder book o
    simulate newBook os


main :: IO ()
main = do
    putStrLn "Starting Order Book Simulation..."
    simulate emptyBook orders
