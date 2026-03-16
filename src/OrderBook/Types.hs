module OrderBook.Types
  ( OrderType(..)
  , OrderId
  , Price
  , Quantity
  , Order(..)
  , OrderBook(..)
  , Trade(..)
  , emptyOrderBook
  , mkOrder
  ) where

data OrderType
  = Buy
  | Sell
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type OrderId = Int
type Price = Double
type Quantity = Int

data Order = Order
  { orderId   :: !OrderId
  , orderType :: !OrderType
  , orderPrice :: !Price
  , orderQty  :: !Quantity
  } deriving (Eq, Show)

instance Ord Order where
  compare o1 o2 = compare (orderPrice o1) (orderPrice o2)

data OrderBook = OrderBook
  { buyOrders   :: ![Order]
  , sellOrders  :: ![Order]
  , totalOrders :: !Int
  , lastTradePrice :: !(Maybe Price)
  } deriving (Show)

data Trade = Trade
  { tradeQty   :: !Quantity
  , tradePrice :: !Price
  , tradeBuyId :: !OrderId
  , tradeSellId :: !OrderId
  } deriving (Eq, Show)

emptyOrderBook :: OrderBook
emptyOrderBook = OrderBook
  { buyOrders      = []
  , sellOrders     = []
  , totalOrders    = 0
  , lastTradePrice = Nothing
  }

mkOrder :: OrderId -> OrderType -> Price -> Quantity -> Order
mkOrder oid ot p q = Order
  { orderId    = oid
  , orderType  = ot
  , orderPrice = p
  , orderQty   = q
  }
