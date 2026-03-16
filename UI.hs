{-# LANGUAGE OverloadedStrings #-}
module OrderBook.UI
  ( drawUI
  , theMap
  ) where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import qualified Graphics.Vty as V
import           Data.List (intercalate)
import           Numeric (showFFloat)

import OrderBook.Types
import OrderBook.State

buyAttr, sellAttr, tradeAttr, headerAttr, labelAttr,
  priceAttr, dimAttr, accentAttr, footerAttr :: AttrName
buyAttr    = attrName "buy"
sellAttr   = attrName "sell"
tradeAttr  = attrName "trade"
headerAttr = attrName "header"
labelAttr  = attrName "label"
priceAttr  = attrName "price"
dimAttr    = attrName "dim"
accentAttr = attrName "accent"
footerAttr = attrName "footer"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (buyAttr,    V.withForeColor (V.withStyle V.defAttr V.bold) (V.rgbColor 0 230 120))
  , (sellAttr,   V.withForeColor (V.withStyle V.defAttr V.bold) (V.rgbColor 255 80  80))
  , (tradeAttr,  V.withForeColor (V.withStyle V.defAttr V.bold) (V.rgbColor 255 210 0))
  , (headerAttr, V.withForeColor (V.withStyle V.defAttr V.bold) (V.rgbColor 80  180 255))
  , (labelAttr,  V.withForeColor V.defAttr                      (V.rgbColor 160 160 200))
  , (priceAttr,  V.withForeColor V.defAttr                      (V.rgbColor 220 220 255))
  , (dimAttr,    V.withForeColor V.defAttr                      (V.rgbColor 100 100 130))
  , (accentAttr, V.withForeColor (V.withStyle V.defAttr V.bold) (V.rgbColor 255 160 30))
  , (footerAttr, V.withForeColor V.defAttr                      (V.rgbColor 130 130 160))
  , (borderAttr, V.withForeColor V.defAttr                      (V.rgbColor 60  60  90))
  ]

drawUI :: AppState -> [Widget AppName]
drawUI st = [ui]
  where
    ui = vBox
      [ drawHeader st
      , hBox
          [ drawBuyPanel  (buyOrders  (appOrderBook st))
          , drawSpread    (appOrderBook st)
          , drawSellPanel (sellOrders (appOrderBook st))
          ]
      , drawTradeLog (appTradeLog st)
      , drawFooter
      ]

drawHeader :: AppState -> Widget AppName
drawHeader st =
  withBorderStyle unicodeBold $
  borderWithLabel (withAttr accentAttr $ str " ▸ ORDER BOOK SIMULATOR ◂ ") $
  padLeftRight 2 $
  hBox
    [ withAttr headerAttr $ str "  ❰ HASKELL TRADING TERMINAL ❱"
    , fill ' '
    , withAttr labelAttr  $ str "Orders Processed: "
    , withAttr priceAttr  $ str (show (totalOrders (appOrderBook st)))
    , padLeft (Pad 4) $ withAttr labelAttr $ str "Last Trade: "
    , lastTradeWidget (lastTradePrice (appOrderBook st))
    , str "  "
    ]

lastTradeWidget :: Maybe Price -> Widget AppName
lastTradeWidget Nothing  = withAttr dimAttr $ str "—"
lastTradeWidget (Just p) = withAttr tradeAttr $ str (fmtPrice p)

drawBuyPanel :: [Order] -> Widget AppName
drawBuyPanel orders =
  withBorderStyle unicodeRounded $
  borderWithLabel (withAttr buyAttr $ str " ▲ BIDS (Buy Orders) ") $
  vBox
    [ drawOrderTableHeader buyAttr
    , hBorder
    , if null orders
        then padTop (Pad 1) $ hCenter $ withAttr dimAttr $ str "No buy orders"
        else vBox (map (drawOrderRow buyAttr) orders)
    , fill ' '
    , hBorder
    , withAttr dimAttr $
        padLeftRight 1 $
        str ("Count: " ++ show (length orders) ++ totalQtyStr orders)
    ]

drawSellPanel :: [Order] -> Widget AppName
drawSellPanel orders =
  withBorderStyle unicodeRounded $
  borderWithLabel (withAttr sellAttr $ str " ▼ ASKS (Sell Orders) ") $
  vBox
    [ drawOrderTableHeader sellAttr
    , hBorder
    , if null orders
        then padTop (Pad 1) $ hCenter $ withAttr dimAttr $ str "No sell orders"
        else vBox (map (drawOrderRow sellAttr) orders)
    , fill ' '
    , hBorder
    , withAttr dimAttr $
        padLeftRight 1 $
        str ("Count: " ++ show (length orders) ++ totalQtyStr orders)
    ]

drawSpread :: OrderBook -> Widget AppName
drawSpread book =
  withBorderStyle unicode $
  borderWithLabel (withAttr accentAttr $ str " ≈ SPREAD ") $
  hLimit 22 $
  vBox
    [ padTop (Pad 1) $
      hCenter $ withAttr labelAttr $ str "Best Bid"
    , hCenter $ withAttr buyAttr $
        str (maybe "   —   " fmtPrice bestBid)
    , padTop (Pad 1) $
      hCenter $ withAttr labelAttr $ str "Spread"
    , hCenter $ withAttr tradeAttr $
        str (spreadStr bestBid bestAsk)
    , padTop (Pad 1) $
      hCenter $ withAttr labelAttr $ str "Best Ask"
    , hCenter $ withAttr sellAttr $
        str (maybe "   —   " fmtPrice bestAsk)
    , fill ' '
    , padTop (Pad 1) $
      hCenter $ withAttr labelAttr $ str "Mid Price"
    , hCenter $ withAttr priceAttr $
        str (midPriceStr bestBid bestAsk)
    , padBottom (Pad 1) $ str ""
    ]
  where
    bestBid = case buyOrders  book of { [] -> Nothing; (x:_) -> Just (orderPrice x) }
    bestAsk = case sellOrders book of { [] -> Nothing; (x:_) -> Just (orderPrice x) }

spreadStr :: Maybe Price -> Maybe Price -> String
spreadStr (Just b) (Just a) = fmtPrice (a - b)
spreadStr _ _               = "   —   "

midPriceStr :: Maybe Price -> Maybe Price -> String
midPriceStr (Just b) (Just a) = fmtPrice ((a + b) / 2)
midPriceStr _ _               = "   —   "

drawOrderTableHeader :: AttrName -> Widget AppName
drawOrderTableHeader attr =
  withAttr attr $
  padLeftRight 1 $
  hBox
    [ fixedCol 8  "ORDER ID"
    , fixedCol 12 "PRICE"
    , fixedCol 10 "QTY"
    , fixedCol 14 "NOTIONAL"
    ]

drawOrderRow :: AttrName -> Order -> Widget AppName
drawOrderRow attr order =
  withAttr attr $
  padLeftRight 1 $
  hBox
    [ fixedCol 8  ("#" ++ show (orderId order))
    , fixedCol 12 (fmtPrice (orderPrice order))
    , fixedCol 10 (show (orderQty order))
    , withAttr dimAttr $
      fixedCol 14 (fmtPrice (orderPrice order * fromIntegral (orderQty order)))
    ]

fixedCol :: Int -> String -> Widget AppName
fixedCol n s = str (take n (s ++ repeat ' '))

totalQtyStr :: [Order] -> String
totalQtyStr [] = "   Total Qty: 0"
totalQtyStr os = "   Total Qty: " ++ show (sum (map orderQty os))

drawTradeLog :: [String] -> Widget AppName
drawTradeLog msgs =
  withBorderStyle unicodeRounded $
  borderWithLabel (withAttr tradeAttr $ str " ◈ EXECUTION LOG ") $
  vLimit 8 $
  vBox (logLines ++ [fill ' '])
  where
    logLines
      | null msgs = [hCenter (withAttr dimAttr (str "No trades yet"))]
      | otherwise = map renderMsg (take 6 msgs)
    renderMsg msg =
      padLeft (Pad 2) $
      hBox [ withAttr accentAttr $ str "▸ "
           , withAttr tradeAttr  $ str msg
           ]

drawFooter :: Widget AppName
drawFooter =
  withAttr footerAttr $
  padLeftRight 2 $
  hBox
    [ key "B" "Buy Order"
    , sep
    , key "S" "Sell Order"
    , sep
    , key "M" "Match"
    , sep
    , key "C" "Clear Book"
    , sep
    , key "A" "Auto-seed"
    , sep
    , withAttr sellAttr $ str "[Q]"
    , withAttr footerAttr $ str " Quit"
    ]
  where
    key k desc = hBox [ withAttr accentAttr $ str ("[" ++ k ++ "]")
                       , withAttr footerAttr $ str (" " ++ desc) ]
    sep = withAttr dimAttr $ str "  │  "

fmtPrice :: Price -> String
fmtPrice p = "$" ++ showFFloat (Just 2) p ""
