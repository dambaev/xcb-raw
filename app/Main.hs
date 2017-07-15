module Main where

import Graphics.XCB.Raw as XCB

import Data.Maybe
import Control.Monad

main :: IO ()
main = do
    mconnection <- XCB.xcb_connectIO
    case mconnection of
        Nothing-> do
            error "failed to conenct to display"
        Just connection-> do
            mxid <- xcb_generate_idIO connection
            when (mxid == Nothing) $! error "failed to generate xid"
