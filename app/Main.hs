module Main where

import Graphics.XCB.Raw as XCB

import Data.Maybe

main :: IO ()
main = do
    mconnection <- XCB.xcb_connect
    case mconnection of
        Nothing-> do
            error "failed to conenct to display"
        Just some-> do
            return ()
