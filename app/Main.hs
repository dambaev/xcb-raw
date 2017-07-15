module Main where

import Graphics.XCB.Raw as XCB

import Data.Maybe

main :: IO ()
main = do
    mconnection <- XCB.xcb_connectIO
    case mconnection of
        Nothing-> do
            error "failed to conenct to display"
        Just connection-> do
            mxid <- xcb_generate_idIO connection
            _ <- case mxid of
                Nothing-> error "failed to generate xid"
                _ -> return ()
            mwid <- createWindowIO connection
                $ createWindowPosition (Position 100 100)
                $ createWindowSize (Size 100 100)
                $ defCreateWindow
            _ <- case mwid of
                Nothing-> error "failed to create window"
                _ -> return ()
            return ()
