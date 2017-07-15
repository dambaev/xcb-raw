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
            case mxid of
                Nothing-> error "failed to generate xid"
                Just xid -> do
                    putStrLn $ "xid = " ++ (show xid)
                    return ()
