{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.XCB.Raw.XCB
  where

import qualified Graphics.XCB.Raw.Types as XT
import qualified Language.C.Inline as LCI

import qualified Foreign.Concurrent as FC
import qualified Foreign.C.Types as FCT
import qualified Foreign.ForeignPtr as FFP
import qualified Foreign.Ptr as FP

-- here we include C's headers
LCI.include "xcb/xcb.h"
LCI.include "stdlib.h"
LCI.include "stddef.h"
LCI.include "errno.h"

{-|
  'xcb_connect' will try to connect to X display.
  Returns:
    Nothing - in case of error
    Just connection - in case of success
-}
xcb_connectIO:: IO (Maybe XT.Connection)
xcb_connectIO = do
    ptr <- [LCI.block| void*
        {
            xcb_connection_t * p_ret = NULL;
            p_ret = xcb_connect(NULL,NULL);
            if( xcb_connection_has_error(p_ret))
            {
                xcb_disconnect( p_ret);
                return NULL;
            }
            return p_ret;
        } |]
    if FP.nullPtr == ptr
        then return Nothing
        else do
            -- if connection has been successfull, setup cleanup routine
            -- which will be called on GC cleanup
            let free_conn ptr =
                    [LCI.block|void {
                        xcb_disconnect($(void* ptr));
                    } |]
            FC.newForeignPtr ptr (free_conn ptr) >>=
                return . Just . XT.Connection

{-|
    this function generates new xcb_window_t value by requesting it from
    server
-}
xcb_generate_idIO
    :: XT.Connection -- ^ current XCB connection
    -> IO (Maybe XT.XID) -- ^ Just XID - in case of success, Nothing - on failure
xcb_generate_idIO (XT.Connection conn ) = do
    FFP.withForeignPtr conn $ \pconn-> do
        idOrZero <- fmap fromIntegral [LCI.block| uint32_t
            {
                xcb_window_t ret = 0;
                ret = xcb_generate_id($(void* pconn));
                if( xcb_connection_has_error( $(void* pconn)))
                {
                    // I don't know yet if I should handle errno, because it can produce race condition
                    return 0;
                }
                return ret;
            } |]
        return $! if idOrZero == 0
            then Nothing
            else Just $ XT.XID idOrZero

createWindowIO
    :: XT.Connection
    -> XT.CreateWindow
    -> IO( Maybe XT.XID)
createWindowIO xconn@(XT.Connection conn) cw = do
    mxid <- xcb_generate_idIO xconn
    case mxid of
        Nothing-> return Nothing
        Just xid@(XT.XID rawXID)-> do
            let
                XT.Position x' y' = XT.cwPosition cw
                XT.Size w' h' = XT.cwSize cw
                x = fromIntegral x'
                y = fromIntegral y'
                w = fromIntegral w'
                h = fromIntegral h'
            okOrZero <- FFP.withForeignPtr conn $! \pconn-> do [LCI.block| uint32_t
                {
                xcb_screen_t * p_screen = NULL;
                xcb_connection_t * p_conn = $(void* pconn);
                const struct xcb_setup_t * setup = xcb_get_setup(p_conn);
                if( !setup || xcb_connection_has_error( p_conn))
                {
                    return 0;
                }
                xcb_screen_iterator_t   iter = xcb_setup_roots_iterator(setup);
                p_screen = iter.data;

                uint32_t u_values[] = { XCB_EVENT_MASK_EXPOSURE
                                      | XCB_EVENT_MASK_STRUCTURE_NOTIFY
                                      };

                xcb_void_cookie_t cookie = xcb_create_window_checked
                  ( p_conn
                  , XCB_COPY_FROM_PARENT
                  , $(uint32_t rawXID)
                  , p_screen-> root
                  , $(uint32_t x)
                  , $(uint32_t y)
                  , $(uint32_t w)
                  , $(uint32_t h)
                  , 0
                  , XCB_WINDOW_CLASS_COPY_FROM_PARENT
                  , XCB_COPY_FROM_PARENT
                  , XCB_CW_EVENT_MASK
                  , u_values
                  );
                if( xcb_request_check( p_conn, cookie)
                 || xcb_connection_has_error( p_conn))
                {
                    return 0;
                }
                cookie = xcb_map_window( p_conn, $(uint32_t rawXID));
                if( xcb_request_check( p_conn, cookie)
                 || xcb_connection_has_error( p_conn))
                {
                    return 0;
                }
                if( xcb_flush( p_conn) <= 0)
                {
                    return 0;
                }
                }|]
            return $! if okOrZero == 0
                then Nothing
                else Just xid



