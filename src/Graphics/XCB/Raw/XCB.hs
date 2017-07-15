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
    this function generates new xcb_window_t value be requesting it from
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

