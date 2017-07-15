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


xcb_connect:: IO (Maybe XT.Connection)
xcb_connect = do
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
            let free_conn ptr =
                    [LCI.block|void {
                        xcb_disconnect($(void* ptr));
                    } |]
            FC.newForeignPtr ptr (free_conn ptr) >>=
                return . Just . XT.Connection




