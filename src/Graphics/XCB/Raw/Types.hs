
module Graphics.XCB.Raw.Types
  where

import qualified Foreign.C.Types as FCT
import qualified Data.Word as DW
import qualified Foreign.ForeignPtr as FFP(ForeignPtr)
import qualified Foreign.Ptr as FP(Ptr)


data Connection = Connection (FFP.ForeignPtr ())
data RawEvent = RawEvent FCT.CInt (FP.Ptr ())

data XID = XID DW.Word32
  deriving ( Show
           , Eq
           , Ord
           )

data Event
  = XCB_KEY_PRESS
    { xeWindowId:: XID
    }
  | XCB_KEY_RELEASE
    { xeWindowId:: XID
    }
  | XCB_BUTTON_PRESS
    { xeWindowId:: XID
    }
  | XCB_BUTTON_RELEASE
    { xeWindowId:: XID
    }
  | XCB_ENTER_NOTIFY
    { xeWindowId:: XID
    }
  | XCB_LEAVE_NOTIFY
    { xeWindowId:: XID
    }
  | XCB_MAP_NOTIFY
    { xeWindowId:: XID
    }
  | XCB_CONFIGURE_NOTIFY
    { xeWindowId:: XID
    }
  | XCB_EXPOSE
    { xeWindowId:: XID
    }
  | XCB_DESTROY_NOTIFY
    { xeWindowId:: XID
    }
  deriving Show

data Size = Size
  { szWidth:: Int
  , szHeight:: Int
  }

data Position = Position
  { posX:: Int
  , posY:: Int
  }

{--
 - This data structure is used in createWindow action
-}
data CreateWindow = CreateWindow
  { cwParent:: Maybe XID
  , cwSize:: Size
  , cwPosition:: Position
  }

defCreateWindow:: CreateWindow
defCreateWindow = CreateWindow
  { cwParent = Nothing
  , cwSize = Size 0 0
  , cwPosition = Position 0 0
  }

createWindowParent:: XID-> CreateWindow-> CreateWindow
createWindowParent xid wnd = wnd
  { cwParent = Just xid
  }

createWindowPosition:: Position -> CreateWindow-> CreateWindow
createWindowPosition pos wnd = wnd{ cwPosition = pos }

createWindowSize::Size-> CreateWindow -> CreateWindow
createWindowSize rect wnd = wnd { cwSize = rect }
