{-# LANGUAGE ForeignFunctionInterface #-}

module Yf.Tpm (
  TpmContext,
  initialize,
  random,
) where

import Relude

-- import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable

-- newtype TpmContext = TpmContext

type Fapi_Context = ()
newtype TpmContext = TpmContext (ForeignPtr Fapi_Context)

foreign import ccall unsafe "tss2/tss2_fapi.h Fapi_Initialize"
  c_Fapi_Initialize :: Ptr (Ptr Fapi_Context) -> Ptr () -> IO ()

foreign import ccall unsafe "tss2-shim.h &tss_shim_Fapi_Finalize"
  c_Fapi_Finalize :: FunPtr (Ptr Fapi_Context -> IO ())

initialize :: IO TpmContext
initialize = alloca $ \ctxPtrPtr -> do
  c_Fapi_Initialize ctxPtrPtr nullPtr
  ctxPtr <- peek ctxPtrPtr
  frnPtr <- newForeignPtr c_Fapi_Finalize ctxPtr
  pure $ TpmContext frnPtr

foreign import ccall unsafe "tss2/tss2_fapi.h Fapi_GetRandom"
  c_Fapi_GetRandom :: Ptr Fapi_Context -> CSize -> Ptr (Ptr CChar) -> IO CUInt

random :: TpmContext -> CSize -> IO ByteString
random (TpmContext tpmCtx) size = withForeignPtr tpmCtx $ \ctx -> alloca $ \randomDataPtr -> do
  status <- c_Fapi_GetRandom ctx size randomDataPtr
  _ <- when ((fromEnum status) /= 0) $ fail "Recieved error from tpm"
  randomData <- peek randomDataPtr
  ByteString.packCStringLen (randomData, fromEnum size)
