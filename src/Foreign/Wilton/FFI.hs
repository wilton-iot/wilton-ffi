-- |
-- Module:      Foreign.Wilton.FFI
-- Copyright:   (c) 2018, alex at staticlibs.net
-- License:     MIT
-- Maintainer:  alex at staticlibs.net
-- Stability:   experimental
-- Portability: portable
--
-- Haskell modules support for [Wilton JavaScript runtime](https://github.com/wilton-iot/wilton).

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Foreign.Wilton.FFI (
    --
    -- * Usage example:
    -- $use
    registerWiltoncall,
    createWiltonError
    ) where

import qualified Control.Exception as E (SomeException, catch)
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode)
import Data.ByteString (ByteString, useAsCString, useAsCStringLen, packCString, packCStringLen)
import qualified Data.ByteString as BS (concat, length, putStrLn)
import qualified Data.ByteString.Lazy as BL (fromChunks, toChunks)
{- Data.ByteString.UTF8 (fromString) -}
import qualified Data.ByteString.Char8 as BC8 (pack)
import Data.Data (Data, constrFields, dataTypeConstrs, dataTypeOf)
import Foreign.Ptr (Ptr, FunPtr, nullPtr, ptrToIntPtr)
import Foreign.C.String (CString, CStringLen)
import Foreign.C.Types
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (poke, pokeByteOff)


-- callback types

type WiltonCallback = Ptr () -> CString -> CInt -> Ptr CString -> Ptr CInt -> IO CString
type WiltonCallbackInternal = ByteString -> IO (Either ByteString ByteString)


-- wilton C API import
-- https://github.com/wilton-iot/wilton_core/tree/master/include/wilton

foreign import ccall unsafe "wilton_alloc"
    wilton_alloc :: CInt -> IO CString

foreign import ccall unsafe "wilton_free"
    wilton_free :: CString -> IO ()

foreign import ccall safe "wiltoncall_register"
    wiltoncall_register :: CString -> CInt -> Ptr () -> FunPtr WiltonCallback -> IO CString


-- function pointer wrapper

foreign import ccall "wrapper"
    createCallbackPtr :: WiltonCallback -> IO (FunPtr WiltonCallback)


-- helper functions

copyToWiltonBuffer :: ByteString -> IO CString
copyToWiltonBuffer bs = do
    res <- wilton_alloc (fromIntegral ((BS.length bs) + 1))
    useAsCString bs (\cs ->
        copyBytes res cs (BS.length bs))
    pokeByteOff res (BS.length bs) (0 :: CChar)
    return res

wrapBsCallback :: WiltonCallbackInternal -> WiltonCallback
wrapBsCallback cb = (\_ jsonCs jsonCsLen jsonOutPtr jsonOutLenPtr -> do
    dataBs <- if 0 /= ptrToIntPtr jsonCs && jsonCsLen > 0
        then packCStringLen (jsonCs, (fromIntegral jsonCsLen))
        else return (BC8.pack "{}")
    respEither <- E.catch
        (cb dataBs)
        (\(e :: E.SomeException) -> do
            return (Left (BC8.pack (show e))))
    either
        (\errBs -> do
            errCs <- copyToWiltonBuffer errBs
            return errCs)
        (\respBs -> do
            respCs <- copyToWiltonBuffer respBs
            poke jsonOutPtr respCs
            poke jsonOutLenPtr (fromIntegral (BS.length respBs))
            return nullPtr)
        respEither )

-- | Registers a function, that can be called from javascript
--
-- This function takes a function and registers it with Wilton, so
-- it can be called from JavaScript using [wiltoncall](https://wilton-iot.github.io/wilton/docs/html/namespacewiltoncall.html)
-- API.
--
-- Function must take a single argument - a data that implements
-- [Data.Aeson.FromJSON](https://hackage.haskell.org/package/aeson-1.3.0.0/docs/Data-Aeson.html#t:FromJSON) and
-- [Data.Data.Data](https://hackage.haskell.org/package/base-4.11.0.0/docs/Data-Data.html#t:Data),
-- and must return a data that implements
-- [Data.Aeson.ToJSON](https://hackage.haskell.org/package/aeson-1.3.0.0/docs/Data-Aeson.html#t:ToJSON).
-- Function input argument is converted from JavaScript object to Haskell data object.
-- Function output is returned to JavaScript as a JSON (that can be immediately converted to JavaScript object).
--
-- If function raises and @Exception@, its error message is converted into JavasSript `Error` message (that can be
-- caught and handled on JavaScript side).
--
-- Arguments:
--
--    * @name :: String@: name for this call, that should be used from JavaScript to invoke the function
--    * @callback :: (from -> IO to)@: Function, that will be called from JavaScript
--
-- Return value: error status.
--
registerWiltoncall :: forall from to. (Data from, FromJSON from, ToJSON to) => String -> (from -> IO to) -> IO (Maybe ByteString)
registerWiltoncall nameString cbJson = do
    let cbBs = (\jsonBs -> either
            (\e -> return (Left (BC8.pack ("JSON parse error: [" ++ (show e) ++ "], required fields: " ++
                ((show . (map constrFields) . dataTypeConstrs . dataTypeOf) (undefined::from))))))
            (\obj -> do
                resObj <- cbJson obj
                let resBs = BS.concat (BL.toChunks (encode resObj))
                return (Right resBs) )
            (eitherDecode (BL.fromChunks [jsonBs]) :: Either String from) )
    let cbCs = wrapBsCallback cbBs
    cb <- createCallbackPtr cbCs
    let name = BC8.pack nameString
    errc <- useAsCStringLen name (\(cs, len) ->
        wiltoncall_register cs (fromIntegral len) nullPtr cb )
    if 0 /= ptrToIntPtr errc then do
        bs <- packCString errc
        wilton_free errc
        return (Just bs)
    else return Nothing

-- | Create an error message, that can be passed back to Wilton
--
-- Helper function, that can be used with a @Maybe ByteString@ value returned
-- from @registerWiltoncall@ function.
--
-- Arguments:
--
--    * @error :: Maybe ByteString@: error status
--
-- Return value: error status, that can be returned back to Wilton
--
createWiltonError :: Maybe ByteString -> IO CString
createWiltonError errBsMaybe =
    maybe
        (return nullPtr)
        copyToWiltonBuffer
        errBsMaybe

-- $use
--
-- Add @aeson@ and @wilton-ffi@ deps to @package.yaml@:
--
-- > dependencies:
-- >    - ...
-- >    - aeson
-- >    - wilton-ffi
--
-- Inside @Lib.hs@, enable required extensions:
--
-- > {-# LANGUAGE DeriveDataTypeable #-}
-- > {-# LANGUAGE DeriveGeneric #-}
-- > {-# LANGUAGE ForeignFunctionInterface #-}
--
-- Import @aeson@, @wilton-ffi@ and other deps:
--
-- > import Data.Aeson
-- > import Data.Data
-- > import GHC.Generics
-- > import Foreign.C.String
-- > import Foreign.Wilton.FFI
--
-- Declare input/output structs:
--
-- > data MyIn = MyIn {} deriving (Typeable, Data, Generic, Show)
-- > instance FromJSON MyIn
-- > data MyOut = MyOut {} deriving (Generic, Show)
-- > instance ToJSON MyObjOut
--
-- Write a function that does some work:
--
-- > hello :: MyIn -> IO MyOut
-- > hello obj = ...
--
-- Register that function inside the `wilton_module_init` function,
-- that will be called by Wilton during the Haskell module load:
--
-- > foreign export ccall wilton_module_init :: IO CString
-- > wilton_module_init :: IO CString
-- > wilton_module_init = do
-- >     -- register a call, error checking omitted
-- >     _ <- registerWiltoncall "hello" hello
-- >     -- return success status to Wilton
-- >     createWiltonError Nothing
--
-- Build the module as shared library (change RTS version as needed):
--
-- > > stack build
-- > > stack ghc -- --make -dynamic -shared -fPIC -threaded -lHSrts_thr-ghc8.2.2 src/Lib.hs -o libsome_name.so
--
-- See an [example](https://github.com/wilton-iot/wilton_examples/blob/master/haskell/test.js#L17)
-- how to load and use Haskell library from JavaScript.
--
