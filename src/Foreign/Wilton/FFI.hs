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
    registerWiltonCall,
    createWiltonError,
    runWiltonScript
    ) where

import Data.ByteString (ByteString)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.String (CString, CStringLen)
import Foreign.C.Types (CChar, CInt(..))

import qualified Control.Exception as Exception (SomeException, catch)
import qualified Data.Aeson as Aeson (FromJSON, ToJSON, Value, encode, eitherDecode)
import qualified Data.ByteString as ByteString (concat, length, putStrLn, useAsCString, packCString, packCStringLen)
import qualified Data.ByteString.Lazy as ByteStringLazy (fromChunks, toChunks)
import qualified Data.ByteString.UTF8 as UTF8 (fromString, toString)
import qualified Data.Data as Data (Data, constrFields, dataTypeConstrs, dataTypeOf)
import qualified Foreign.Ptr as Ptr (Ptr, FunPtr, nullPtr, ptrToIntPtr)
import qualified Foreign.Marshal.Alloc as Alloc (alloca)
import qualified Foreign.Marshal.Utils as MarshalUtils (copyBytes)
import qualified Foreign.Storable as Storable (peek, poke, pokeByteOff)


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

foreign import ccall safe "wiltoncall_runscript"
    wiltoncall_runscript :: CString -> CInt -> CString -> CInt -> Ptr (CString) -> Ptr (CInt) -> IO CString

-- function pointer wrapper

foreign import ccall "wrapper"
    createCallbackPtr :: WiltonCallback -> IO (FunPtr WiltonCallback)


-- helper functions

copyToWiltonBuffer :: ByteString -> IO CString
copyToWiltonBuffer bs = do
    res <- wilton_alloc (fromIntegral ((ByteString.length bs) + 1))
    ByteString.useAsCString bs (\cs ->
        MarshalUtils.copyBytes res cs (ByteString.length bs))
    Storable.pokeByteOff res (ByteString.length bs) (0 :: CChar)
    return res

wrapBsCallback :: WiltonCallbackInternal -> WiltonCallback
wrapBsCallback cb = (\_ jsonCs jsonCsLen jsonOutPtr jsonOutLenPtr -> do
    dataBs <- if 0 /= Ptr.ptrToIntPtr jsonCs && jsonCsLen > 0
        then ByteString.packCStringLen (jsonCs, (fromIntegral jsonCsLen))
        else return (UTF8.fromString "{}")
    respEither <- Exception.catch
        (cb dataBs)
        (\(e :: Exception.SomeException) -> do
            return (Left (UTF8.fromString (show e))))
    either
        (\errBs -> do
            errCs <- copyToWiltonBuffer errBs
            return errCs)
        (\respBs -> do
            respCs <- copyToWiltonBuffer respBs
            Storable.poke jsonOutPtr respCs
            Storable.poke jsonOutLenPtr (fromIntegral (ByteString.length respBs))
            return Ptr.nullPtr)
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
registerWiltonCall ::
        forall from to. (Data.Data from, Aeson.FromJSON from, Aeson.ToJSON to) =>
        String -> (from -> IO to) -> IO (Maybe ByteString)
registerWiltonCall nameString cbJson = do
    let cbBs = (\jsonBs -> either
            (\e -> return (Left (UTF8.fromString ("JSON parse error: [" ++ (show e) ++ "], required fields: " ++
                ((show . (map Data.constrFields) . Data.dataTypeConstrs . Data.dataTypeOf) (undefined::from))))))
            (\obj -> do
                resObj <- cbJson obj
                let resBs = ByteString.concat (ByteStringLazy.toChunks (Aeson.encode resObj))
                return (Right resBs) )
            (Aeson.eitherDecode (ByteStringLazy.fromChunks [jsonBs]) :: Either String from) )
    let cbCs = wrapBsCallback cbBs
    cb <- createCallbackPtr cbCs
    let nameBs = UTF8.fromString nameString
    errc <- ByteString.useAsCString nameBs (\cs ->
        wiltoncall_register cs (fromIntegral (ByteString.length nameBs)) Ptr.nullPtr cb )
    if 0 /= Ptr.ptrToIntPtr errc then do
        bs <- ByteString.packCString errc
        wilton_free errc
        return (Just bs)
    else return Nothing


-- | Create an error message, that can be passed back to Wilton
--
-- Helper function, that can be used with a @Maybe ByteString@ value returned
-- from @registerWiltonCall@ function.
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
        (return Ptr.nullPtr)
        copyToWiltonBuffer
        errBsMaybe


-- | Call JavaScript function
--
-- Allows to call a specified JavaScript function
-- passing arguments as a list of JSON values and receiving
-- result as a @ByteString@.
--
-- Arguments (JSON call descriptor fields):
--
--    * @module :: Aeson.String@: name of the RequireJS module
--    * @func :: Aeson.String@: name of the function field in the module object (optional: not needed if module itself is a function)
--    * @args :: Aeson.Vector@: function arguments (optional)
--
-- Return value: either error string or call response as a @ByteString@
--
-- Example:
--
-- > -- prepare call descriptor (Aeson.Value)
-- > let callDesc = Aeson.Object (Map.fromList [
-- >         ("module", "lodash/string"),
-- >         ("func", "capitalize"),
-- >         ("args", Aeson.Array (Vector.fromList [Aeson.String msg]))
-- >         ])
-- >
-- > -- perform a call and check the results
-- > respEither <- runWiltonScript callDesc
-- > either (\err -> ...) (\respBs -> ...) respEither
--
runWiltonScript :: Aeson.Value -> IO (Either String ByteString)
runWiltonScript callDesc = do
    let callDescBs = ByteString.concat (ByteStringLazy.toChunks (Aeson.encode callDesc))
    ByteString.useAsCString callDescBs (\cs ->
        Alloc.alloca (\outPtr -> do
            Alloc.alloca (\outLenPtr -> do
                let len = fromIntegral (ByteString.length callDescBs)
                errc <- wiltoncall_runscript cs (fromIntegral 0) cs len outPtr outLenPtr
                out <- Storable.peek outPtr
                outLen <- Storable.peek outLenPtr
                res <- if 0 /= Ptr.ptrToIntPtr errc then do
                    bs <- ByteString.packCString errc
                    wilton_free errc
                    return (Left (UTF8.toString bs))
                else do
                    if 0 /= Ptr.ptrToIntPtr out && outLen > 0 then do
                       outBs <- ByteString.packCStringLen (out, (fromIntegral outLen))
                       return (Right outBs)
                    else do
                       return (Right (UTF8.fromString ""))
                if 0 /= Ptr.ptrToIntPtr out then do
                    _ <- wilton_free out
                    return res
                else return res )))

-- $use
--
-- Add @aeson@ and @wilton-ffi@ deps to @package.yaml@:
--
-- > dependencies:
-- >    - ...
-- >    - aeson
-- >    - utf8-string
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
-- >     _ <- registerWiltonCall "hello" hello
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
