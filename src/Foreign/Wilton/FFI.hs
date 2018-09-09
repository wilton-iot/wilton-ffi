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
      registerWiltonCall
    , invokeWiltonCall
    , invokeWiltonCallByteString
    , createWiltonError
    ) where

import Prelude
    ( Either(Left, Right), IO, Maybe(Just, Nothing)
    , (==), (/=), (>), (>=), (&&), (.), (+), (-), (++)
    , fromIntegral, return, show, undefined
    )

import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson (encode, eitherDecode)
import qualified Data.ByteString as ByteString (concat, drop, length, take)
import qualified Data.ByteString.Char8 as ByteStringChar8 (index)
import qualified Data.ByteString.Lazy as ByteStringLazy (fromChunks, toChunks)
import qualified Data.ByteString.UTF8 as UTF8 (fromString, toString)
import Data.ByteString (ByteString, packCString, packCStringLen, useAsCString)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CInt(CInt))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek, poke, pokeByteOff)

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

foreign import ccall safe "wiltoncall"
    wiltoncall :: CString -> CInt -> CString -> CInt -> Ptr (CString) -> Ptr (CInt) -> IO CString

-- function pointer wrapper

foreign import ccall "wrapper"
    createCallbackPtr :: WiltonCallback -> IO (FunPtr WiltonCallback)

-- helper functions

copyToWiltonBuffer :: ByteString -> IO CString
copyToWiltonBuffer bs = do
    res <- wilton_alloc ((bytesLength bs) + 1)
    useAsCString bs (\cs ->
        copyBytes res cs (ByteString.length bs))
    pokeByteOff res (ByteString.length bs) (0 :: CChar)
    return res

encodeJsonBytes :: ToJSON a => a -> ByteString
encodeJsonBytes = ByteString.concat . ByteStringLazy.toChunks . Aeson.encode

bytesLength :: ByteString -> CInt
bytesLength = fromIntegral . ByteString.length

unwrapJsonString :: ByteString -> ByteString
unwrapJsonString st =
    if ByteString.length st >= 2
        && ('"' == ByteStringChar8.index st 0)
        && ('"' == ByteStringChar8.index st ((ByteString.length st) - 1))
    then ByteString.take ((ByteString.length st) - 2) (ByteString.drop 1 st)
    else st

wrapBsCallback :: WiltonCallbackInternal -> WiltonCallback
wrapBsCallback cb = fun
    where
        fun _ jsonCs jsonCsLen jsonOutPtr jsonOutLenPtr = do
            dataBs <-
                if nullPtr /= jsonCs && jsonCsLen > 0
                then packCStringLen (jsonCs, (fromIntegral jsonCsLen))
                else return (UTF8.fromString "{}")
            respEither <-
                catch
                (cb dataBs)
                (\(e :: SomeException) ->
                    return (Left (UTF8.fromString (show e))))
            case respEither of
                Left errBs ->
                    copyToWiltonBuffer errBs
                Right respBs -> do
                    respCs <- copyToWiltonBuffer respBs
                    poke jsonOutPtr respCs
                    poke jsonOutLenPtr (bytesLength respBs)
                    return nullPtr

-- | Registers a function, that can be called from JavaScript
--
-- This function takes a function and registers it with Wilton, so
-- it can be called from JavaScript using [wiltoncall](https://wilton-iot.github.io/wilton/docs/html/namespacewiltoncall.html)
-- API.
--
-- Function must take a single argument - a data that implements
-- [Data.Aeson.FromJSON](https://hackage.haskell.org/package/aeson-1.3.0.0/docs/Data-Aeson.html#t:FromJSON)
-- and must return a data that implements
-- [Data.Aeson.ToJSON](https://hackage.haskell.org/package/aeson-1.3.0.0/docs/Data-Aeson.html#t:ToJSON).
-- Function input argument is converted from a JavaScript object into a Haskell data object.
-- Function output is returned to JavaScript as a JSON (that can be immediately converted to JavaScript object).
--
-- If function raises and @Exception@, its error message is converted into JavasSript `Error` message (that can be
-- caught and handled on JavaScript side).
--
-- Arguments:
--
--    * @name :: ByteString@: name for this call, that should be used from JavaScript to invoke the function
--    * @callback :: (a -> IO b)@: Function, that will be called from JavaScript
--
-- Return value: error status.
--
registerWiltonCall ::
        forall a b . (FromJSON a, ToJSON b) =>
        ByteString -> (a -> IO b) -> IO (Maybe ByteString)
registerWiltonCall nameBs cbJson = do
    let cbCs = wrapBsCallback cbBs
    cb <- createCallbackPtr cbCs
    errc <-
        useAsCString nameBs (\cs ->
            wiltoncall_register cs (bytesLength nameBs) nullPtr cb )
    if nullPtr /= errc
    then do
        bs <- packCString errc
        wilton_free errc
        return (Just bs)
    else return Nothing
    where
        cbBs jsonBs =
            case Aeson.eitherDecode (ByteStringLazy.fromChunks [jsonBs]) of
                Left e -> return (Left (UTF8.fromString ("Parse error,"
                        ++ " json: [" ++ (UTF8.toString jsonBs) ++ "],"
                        ++ " message: [" ++ e ++ "]")))
                Right (obj :: a) -> do
                    -- target callback is invoked here
                    resObj <- cbJson obj
                    let resBs = encodeJsonBytes resObj
                    return (Right resBs)

-- | Invoke a function from @WiltonCall@ registry
--
-- Allows to call a specified function, that was previously registered as a @WiltonCall@
-- passing arguments as a data that can be converted to JSON and receiving the result
-- as a data parsed from JSON.
--
-- Arguments
--
--    * @callName :: ByteString@: name of the previously registered @WiltonCall@
--    * @callData :: a@: a data that implements [Data.Aeson.ToJSON](https://hackage.haskell.org/package/aeson-1.3.0.0/docs/Data-Aeson.html#t:ToJSON)
--                               or an @Aeson.Value@ for dynamic JSON conversion
--
-- Return value: either error @ByteString@ or a call response as a data that implements
-- [Data.Aeson.FromJSON](https://hackage.haskell.org/package/aeson-1.3.0.0/docs/Data-Aeson.html#t:FromJSON)
-- or an @Aeson.Value@ for dynamic JSON conversion
--
-- Example:
--
-- > -- call data definition
-- >
-- > data FileUploadArgs = FileUploadArgs
-- >     { url :: Text
-- >     , filePath :: Text
-- >     } deriving (Generic, Show)
-- > instance ToJSON FileUploadArgs
-- >
-- > let callData = FileUploadArgs "http://127.0.0.1:8080/some/path" "path/to/file"
-- > -- perform a call (sending a specified file over HTTP) and check the results
-- > respEither <- invokeWiltonCall "httpclient_send_file" callData
-- > either (\err -> ...) (\resp -> ...) respEither
--
invokeWiltonCall ::
        forall a b . (ToJSON a, FromJSON b) =>
        ByteString -> a -> IO (Either ByteString b)
invokeWiltonCall callName callData = do
    let callDataBs = encodeJsonBytes callData
    -- this is ugly, but proper typing is cumbersome here
    let callDataPass = unwrapJsonString callDataBs
    resEither <- invokeWiltonCallByteString callName callDataPass
    case resEither of
        Left err -> return (Left err)
        Right jsonBs -> do
            let jsonBsNonEmpty = if ByteString.length jsonBs > 0
                then jsonBs
                else UTF8.fromString "[]" -- unit
            case Aeson.eitherDecode (ByteStringLazy.fromChunks [jsonBsNonEmpty]) of
                Left e ->
                    return (Left (UTF8.fromString ("Parse error,"
                        ++ " json: [" ++ (UTF8.toString jsonBsNonEmpty) ++ "],"
                        ++ " message: [" ++ e ++ "]")))
                Right (obj :: b) -> return (Right obj)

-- | Invoke a function from @WiltonCall@ registry
--
-- Allows to call a specified function, that was previously registered as a @WiltonCall@
-- passing arguments and receiving result as @ByteString@s.
--
-- Arguments
--
--    * @callName :: ByteString@: name of the previously registered @WiltonCall@
--    * @callData :: ByteString@: argument (usually JSON) that is passes to the specified @WiltonCall@
--
-- Return value: either error string or a call response as a @ByteString@
--
invokeWiltonCallByteString :: ByteString -> ByteString -> IO (Either ByteString ByteString)
invokeWiltonCallByteString callName callDataBs = do
    useAsCString callName (\nameCs ->
        useAsCString callDataBs (\callDataCs ->
            alloca (\(outPtr :: Ptr CString) -> do
                alloca (\(outLenPtr :: Ptr CInt) -> do
                    poke outPtr nullPtr
                    poke outLenPtr 0
                    errc <- wiltoncall nameCs (bytesLength callName) callDataCs (bytesLength callDataBs) outPtr outLenPtr
                    out <- peek outPtr
                    outLen <- peek outLenPtr
                    res <-
                        if nullPtr /= errc
                        then do
                            bs <- packCString errc
                            wilton_free errc
                            return (Left bs)
                        else do
                            if nullPtr /= out && outLen > 0
                            then do
                               outBs <- packCStringLen (out, (fromIntegral outLen))
                               return (Right outBs)
                            else
                               return (Right (UTF8.fromString ""))
                    if nullPtr /= out
                    then do
                        wilton_free out
                        return res
                    else return res ))))

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
    case errBsMaybe of
        Just errBs -> copyToWiltonBuffer errBs
        _ -> return nullPtr

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
-- > {-# LANGUAGE DeriveGeneric #-}
-- > {-# LANGUAGE ForeignFunctionInterface #-}
--
-- Import @aeson@, @wilton-ffi@ and other deps:
--
-- > import Data.Aeson
-- > import GHC.Generics
-- > import Foreign.C.String
-- > import Foreign.Wilton.FFI
--
-- Declare input/output structs:
--
-- > data MyIn = MyIn {} deriving (Generic, Show)
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
