--
-- Copyright 2018, alex at staticlibs.net
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Foreign.Wilton.FFI (
    registerWiltoncall,
    createWiltonError
    ) where

import qualified Control.Exception as E (SomeException, catch, displayException)
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecodeStrict)
import Data.ByteString (ByteString, useAsCString, useAsCStringLen, packCString, packCStringLen)
import qualified Data.ByteString as BS (length, putStrLn)
import Data.ByteString.Lazy (toStrict)
{- Data.ByteString.UTF8 (fromString) -}
import Data.ByteString.Char8 (pack)
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
        else return (pack "{}")
    respEither <- E.catch
        (cb dataBs)
        (\(e :: E.SomeException) -> do
            return (Left (pack (E.displayException e))))
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

registerWiltoncall :: forall from to. (FromJSON from, ToJSON to) => String -> (from -> IO to) -> IO (Maybe ByteString)
registerWiltoncall nameString cbJson = do
    let cbBs = (\jsonBs -> either
            (\e -> return (Left (pack e)))
            (\obj -> do
                resObj <- cbJson obj
                let resBs = toStrict (encode resObj)
                return (Right resBs) )
            (eitherDecodeStrict jsonBs :: Either String from) )
    let cbCs = wrapBsCallback cbBs
    cb <- createCallbackPtr cbCs
    let name = pack nameString
    errc <- useAsCStringLen name (\(cs, len) ->
        wiltoncall_register cs (fromIntegral len) nullPtr cb )
    if 0 /= ptrToIntPtr errc then do
        bs <- packCString errc
        wilton_free errc
        return (Just bs)
    else return Nothing

createWiltonError :: Maybe ByteString -> IO CString
createWiltonError errBsMaybe =
    maybe
        (return nullPtr)
        copyToWiltonBuffer
        errBsMaybe
