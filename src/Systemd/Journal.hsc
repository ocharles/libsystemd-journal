#include <systemd/sd-journal.h>

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Systemd.Journal
    ( -- * Writing to the journal
      sendMessage
    , sendMessageWith
    , sendJournalFields

    , JournalFields

      -- ** Standard systemd journal fields
    , message
    , messageId
    , priority
    , Syslog.Priority(..)
    , codeFile
    , codeLine
    , codeFunc
    , errno
    , syslogFacility
    , syslogIdentifier
    , syslogPid

      -- ** Custom journal fields
    , JournalField
    , mkJournalField

      -- * Reading the journal
    , openJournal
    , JournalEntry
    , journalEntryFields
    , JournalFlag (..)
    , Filter (..)
    ) where

import Control.Applicative
import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.|.))
import Data.Char (ord, toUpper)
import Data.Data (Data)
import Data.Foldable (for_)
import Data.Hashable (Hashable)
import Data.Int
import Data.List (foldl')
import Data.Monoid (Monoid, mappend, mempty)
import Data.String (IsString (..))
import Data.Typeable (Typeable)
import Data.Word
import Foreign (Ptr, alloca, peek, throwIfNeg)
import Foreign.C (CString, peekCString)
import System.Posix.Types (CPid(..))

import Data.Generics.Uniplate.Data ()

import qualified Data.ByteString as BS
import qualified Data.Generics.Uniplate.Operations as Uniplate
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import qualified Data.Vector.Storable as V
import qualified Pipes as Pipes
import qualified System.Posix.Syslog as Syslog
import qualified System.Posix.Types.Iovec as Iovec

--------------------------------------------------------------------------------
foreign import ccall "sd_journal_sendv"
  sdJournalSendV :: Ptr Iovec.CIovec -> Int -> IO Int

--------------------------------------------------------------------------------
newtype JournalField = JournalField Text.Text
  deriving (Eq, Data, Hashable, Ord, Read, Show, Typeable, Monoid)

instance IsString JournalField where
  fromString = JournalField . Text.pack . map toUpper

--------------------------------------------------------------------------------
-- | Construct a 'JournalField' by converting to uppercase, as required by the
-- journal.
mkJournalField :: Text.Text -> JournalField
mkJournalField = JournalField . Text.toUpper

--------------------------------------------------------------------------------
-- | A structured object of all the fields in an entry in the journal. You
-- generally don't construct this yourself, but you use the monoid instance and
-- smart constructors below.
--
-- For example,
--
-- > sendJournalFields (message "Oh god, it burns!" <> priority Emergency)
type JournalFields = HashMap.HashMap JournalField BS.ByteString

--------------------------------------------------------------------------------
-- | The human readable message string for this entry. This is supposed to be
-- the primary text shown to the user. It is usually not translated (but might be
-- in some cases), and is not supposed to be parsed for meta data.
message :: Text.Text -> JournalFields
message = HashMap.singleton (JournalField "MESSAGE") . Text.encodeUtf8

--------------------------------------------------------------------------------
-- | A 128bit message identifier ID for recognizing certain message types, if
-- this is desirable. Developers can generate a new ID for this purpose with
-- @journalctl --new-id@.
messageId :: UUID.UUID -> JournalFields
messageId =
  HashMap.singleton (JournalField "MESSAGE_ID") . Text.encodeUtf8 . Text.pack . UUID.toString

--------------------------------------------------------------------------------
-- | A priority value compatible with syslog's priority concept.
priority :: Syslog.Priority -> JournalFields
priority =
  HashMap.singleton (JournalField "PRIORITY") . Text.encodeUtf8 .  Text.pack . show . fromEnum

--------------------------------------------------------------------------------
-- | The source code file generating this message.
codeFile :: FilePath -> JournalFields
codeFile =
  HashMap.singleton (JournalField "CODE_FILE") . Text.encodeUtf8 .  Text.pack

--------------------------------------------------------------------------------
-- | The source code line number generating this message.
codeLine :: Int -> JournalFields
codeLine = HashMap.singleton (JournalField "CODE_LINE") . Text.encodeUtf8 .  Text.pack . show

--------------------------------------------------------------------------------
-- | The source code function name generating this message.
codeFunc :: Text.Text -> JournalFields
codeFunc = HashMap.singleton (JournalField "CODE_FUNC") . Text.encodeUtf8

--------------------------------------------------------------------------------
-- | The low-level Unix error number causing this entry, if any. Contains the
-- numeric value of @errno(3)@.
errno :: Int -> JournalFields
errno = HashMap.singleton (JournalField "ERRNO") . Text.encodeUtf8 .  Text.pack . show

--------------------------------------------------------------------------------
-- | Syslog compatibility field.
syslogFacility :: Syslog.Facility -> JournalFields
syslogFacility =
  HashMap.singleton (JournalField "SYSLOG_FACILITY") . Text.encodeUtf8 .  Text.pack . show . fromEnum

--------------------------------------------------------------------------------
-- | Syslog compatibility field.
syslogIdentifier :: Text.Text -> JournalFields
syslogIdentifier =
  HashMap.singleton (JournalField "SYSLOG_IDENTIFIER") . Text.encodeUtf8

--------------------------------------------------------------------------------
-- | Syslog compatibility field.
syslogPid :: CPid -> JournalFields
syslogPid (CPid pid) =
  HashMap.singleton (JournalField "SYSLOG_PID") (Text.encodeUtf8 $ Text.pack $ show pid)

--------------------------------------------------------------------------------
-- | Send a message to the systemd journal.
--
-- > sendMessage t == sendJournalFields (message t)
sendMessage :: Text.Text -> IO ()
sendMessage = sendJournalFields . message

--------------------------------------------------------------------------------
-- | Send a message and supply extra fields.
--
-- Note: The @MESSAGE@ field  will be replaced with the first parameter to this
-- function. If you don't want this, use 'sendJournalFields'
sendMessageWith :: Text.Text -> JournalFields -> IO ()
sendMessageWith text meta = sendJournalFields $ mappend meta $ message text

--------------------------------------------------------------------------------
-- | Send an exact set of fields to the systemd journal.
sendJournalFields :: JournalFields -> IO ()
sendJournalFields meta = void $
  throwIfNeg (("sd_journal_send returned :" ++) . show) $
  go id 0 (HashMap.toList meta)

  where
  go f n [] = V.unsafeWith (V.fromList (f [])) $ \iovecs ->
    sdJournalSendV iovecs n

  go f n ((k, v) : xs) =
    Iovec.unsafeUseAsCIovec (encodeKv k v) $
      \messageIovec -> go (f . (++ [messageIovec])) (n + 1) xs

--------------------------------------------------------------------------------
encodeKv :: JournalField -> BS.ByteString -> BS.ByteString
encodeKv (JournalField k) v =
  Text.encodeUtf8 k `mappend` BS.singleton (fromIntegral $ ord '=') `mappend` v

--------------------------------------------------------------------------------
foreign import ccall "sd_journal_open"
 sdJournalOpen :: Ptr (Ptr JournalEntry) -> #{type int} -> IO Int

foreign import ccall "sd_journal_enumerate_data"
 sdJournalEnumerateData :: Ptr JournalEntry -> Ptr CString -> Ptr #{type size_t} -> IO #{type int}

foreign import ccall "sd_journal_next"
 sdJournalNext :: Ptr JournalEntry -> IO Int

foreign import ccall "sd_journal_add_match"
 sdJournalAddMatch :: Ptr JournalEntry -> Ptr a -> #{type size_t} -> IO Int

foreign import ccall "sd_journal_add_conjunction"
 sdJournalAddConjunction :: Ptr JournalEntry -> IO Int

foreign import ccall "sd_journal_add_disjunction"
 sdJournalAddDisjunction :: Ptr JournalEntry -> IO Int

foreign import ccall "strerror" c'strerror
  :: #{type int} -> IO CString

--------------------------------------------------------------------------------
-- | Flags to specify which journal entries to read.
data JournalFlag
  = LocalOnly
  -- ^ Only journal files generated on the local machine will be opened.
  | RuntimeOnly
  -- ^ Only volatile journal files will be opened, excluding those which are
  -- stored on persistent storage.
  | SystemOnly
  -- ^ Only journal files of system services and the kernel (in opposition to
  -- user session processes) will be opened.
  deriving (Bounded, Enum, Eq, Ord)

--------------------------------------------------------------------------------
-- | An entry that has been read from the systemd journal.
data JournalEntry
  = JournalEntry { journalEntryFields :: JournalFields
                   -- ^ A map of each 'JournalField' to its value.
                 }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | A logical expression to filter journal entries when reading the journal.
data Filter
  = Match JournalField BS.ByteString
  -- ^ A binary exact match on a given 'JournalField'.
  | And Filter Filter
  -- ^ Logical conjunction of two filters. Will only show journal entries that
  -- satisfy both conditions.
  | Or Filter Filter
  -- ^ Logical disjunction of two filters. Will show journal entries that
  -- satisfy either condition.
  deriving (Data, Eq, Show, Typeable)

--------------------------------------------------------------------------------
-- | Opens the journal for reading, optionally filtering the journal entries.
-- Filters are defined as arbitrary binary expression trees, which are then
-- rewritten to be in conjunctive normal form before filtering with systemd
-- to comply with systemd's rule system.
openJournal
  :: [JournalFlag]
  -- ^ A list of flags taken under logical disjunction (or) to specify which
  -- journal files to open.
  -> (Maybe Filter)
  -- ^ An optional filter to apply the journal. Only entries satisfying the
  -- filter will be emitted.
  -> Pipes.Producer JournalEntry IO ()
openJournal flags journalFilter = do
  journalPtr <- liftIO $ alloca $ \journalPtrPtr -> do
    _ <- throwIfNeg (("sdl_journal_open returned: " ++) . show) $
           sdJournalOpen journalPtrPtr encodedJournalFlags

    peek journalPtrPtr

  liftIO $ for_ journalFilter $ applyFilter journalPtr

  go journalPtr

  where
  encodedJournalFlags = foldl' (.|.) 0 (map encodeJournalFlag flags)

  applyFilter journalPtr =
    let cnf (Or a (And b c)) = And (Or a b) (Or a c)
        cnf (Or (And a b) c) = And (Or a c) (Or b c)
        cnf x = x

        addRule (And l r) = addRule l >> sdJournalAddConjunction journalPtr >> addRule r
        addRule (Or l r) = addRule l >> sdJournalAddDisjunction journalPtr >> addRule r
        addRule (Match k v) = BS.useAsCStringLen (encodeKv k v) $ \(ptr, len) ->
          sdJournalAddMatch journalPtr ptr (fromIntegral len)

    in addRule . Uniplate.transform cnf

  go journalPtr = do
    let readField =
          alloca $ \dataPtrPtr ->
          alloca $ \lengthPtr -> do
            ret <- sdJournalEnumerateData journalPtr dataPtrPtr lengthPtr
            if ret == 0
              then return Nothing
              else if ret < 0
                      then c'strerror (negate ret) >>= peekCString
                             >>= error . ("sd_journal_enumerate_data: " ++)
                      else do dataPtr <- peek dataPtrPtr
                              dataLength <- peek lengthPtr
                              Just <$> BS.packCStringLen (dataPtr, fromIntegral $ dataLength)

        readFields acc = do
          field <- readField
          case field of
            Just f ->
              let (fieldName, fieldValue) =
                    BS.breakByte (fromIntegral $ ord '=') f
              in readFields
                   (HashMap.insert
                      (JournalField $ Text.decodeUtf8 fieldName)
                      (BS.tail fieldValue)
                      acc)

            Nothing -> return acc

    progressedBy <- liftIO (sdJournalNext journalPtr)
    when (progressedBy > 0) $ do
      Pipes.yield =<< (liftIO $ JournalEntry <$> readFields mempty)
      go journalPtr

--------------------------------------------------------------------------------
encodeJournalFlag :: JournalFlag -> #{type int}
encodeJournalFlag LocalOnly = #{const SD_JOURNAL_LOCAL_ONLY}
encodeJournalFlag RuntimeOnly = #{const SD_JOURNAL_RUNTIME_ONLY}
encodeJournalFlag SystemOnly = #{const SD_JOURNAL_SYSTEM_ONLY}
