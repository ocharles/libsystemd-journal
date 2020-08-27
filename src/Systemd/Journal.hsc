#include <systemd/sd-journal.h>

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
    , journalField

      -- * Reading the journal
    , openJournal
    , Start(..)
    , Direction(..)
    , JournalEntry, JournalEntryCursor
    , journalEntryFields, journalEntryCursor, journalEntryRealtime
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
import Data.Semigroup (Semigroup)
import Data.String (IsString (..))
import Data.Typeable (Typeable)
import Data.Word
import Foreign (Ptr, alloca, free, peek, throwIfNeg)
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
import qualified Pipes.Safe as Pipes
import qualified System.Posix.Syslog as Syslog
import qualified System.Posix.Types.Iovec as Iovec

--------------------------------------------------------------------------------
foreign import ccall "sd_journal_sendv"
  sdJournalSendV :: Ptr Iovec.CIovec -> Int -> IO Int

--------------------------------------------------------------------------------
newtype JournalField = JournalField Text.Text
  deriving (Eq, Data, Hashable, Ord, Read, Show, Typeable, Monoid, Semigroup)

instance IsString JournalField where
  fromString = JournalField . Text.pack . map toUpper

--------------------------------------------------------------------------------
-- | Construct a 'JournalField' by converting to uppercase, as required by the
-- journal.
mkJournalField :: Text.Text -> JournalField
mkJournalField = JournalField . Text.toUpper

--------------------------------------------------------------------------------
-- | Extract the name of a 'JournalField'.
journalField :: JournalField -> Text.Text
journalField (JournalField f) = f

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

foreign import ccall "sd_journal_previous"
 sdJournalPrevious :: Ptr JournalEntry -> IO Int

foreign import ccall "sd_journal_add_match"
 sdJournalAddMatch :: Ptr JournalEntry -> Ptr a -> #{type size_t} -> IO Int

foreign import ccall "sd_journal_add_conjunction"
 sdJournalAddConjunction :: Ptr JournalEntry -> IO Int

foreign import ccall "sd_journal_add_disjunction"
 sdJournalAddDisjunction :: Ptr JournalEntry -> IO Int

foreign import ccall "sd_journal_close"
  sdJournalClose :: Ptr JournalEntry -> IO ()

foreign import ccall "sd_journal_get_cursor"
  sdJournalGetCursor :: Ptr JournalEntry -> Ptr CString -> IO ()

foreign import ccall "sd_journal_seek_cursor"
  sdJournalSeekCursor :: Ptr JournalEntry -> CString -> IO #{type int}

foreign import ccall "sd_journal_seek_tail"
  sdJournalSeekTail :: Ptr JournalEntry -> IO #{type int}

foreign import ccall "sd_journal_previous_skip"
  sdJournalPreviousSkip :: Ptr JournalEntry -> #{type uint64_t} -> IO #{type int}

foreign import ccall "sd_journal_wait"
  sdJournalWait :: Ptr JournalEntry -> #{type uint64_t} -> IO #{type int}

foreign import ccall "sd_journal_set_data_threshold"
  sdJournalSetDataThreshold :: Ptr JournalEntry -> #{type size_t} -> IO #{type int}

foreign import ccall "strerror" c'strerror
  :: #{type int} -> IO CString

foreign import ccall "sd_journal_get_realtime_usec"
 sdJournalGetRealtimeUsec :: Ptr JournalEntry -> Ptr #{type uint64_t} -> IO #{type int}

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
type JournalEntryCursor = BS.ByteString

--------------------------------------------------------------------------------
-- | An entry that has been read from the systemd journal.
data JournalEntry = JournalEntry
  { journalEntryFields :: JournalFields
    -- ^ A map of each 'JournalField' to its value.

  , journalEntryCursor :: JournalEntryCursor
  -- ^ A 'JournalCursor' can be used as marker into the journal stream. This can
  -- be used to re-open the journal at a specific point in the future, and
  -- 'JournalCursor's can be serialized to disk.

  , journalEntryRealtime :: Word64
  -- ^ The time (in microseconds since the epoch) when this journal entry was
  -- received by the systemd journal.
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
-- | In which direction to read the journal.
data Direction
  = Forwards
  -- ^ Read towards the end.
  | Backwards
  -- ^ Read towards the beginning.
  deriving (Eq)

--------------------------------------------------------------------------------
-- | Where to begin reading the journal from.
data Start
  = FromStart
  -- ^ Begin reading from the start of the journal.
  | FromEnd Direction
  -- ^ Begin reading from the end of the journal.
  | FromCursor JournalEntryCursor Direction
  -- ^ From a 'JournalEntryCursor'.

--------------------------------------------------------------------------------
-- | Opens the journal for reading, optionally filtering the journal entries.
-- Filters are defined as arbitrary binary expression trees, which are then
-- rewritten to be in conjunctive normal form before filtering with systemd
-- to comply with systemd's rule system.
openJournal
  :: Pipes.MonadSafe m
  => [JournalFlag]
  -- ^ A list of flags taken under logical disjunction (or) to specify which
  -- journal files to open.
  -> Start
  -- ^ Where to begin reading journal entries from.
  -> Maybe Filter
  -- ^ An optional filter to apply the journal. Only entries satisfying the
  -- filter will be emitted.
  -> Maybe Integer
  -- ^ The data field size threshold, or Nothing for no field size limit
  -> Pipes.Producer' JournalEntry m ()
openJournal flags start journalFilter threshold =
  Pipes.bracket (liftIO openJournalPtr) (liftIO . sdJournalClose) go

  where
  openJournalPtr = do
    journalPtr <- alloca $ \journalPtrPtr -> do
      _ <- throwIfNeg (("sdl_journal_open returned: " ++) . show) $
             sdJournalOpen journalPtrPtr encodedJournalFlags
      peek journalPtrPtr

    for_ journalFilter $ applyFilter journalPtr

    case start of
      FromStart ->
        return ()

      FromEnd d -> void $ do
        throwIfNeg (("sd_journal_seek_tail: " ++) . show) $
          sdJournalSeekTail journalPtr
        when (d == Forwards) $ do
          throwIfNeg (("sd_journal_previous_skip" ++) . show) $
            sdJournalPreviousSkip journalPtr 1
          return ()

      FromCursor cursor _ -> void $
        BS.useAsCString cursor (sdJournalSeekCursor journalPtr)

    _ <- throwIfNeg (("sd_journal_set_data_threshold returned: " ++) . show) .
        sdJournalSetDataThreshold journalPtr $ case threshold of
                                                Nothing -> fromIntegral (0 :: Integer)
                                                Just n  -> fromIntegral n

    return journalPtr

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


  sdJournalDirection :: Direction
  sdJournalDirection = case start of
    FromStart -> Forwards
    FromEnd d -> d
    FromCursor _ d -> d

  sdJournalMove :: Ptr JournalEntry -> IO Int
  sdJournalMove = if sdJournalDirection == Forwards then sdJournalNext else sdJournalPrevious

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
                    BS.break (== (fromIntegral $ ord '=')) f
              in readFields
                   (HashMap.insert
                      (JournalField $ Text.decodeUtf8 fieldName)
                      (BS.tail fieldValue)
                      acc)

            Nothing -> return acc

    progressedBy <- liftIO (sdJournalMove journalPtr)

    case compare progressedBy 0 of
      GT -> do
        entry <- liftIO $ JournalEntry
          <$> readFields mempty
          <*> (alloca $ \cursorStrPtr -> do
                sdJournalGetCursor journalPtr cursorStrPtr
                cursorCString <- peek cursorStrPtr
                BS.packCString cursorCString <* free cursorCString)
          <*> (alloca $ \realtimePtr -> do
                sdJournalGetRealtimeUsec journalPtr realtimePtr
                peek realtimePtr)

        Pipes.yield entry

        go journalPtr

      EQ -> when (sdJournalDirection == Forwards) $ do
        liftIO $ sdJournalWait journalPtr maxBound
        go journalPtr

      LT -> error $ "sd_journal_next: " ++ show progressedBy

--------------------------------------------------------------------------------
encodeJournalFlag :: JournalFlag -> #{type int}
encodeJournalFlag LocalOnly = #{const SD_JOURNAL_LOCAL_ONLY}
encodeJournalFlag RuntimeOnly = #{const SD_JOURNAL_RUNTIME_ONLY}
encodeJournalFlag SystemOnly = #{const SD_JOURNAL_SYSTEM_ONLY}
