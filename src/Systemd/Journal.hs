{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Systemd.Journal
    ( -- * Logging
      logMessage
    , logMessageWith
    , logProperties

      -- * Log Properties
    , LogProperties

      -- ** Standard systemd log properties
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

      -- ** Custom Properties
    , LogKey
    , mkLogKey
    ) where

import Control.Monad (void)
import Data.Char (toUpper)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Monoid (Monoid, mappend)
import Data.String (IsString (..))
import Data.Typeable (Typeable)
import Foreign (Ptr, throwIfNeg)
import System.Posix.Types (CPid(..))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import qualified Data.Vector.Storable as V
import qualified System.Posix.Syslog as Syslog
import qualified System.Posix.Types.Iovec as Iovec

--------------------------------------------------------------------------------
foreign import ccall "sd_journal_sendv"
  sdJournalSendV :: Ptr Iovec.CIovec -> Int -> IO Int

--------------------------------------------------------------------------------
newtype LogKey = LogKey Text.Text
  deriving (Eq, Data, Hashable, Ord, Read, Show, Typeable, Monoid)

instance IsString LogKey where
  fromString = LogKey . Text.pack . map toUpper

--------------------------------------------------------------------------------
-- | Construct a 'LogKey' by converting to uppercase, as required by the
-- journal.
mkLogKey :: Text.Text -> LogKey
mkLogKey = LogKey . Text.toUpper

--------------------------------------------------------------------------------
-- | A structured object of properties to log. You generally don't construct
-- this yourself, but you use the monoid instance and smart constructors below.
-- For example,
--
-- > logProperties (message "Oh god, it burns!" <> priority Emergency)
type LogProperties = HashMap.HashMap LogKey Text.Text

--------------------------------------------------------------------------------
-- | The human readable message string for this entry. This is supposed to be
-- the primary text shown to the user. It is usually not translated (but might be
-- in some cases), and is not supposed to be parsed for meta data.
message :: Text.Text -> LogProperties
message = HashMap.singleton (LogKey "MESSAGE")

--------------------------------------------------------------------------------
-- | A 128bit message identifier ID for recognizing certain message types, if
-- this is desirable. Developers can generate a new ID for this purpose with
-- @journalctl --new-id@.
messageId :: UUID.UUID -> LogProperties
messageId =
  HashMap.singleton (LogKey "MESSAGE_ID") . Text.pack . UUID.toString

--------------------------------------------------------------------------------
-- | A priority value compatible with syslog's priority concept.
priority :: Syslog.Priority -> LogProperties
priority =
  HashMap.singleton (LogKey "PRIORITY") . Text.pack . show . fromEnum

--------------------------------------------------------------------------------
-- | The source code file generating this message.
codeFile :: FilePath -> LogProperties
codeFile =
  HashMap.singleton (LogKey "CODE_FILE") . Text.pack

--------------------------------------------------------------------------------
-- | The source code line number generating this message.
codeLine :: Int -> LogProperties
codeLine = HashMap.singleton (LogKey "CODE_LINE") . Text.pack . show

--------------------------------------------------------------------------------
-- | The source code function name generating this message.
codeFunc :: Text.Text -> LogProperties
codeFunc = HashMap.singleton (LogKey "CODE_FUNC")

--------------------------------------------------------------------------------
-- | The low-level Unix error number causing this entry, if any. Contains the
-- numeric value of @errno(3)@.
errno :: Int -> LogProperties
errno = HashMap.singleton (LogKey "ERRNO") . Text.pack . show

--------------------------------------------------------------------------------
-- | Syslog compatibility field.
syslogFacility :: Syslog.Facility -> LogProperties
syslogFacility =
  HashMap.singleton (LogKey "SYSLOG_FACILITY") . Text.pack . show . fromEnum

--------------------------------------------------------------------------------
-- | Syslog compatibility field.
syslogIdentifier :: Text.Text -> LogProperties
syslogIdentifier =
  HashMap.singleton (LogKey "SYSLOG_IDENTIFIER")

--------------------------------------------------------------------------------
-- | Syslog compatibility field.
syslogPid :: CPid -> LogProperties
syslogPid (CPid pid) =
  HashMap.singleton (LogKey "SYSLOG_PID") (Text.pack $ show pid)

--------------------------------------------------------------------------------
-- | Log a message to the systemd journal.
logMessage :: Text.Text -> IO ()
logMessage = logProperties . message

--------------------------------------------------------------------------------
-- | Log a message and supply extra metadata. All metadata keys will be
-- converted to uppercase for you.
--
-- Note: The 'Message' meta-data property will be replaced with the first
-- parameter to this function. If you don't want this, use 'logMap'
logMessageWith :: Text.Text -> LogProperties -> IO ()
logMessageWith text meta = logProperties $ mappend meta $ message text

--------------------------------------------------------------------------------
logProperties :: LogProperties -> IO ()
logProperties meta = void $
  throwIfNeg (("sd_journal_send returned :" ++) . show) $
  go id 0 (HashMap.toList meta)

  where
  go f n [] = V.unsafeWith (V.fromList (f [])) $ \iovecs ->
    sdJournalSendV iovecs n

  go f n ((LogKey k, v) : xs) =
    Iovec.unsafeUseAsCIovec (Text.encodeUtf8 (Text.intercalate "=" [k, v])) $
      \messageIovec -> go (f . (++ [messageIovec])) (n + 1) xs
