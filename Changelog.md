# 1.4.2

* Updated `base` upper bound

# 1.4.1

* Updated `base` upper bound

# 1.4.0

* Added the ability to read the journal backwards as well as forwards. Thanks to
  @defanor for this change.

# 1.3.4

* Added the `journalEntryRealtime` property to `JournalEntry`s. This is backed by a call to
  `sd_journal_get_realtime_usec`. Thanks to @rickynils for this change.
* Build with `vector` < 0.12.

# 1.3.3

* Added `journalField :: JournalField -> Text`, to view the name of a `JournalField` as `Text`.
  Thanks to @rickynils.
* Addressed a deprecation warning from `bytestring`.

# 1.3.2

* Increase the upper-bound of `base`.

# 1.3.1

* Increase the upper-bound of `text` to < 1.3.

# 1.3.0

* Now depends on `systemd >= 209`. These versions of `systemd` feature the
  `journald` functions in the `systemd` library.

# 1.2.0

* Builds with base 4.7
* `openJournal` now takes an optional threshold parameter. Thanks Shea Levy
  (@shlevy).

# 1.1.0

* It is now possible to read the journal.
* Additionally, a lot of types/functions have changed their name from talking
  about logging to talking about sending messages to the journal.

# 1.0.0

* Initial version, supporting structured logging to the journal.
