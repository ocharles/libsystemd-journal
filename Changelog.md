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
