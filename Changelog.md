# 1.4.6.0

* Support GHC 9.10

# 1.4.5.1

* Miscellaneous cleanup
* Adjusted a number of dependency bounds
* Supported GHC range is now 9.2 to 9.6

Published by: Chris Martin

Date: 2023-08-16

# 1.4.5

* Updated `base` upper bound to be compatible with GHC 8.8 and 8.10 (#21)
* Fixed a warning (#20)

Published by: Oliver Charles

Date: 2020-09-24

# 1.4.4

* Updated `base` upper bound and compatibility with GHC 8.6
* Updated `semigroup` upper bound.

Published by: Oliver Charles

Date: 2019-05-15

# 1.4.3

* Updated `base` upper bound and compatibility with GHC 8.4

Published by: Oliver Charles

Date: 2018-05-01

# 1.4.2

* Updated `base` upper bound

Published by: Oliver Charles

Date: 2017-07-24

# 1.4.1

* Updated `base` upper bound

Published by: Oliver Charles

Date: 2017-01-09

# 1.4.0

* Added the ability to read the journal backwards as well as forwards. Thanks to
  @defanor for this change.

Published by: Oliver Charles

Date: 2015-09-15

# 1.3.4

* Added the `journalEntryRealtime` property to `JournalEntry`s. This is backed by a call to
  `sd_journal_get_realtime_usec`. Thanks to @rickynils for this change.
* Build with `vector` < 0.12.

Published by: Oliver Charles

Date: 2015-09-10

# 1.3.3

* Added `journalField :: JournalField -> Text`, to view the name of a `JournalField` as `Text`.
  Thanks to @rickynils.
* Addressed a deprecation warning from `bytestring`.

Published by: Oliver Charles

Date: 2015-07-19

# 1.3.2

* Increase the upper-bound of `base`.

Published by: Oliver Charles

# 1.3.1

* Increase the upper-bound of `text` to < 1.3.

Published by: Oliver Charles

Date: 2015-01-15

# 1.3.0

* Now depends on `systemd >= 209`. These versions of `systemd` feature the
  `journald` functions in the `systemd` library.

Published by: Oliver Charles

Date: 2014-12-31

# 1.2.0

* Builds with base 4.7
* `openJournal` now takes an optional threshold parameter. Thanks Shea Levy
  (@shlevy).

Published by: Oliver Charles

Date: 2014-05-08

# 1.1.0

* It is now possible to read the journal.
* Additionally, a lot of types/functions have changed their name from talking
  about logging to talking about sending messages to the journal.

Published by: Oliver Charles

Date: 2014-02-08

# 1.0.0

* Initial version, supporting structured logging to the journal.

Published by: Oliver Charles

Date: 2014-02-05
