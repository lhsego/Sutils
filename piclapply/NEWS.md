Version 1.1.3, 2017-10-19
-----------------------------------------------------------------------------------

FIXES

- `piclapply()` was failing if launched from a compute node due the host verification mechanism in `verifyHost()`. This has now been fixed (and replaced) with `verifySystem()` where the package checks agains the environmental variable $SYSTEM_NAME to determine that a PIC system is being used.  Likewise, `showAvailableHosts()` has been replaced by `showAvailableSystems()`.


Version 1.1.2, 2017-08-05
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Improvements in documentation in `README.md`


Version 1.1.1, 2017-05-05
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Improved one of the comments in the R code sent to each R instance


Version 1.1.0, 2017-04-29
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Removal of 'olympus' from the available systems, as it was recently decomissioned.


Version 1.0.0, 2017-04-15
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- First 'official version' of `piclapply()`.

DESCRIPTION

- The `piclapply()` function originally began in the `pnlStat` (now `Smisc`) package.  Then it went on to live in the `picRutils` package.  Most recently, it now has its own dedicated package.


