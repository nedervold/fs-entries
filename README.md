# fs-entries

Datatypes for filesystem-like hierarchical data.

This package is similar to and inspired by
[directory-tree](http://hackage.haskell.org/package/directory-tree),
except that in `directory-tree`, the main data structure (`DirTree`)
represents a directory, while in `fs-entries` the main datatype
(`FSEntries`) represents the *contents* of a directory.  Leaving the
root directory implicit makes the design of some algorithms cleaner.
