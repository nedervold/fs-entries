# fs-entries

Datatypes for filesystem-like hierarchical data.

This package is similar to and inspired by
[directory-tree](http://hackage.haskell.org/package/directory-tree).
It differs that in `directory-tree`, the central data structure
(`DirTree`) represents a directory, while in `fs-entries` the central
datatype (`FSEntries`) represents the *contents* of a directory.

You will never *not* want to have a root directory.  Leaving the root
directory (`/` or `.`) anonymous and implicit makes the design of the
algorithms cleaner.
