Haskell library for flexible RAII-style resource management.

This package provides a superset of the functionality supported by
[resourcet][1]; in addition to allowing for early early release,
it also provides move semantics: lifetimes are first class values
and hierarchical, and resources can be moved between them after
allocation.

[1]: https://hackage.haskell.org/package/resourcet
