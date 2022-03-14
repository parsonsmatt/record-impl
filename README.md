# record-impl

This package uses `RecordDotSyntax` to implement *methods* on datatypes, stealing the `impl` keyword from Rust.
It's pretty immature, though in theory it could support pretty powerful stuff!

## Example:

```haskell
data User = User
    { name :: String
    , birthYear :: Int
    }


```
