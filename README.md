scythe
========================

Fast, [RFC-4180](https://tools.ietf.org/html/rfc4180#ref-4) compliant, CSV lexing.

### Example
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.CSV
import Data.ByteString.Lazy

-- | test.csv
testCSV :: ByteString
testCSV = "1,2,3,4,5\n1,2,3,4,5\n"

main :: IO ()
main = mapM_ print (getCSV testCSV)
```

### Result
```haskell
Item 1
Item 2
Item 3
Item 4
Item 5
Newline
Item 1
Item 2
Item 3
Item 4
Item 5
Newline
```


### Build
```shell
nix-build
```

### Develop
```haskell
git clone https://github.com/dmjio/scythe
cd scythe
nix-shell
cabal (new-)build
```

