# haskell-api-helpers-ghcs

Some api helpers. Modeled after haskell-api-helpers so that I can use the same interface in ghcjs.


## Misc

```
:set -XOverloadedStrings
let opts = ApiOptions "https://localhost" "api" True
runWith (getAt ([("key","value")] :: [(String,String)]) []) opts
```
