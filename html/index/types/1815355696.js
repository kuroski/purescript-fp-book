// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1815355696"] = [{"values":[{"sourceSpan":{"start":[326,1],"name":".spago/ordered-collections/v2.0.1/src/Data/Map/Internal.purs","end":[326,101]},"score":1,"packageInfo":{"values":["ordered-collections"],"tag":"Package"},"name":"foldSubmap","moduleName":"Data.Map.Internal","info":{"values":[{"type":{"tag":"ForAll","contents":["k",{"tag":"ForAll","contents":["v",{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"k"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Monoid"],"Monoid"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"k"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"k"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"k"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"v"}]},{"tag":"TypeVar","contents":"m"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Map","Internal"],"Map"]},{"tag":"TypeVar","contents":"k"}]},{"tag":"TypeVar","contents":"v"}]}]},{"tag":"TypeVar","contents":"m"}]}]}]}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Fold over the entries of a given map where the key is between a lower and\nan upper bound. Passing `Nothing` as either the lower or upper bound\nargument means that the fold has no lower or upper bound, i.e. the fold\nstarts from (or ends with) the smallest (or largest) key in the map.\n\n```purescript\nfoldSubmap (Just 1) (Just 2) (\\_ v -> [v])\n (fromFoldable [Tuple 0 \"zero\", Tuple 1 \"one\", Tuple 2 \"two\", Tuple 3 \"three\"])\n == [\"one\", \"two\"]\n\nfoldSubmap Nothing (Just 2) (\\_ v -> [v])\n (fromFoldable [Tuple 0 \"zero\", Tuple 1 \"one\", Tuple 2 \"two\", Tuple 3 \"three\"])\n == [\"zero\", \"one\", \"two\"]\n```\n"}],"tag":"SearchResult"}]