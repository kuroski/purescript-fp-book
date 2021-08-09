// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["115909502"] = [{"values":[{"sourceSpan":{"start":[31,1],"name":".spago/pipes/v7.0.1/src/Pipes/ListT.purs","end":[31,63]},"score":1,"packageInfo":{"values":["pipes"],"tag":"Package"},"name":"runListTRec","moduleName":"Pipes.ListT","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Rec","Class"],"MonadRec"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Pipes","ListT"],"ListT"]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[28,1],"name":".spago/pipes/v7.0.1/src/Pipes/ListT.purs","end":[28,57]},"score":1,"packageInfo":{"values":["pipes"],"tag":"Package"},"name":"runListT","moduleName":"Pipes.ListT","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Pipes","ListT"],"ListT"]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[101,1],"name":".spago/pipes/v7.0.1/src/Pipes/Core.purs","end":[101,62]},"score":1,"packageInfo":{"values":["pipes"],"tag":"Package"},"name":"runEffectRec","moduleName":"Pipes.Core","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["r",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Rec","Class"],"MonadRec"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Pipes","Core"],"Effect"]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"r"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"r"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[92,1],"name":".spago/pipes/v7.0.1/src/Pipes/Core.purs","end":[92,54]},"score":1,"packageInfo":{"values":["pipes"],"tag":"Package"},"name":"runEffect","moduleName":"Pipes.Core","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["r",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Pipes","Core"],"Effect"]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"r"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"r"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[67,1],"name":".spago/free/v6.0.1/src/Data/Yoneda.purs","end":[67,45]},"score":2,"packageInfo":{"values":["free"],"tag":"Package"},"name":"lowerYoneda","moduleName":"Data.Yoneda","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Yoneda"],"Yoneda"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Lower a value of type `Yoneda f a` to the type constructor `f`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[120,1],"name":".spago/nonempty/v6.0.0/src/Data/NonEmpty.purs","end":[120,40]},"score":5,"packageInfo":{"values":["nonempty"],"tag":"Package"},"name":"tail","moduleName":"Data.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","NonEmpty"],"NonEmpty"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Get everything but the 'first' element of a non-empty container.\n\n```purescript\ntail (1 :| [2, 3]) == [2, 3]\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[104,1],"name":".spago/nonempty/v6.0.0/src/Data/NonEmpty.purs","end":[104,58]},"score":5,"packageInfo":{"values":["nonempty"],"tag":"Package"},"name":"oneOf","moduleName":"Data.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Alternative"],"Alternative"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","NonEmpty"],"NonEmpty"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the `alt` (`<|>`) result of:\n- The first element lifted to the container of the remaining elements.\n- The remaining elements.\n\n```purescript\nimport Data.Maybe(Maybe(..))\n\noneOf (1 :| Nothing) == Just 1\noneOf (1 :| Just 2) == Just 1\n\noneOf (1 :| [2, 3]) == [1,2,3]\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[24,1],"name":".spago/ordered-collections/v2.0.1/src/Data/Map.purs","end":[24,37]},"score":1,"packageInfo":{"values":["ordered-collections"],"tag":"Package"},"name":"keys","moduleName":"Data.Map","info":{"values":[{"type":{"tag":"ForAll","contents":["k",{"tag":"ForAll","contents":["v",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Map","Internal"],"Map"]},{"tag":"TypeVar","contents":"k"}]},{"tag":"TypeVar","contents":"v"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Set"],"Set"]},{"tag":"TypeVar","contents":"k"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The set of keys of the given map.\nSee also `Data.Set.fromMap`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[626,1],"name":".spago/ordered-collections/v2.0.1/src/Data/Map/Internal.purs","end":[626,40]},"score":1,"packageInfo":{"values":["ordered-collections"],"tag":"Package"},"name":"values","moduleName":"Data.Map.Internal","info":{"values":[{"type":{"tag":"ForAll","contents":["k",{"tag":"ForAll","contents":["v",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Map","Internal"],"Map"]},{"tag":"TypeVar","contents":"k"}]},{"tag":"TypeVar","contents":"v"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"v"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Get a list of the values contained in a map\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[620,1],"name":".spago/ordered-collections/v2.0.1/src/Data/Map/Internal.purs","end":[620,38]},"score":1,"packageInfo":{"values":["ordered-collections"],"tag":"Package"},"name":"keys","moduleName":"Data.Map.Internal","info":{"values":[{"type":{"tag":"ForAll","contents":["k",{"tag":"ForAll","contents":["v",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Map","Internal"],"Map"]},{"tag":"TypeVar","contents":"k"}]},{"tag":"TypeVar","contents":"v"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"k"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Get a list of the keys contained in a map\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[283,1],"name":".spago/either/v5.0.0/src/Data/Either.purs","end":[283,42]},"score":15,"packageInfo":{"values":["either"],"tag":"Package"},"name":"hush","moduleName":"Data.Either","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["b",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"b"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Turns an `Either` into a `Maybe`, by throwing eventual `Left` values away and converting\nthem into `Nothing`. `Right` values get turned into `Just`s.\n\n```purescript\nhush (Left \"ParseError\") = Nothing\nhush (Right 42) = Just 42\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[80,1],"name":".spago/transformers/v5.1.0/src/Control/Monad/List/Trans.purs","end":[80,64]},"score":8,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"runListTRec","moduleName":"Control.Monad.List.Trans","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Rec","Class"],"MonadRec"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","List","Trans"],"ListT"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Drain a `ListT`, running it to completion and discarding all values.\nStack safe: Uses tail call optimization.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[75,1],"name":".spago/transformers/v5.1.0/src/Control/Monad/List/Trans.purs","end":[75,55]},"score":8,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"runListT","moduleName":"Control.Monad.List.Trans","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","List","Trans"],"ListT"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Drain a `ListT`, running it to completion and discarding all values.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[33,1],"name":".spago/transformers/v5.1.0/src/Control/Monad/Identity/Trans.purs","end":[33,49]},"score":8,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"runIdentityT","moduleName":"Control.Monad.Identity.Trans","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","Identity","Trans"],"IdentityT"]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"a"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Run a computation in the `IdentityT` monad.\n"}],"tag":"SearchResult"}]