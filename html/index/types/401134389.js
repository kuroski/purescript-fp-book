// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["401134389"] = [{"values":[{"sourceSpan":{"start":[644,1],"name":".spago/ordered-collections/v2.0.1/src/Data/Map/Internal.purs","end":[644,70]},"score":1,"packageInfo":{"values":["ordered-collections"],"tag":"Package"},"name":"unions","moduleName":"Data.Map.Internal","info":{"values":[{"type":{"tag":"ForAll","contents":["k",{"tag":"ForAll","contents":["v",{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"k"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Map","Internal"],"Map"]},{"tag":"TypeVar","contents":"k"}]},{"tag":"TypeVar","contents":"v"}]}}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Map","Internal"],"Map"]},{"tag":"TypeVar","contents":"k"}]},{"tag":"TypeVar","contents":"v"}]}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Compute the union of a collection of maps\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[573,1],"name":".spago/ordered-collections/v2.0.1/src/Data/Map/Internal.purs","end":[573,78]},"score":1,"packageInfo":{"values":["ordered-collections"],"tag":"Package"},"name":"fromFoldable","moduleName":"Data.Map.Internal","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["k",{"tag":"ForAll","contents":["v",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"k"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple"],"Tuple"]},{"tag":"TypeVar","contents":"k"}]},{"tag":"TypeVar","contents":"v"}]}}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Map","Internal"],"Map"]},{"tag":"TypeVar","contents":"k"}]},{"tag":"TypeVar","contents":"v"}]}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Convert any foldable collection of key/value pairs to a map.\nOn key collision, later values take precedence over earlier ones.\n"}],"tag":"SearchResult"}]