// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["92270697"] = [{"values":[{"sourceSpan":{"start":[68,1],"name":".spago/nonempty/v6.0.0/src/Data/NonEmpty.purs","end":[68,53]},"score":5,"packageInfo":{"values":["nonempty"],"tag":"Package"},"name":"singleton","moduleName":"Data.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Plus"],"Plus"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","NonEmpty"],"NonEmpty"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create a non-empty structure with a single value.\n\n```purescript\nimport Prelude\n\nsingleton 1 == 1 :| []\nsingleton 1 == 1 :| Nil\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[60,1],"name":".spago/either/v5.0.0/src/Data/Either/Nested.purs","end":[60,31]},"score":15,"packageInfo":{"values":["either"],"tag":"Package"},"name":"in1","moduleName":"Data.Either.Nested","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["z",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"BinaryNoParensType","contents":[{"tag":"TypeOp","contents":[["Data","Either","Nested"],"\\/"]},{"tag":"TypeVar","contents":"a"},{"tag":"TypeVar","contents":"z"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[111,1],"name":".spago/transformers/v5.1.0/src/Control/Monad/List/Trans.purs","end":[111,57]},"score":8,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"singleton","moduleName":"Control.Monad.List.Trans","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Applicative"],"Applicative"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","List","Trans"],"ListT"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create a list with one element.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[140,1],"name":".spago/transformers/v5.1.0/src/Control/Monad/List/Trans.purs","end":[140,48]},"score":8,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"repeat","moduleName":"Control.Monad.List.Trans","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","List","Trans"],"ListT"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Generate an infinite list by repeating a value.\n"}],"tag":"SearchResult"}]