// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["2056031036"] = [{"values":[{"sourceSpan":{"start":[263,1],"name":".spago/either/v5.0.0/src/Data/Either.purs","end":[263,47]},"score":15,"packageInfo":{"values":["either"],"tag":"Package"},"name":"note","moduleName":"Data.Either","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["b",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"b"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Takes a default and a `Maybe` value, if the value is a `Just`, turn it into\na `Right`, if the value is a `Nothing` use the provided default as a `Left`\n\n```purescript\nnote \"default\" Nothing = Left \"default\"\nnote \"default\" (Just 1) = Right 1\n```\n"}],"tag":"SearchResult"}]