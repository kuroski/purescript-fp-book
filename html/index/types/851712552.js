// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["851712552"] = [{"values":[{"sourceSpan":{"start":[271,1],"name":".spago/lists/v6.0.1/src/Data/List.purs","end":[271,66]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"unsnoc","moduleName":"Data.List","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"RCons","contents":["init",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"RCons","contents":["last",{"tag":"TypeVar","contents":"a"},{"tag":"REmpty","contents":{}}]}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Break a list into its last element, and the preceding elements,\nor `Nothing` if the list is empty.\n\nRunning time: `O(n)`\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[377,1],"name":".spago/arrays/v6.0.1/src/Data/Array.purs","end":[377,68]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"unsnoc","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"RCons","contents":["init",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"RCons","contents":["last",{"tag":"TypeVar","contents":"a"},{"tag":"REmpty","contents":{}}]}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Break an array into its last element and all preceding elements.\n\n```purescript\nunsnoc [1, 2, 3] = Just {init: [1, 2], last: 3}\nunsnoc [] = Nothing\n```\n\nRunning time: `O(n)` where `n` is the length of the array\n"}],"tag":"SearchResult"}]