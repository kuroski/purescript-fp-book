// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1647047437"] = [{"values":[{"sourceSpan":{"start":[267,1],"name":".spago/maybe/v5.0.0/src/Data/Maybe.purs","end":[267,52]},"score":21,"packageInfo":{"values":["maybe"],"tag":"Package"},"name":"fromMaybe'","moduleName":"Data.Maybe","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]},{"tag":"TypeVar","contents":"a"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"a"}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Similar to `fromMaybe` but for use in cases where the default value may be\nexpensive to compute. As PureScript is not lazy, the standard `fromMaybe`\nhas to evaluate the default value before returning the result, whereas here\nthe value is only computed when the `Maybe` is known to be `Nothing`.\n\n``` purescript\nfromMaybe' (\\_ -> x) Nothing == x\nfromMaybe' (\\_ -> x) (Just y) == y\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[1256,1],"name":".spago/arrays/v6.0.1/src/Data/Array.purs","end":[1256,69]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"all","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns true if all the array elements satisfy the given predicate.\niterating the array only as necessary and stopping as soon as the predicate\nyields false.\n\n```purescript\nall (_ > 0) [] = True\nall (_ > 0) [1, 2, 3] = True\nall (_ > 0) [-1, -2, -3] = False\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[1245,1],"name":".spago/arrays/v6.0.1/src/Data/Array.purs","end":[1245,69]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"any","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns true if at least one array element satisfies the given predicate,\niterating the array only as necessary and stopping as soon as the predicate\nyields true.\n\n```purescript\nany (_ > 0) [] = False\nany (_ > 0) [-1, 0, 1] = True\nany (_ > 0) [-1, -2, -3] = False\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[549,1],"name":".spago/arrays/v6.0.1/src/Data/Array/NonEmpty.purs","end":[549,62]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"all","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[546,1],"name":".spago/arrays/v6.0.1/src/Data/Array/NonEmpty.purs","end":[546,62]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"any","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]