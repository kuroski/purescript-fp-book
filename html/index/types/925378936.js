// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["925378936"] = [{"values":[{"sourceSpan":{"start":[721,1],"name":".spago/lists/v6.0.1/src/Data/List.purs","end":[721,61]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"nubByEq","moduleName":"Data.List","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Remove duplicate elements from a list, using the provided equivalence function.\nKeeps the first occurrence of each element in the input list,\nin the same order they appear in the input list.\nThis less efficient version of `nubBy` only requires an equivalence\nfunction, rather than an ordering function.\n\n```purescript\nmod3eq = eq `on` \\n -> mod n 3\nnubByEq mod3eq 1:3:4:5:6:Nil == 1:3:5:Nil\n```\n\nRunning time: `O(n^2)`\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[686,1],"name":".spago/lists/v6.0.1/src/Data/List.purs","end":[686,60]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"nubBy","moduleName":"Data.List","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Ordering"],"Ordering"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Remove duplicate elements from a list based on the provided comparison function.\nKeeps the first occurrence of each element in the input list,\nin the same order they appear in the input list.\n\n```purescript\nnubBy (compare `on` Array.length) ([1]:[2]:[3,4]:Nil) == [1]:[3,4]:Nil\n```\n\nRunning time: `O(n log n)`\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[449,1],"name":".spago/lists/v6.0.1/src/Data/List.purs","end":[449,61]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"sortBy","moduleName":"Data.List","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Ordering"],"Ordering"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Sort the elements of a list in increasing order, where elements are\ncompared using the specified ordering.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[292,1],"name":".spago/lists/v6.0.1/src/Data/List/NonEmpty.purs","end":[292,77]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"nubByEq","moduleName":"Data.List.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[286,1],"name":".spago/lists/v6.0.1/src/Data/List/NonEmpty.purs","end":[286,76]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"nubBy","moduleName":"Data.List.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Ordering"],"Ordering"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[247,1],"name":".spago/lists/v6.0.1/src/Data/List/NonEmpty.purs","end":[247,77]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"sortBy","moduleName":"Data.List.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Ordering"],"Ordering"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[630,1],"name":".spago/lists/v6.0.1/src/Data/List/Lazy.purs","end":[630,61]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"nubByEq","moduleName":"Data.List.Lazy","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Lazy","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Lazy","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Remove duplicate elements from a list, using the specified\nfunction to determine equality of elements.\n\nRunning time: `O(n^2)`\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[609,1],"name":".spago/lists/v6.0.1/src/Data/List/Lazy.purs","end":[609,60]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"nubBy","moduleName":"Data.List.Lazy","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Ordering"],"Ordering"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Lazy","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Lazy","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Remove duplicate elements from a list based on the provided comparison function.\nKeeps the first occurrence of each element in the input list,\nin the same order they appear in the input list.\n\nRunning time: `O(n log n)`\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[1075,1],"name":".spago/arrays/v6.0.1/src/Data/Array.purs","end":[1075,63]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"nubByEq","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Remove the duplicates from an array, where element equality is determined\nby the specified equivalence relation, creating a new array.\n\nThis less efficient version of `nubBy` only requires an equivalence\nrelation.\n\n```purescript\nmod3eq a b = a `mod` 3 == b `mod` 3\nnubByEq mod3eq [1, 3, 4, 5, 6] = [1, 3, 5]\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[1049,1],"name":".spago/arrays/v6.0.1/src/Data/Array.purs","end":[1049,62]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"nubBy","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Ordering"],"Ordering"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Remove the duplicates from an array, where element equality is determined\nby the specified ordering, creating a new array.\n\n```purescript\nnubBy compare [1, 3, 4, 2, 2, 1] == [1, 3, 4, 2]\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[830,1],"name":".spago/arrays/v6.0.1/src/Data/Array.purs","end":[830,63]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"sortBy","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Ordering"],"Ordering"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Sort the elements of an array in increasing order, where elements are\ncompared using the specified partial ordering, creating a new array.\nSorting is stable: the order of elements is preserved if they are equal\naccording to the specified partial ordering.\n\n```purescript\ncompareLength a b = compare (length a) (length b)\nsortBy compareLength [[1, 2, 3], [7, 9], [-2]] = [[-2],[7,9],[1,2,3]]\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[461,1],"name":".spago/arrays/v6.0.1/src/Data/Array/NonEmpty.purs","end":[461,79]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"nubByEq","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[458,1],"name":".spago/arrays/v6.0.1/src/Data/Array/NonEmpty.purs","end":[458,78]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"nubBy","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Ordering"],"Ordering"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[375,1],"name":".spago/arrays/v6.0.1/src/Data/Array/NonEmpty.purs","end":[375,79]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"sortBy","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Ordering"],"Ordering"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]