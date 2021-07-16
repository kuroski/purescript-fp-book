// This file was generated by purescript-docs-search.
window.DocsSearchIndex["25"] = [["≇",[{"values":[{"sourceSpan":{"start":[76,1],"name":".spago/numbers/v8.0.0/src/Data/Number/Approximate.purs","end":[76,28]},"score":1,"packageInfo":{"values":["numbers"],"tag":"Package"},"name":"(≇)","moduleName":"Data.Number.Approximate","info":{"values":[],"tag":"ValueAliasResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["snd",[{"values":[{"sourceSpan":{"start":[122,1],"name":".spago/tuples/v6.0.1/src/Data/Tuple.purs","end":[122,34]},"score":19,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"snd","moduleName":"Data.Tuple","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["b",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple"],"Tuple"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]},{"tag":"TypeVar","contents":"b"}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the second component of a tuple.\n"}],"tag":"SearchResult"}]],["snoc",[{"values":[{"sourceSpan":{"start":[81,1],"name":"src/Ch5.purs","end":[81,35]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"snoc","moduleName":"Ch5","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Append an element to the end of a list, creating a new list.\n\n```purescript\nsnoc (1 : 2 : Nil) 3 = (1 : 2 : 3 : Nil)\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[226,1],"name":".spago/arrays/v6.0.1/src/Data/Array/NonEmpty.purs","end":[226,58]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"snoc","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[265,1],"name":".spago/arrays/v6.0.1/src/Data/Array.purs","end":[265,42]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"snoc","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Append an element to the end of an array, creating a new array.\n\n```purescript\nsnoc [1, 2, 3] 4 = [1, 2, 3, 4]\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[91,1],"name":".spago/catenable-lists/v6.0.1/src/Data/CatList.purs","end":[91,46]},"score":1,"packageInfo":{"values":["catenable-lists"],"tag":"Package"},"name":"snoc","moduleName":"Data.CatList","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","CatList"],"CatList"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","CatList"],"CatList"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Append an element to the end of the catenable list, creating a new\ncatenable list.\n\nRunning time: `O(1)`\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[75,1],"name":".spago/catenable-lists/v6.0.1/src/Data/CatQueue.purs","end":[75,48]},"score":1,"packageInfo":{"values":["catenable-lists"],"tag":"Package"},"name":"snoc","moduleName":"Data.CatQueue","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","CatQueue"],"CatQueue"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","CatQueue"],"CatQueue"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Append an element to the end of the queue, creating a new queue.\n\nRunning time: `O(1)`\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[222,1],"name":".spago/lists/v6.0.1/src/Data/List/Lazy.purs","end":[222,40]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"snoc","moduleName":"Data.List.Lazy","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Lazy","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Lazy","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Append an element to the end of a list, creating a new list.\n\nRunning time: `O(n)`\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[142,1],"name":".spago/lists/v6.0.1/src/Data/List/NonEmpty.purs","end":[142,56]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"snoc","moduleName":"Data.List.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[207,1],"name":".spago/lists/v6.0.1/src/Data/List.purs","end":[207,40]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"snoc","moduleName":"Data.List","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Append an element to the end of a list, creating a new list.\n\nRunning time: `O(n)`\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[66,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodePoints.purs","end":[66,46]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"snoc","moduleName":"Data.String.NonEmpty.CodePoints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","CodePoints"],"CodePoint"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[88,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodeUnits.purs","end":[88,41]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"snoc","moduleName":"Data.String.NonEmpty.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Char"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Creates a `NonEmptyString` from a string by appending a character.\n\n```purescript\nsnoc 'c' \"ab\" = NonEmptyString \"abc\"\nsnoc 'a' \"\" = NonEmptyString \"a\"\n```\n"}],"tag":"SearchResult"}]],["snoc'",[{"values":[{"sourceSpan":{"start":[229,1],"name":".spago/arrays/v6.0.1/src/Data/Array/NonEmpty.purs","end":[229,51]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"snoc'","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[145,1],"name":".spago/lists/v6.0.1/src/Data/List/NonEmpty.purs","end":[145,51]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"snoc'","moduleName":"Data.List.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["round",[{"values":[{"sourceSpan":{"start":[61,1],"name":".spago/integers/v5.0.0/src/Data/Int.purs","end":[61,23]},"score":3,"packageInfo":{"values":["integers"],"tag":"Package"},"name":"round","moduleName":"Data.Int","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Number"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Convert a `Number` to an `Int`, by taking the nearest integer to the\nargument. Values outside the `Int` range are clamped, `NaN` and `Infinity`\nvalues return 0.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[55,1],"name":".spago/math/v3.0.0/src/Math.purs","end":[55,41]},"score":6,"packageInfo":{"values":["math"],"tag":"Package"},"name":"round","moduleName":"Math","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Number"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Number"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the integer closest to the argument.\n"}],"tag":"SearchResult"}]],["row",[{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":[],"tag":"Builtin"},"name":"Row","moduleName":"Prim","info":{"values":[{"kind":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"`Row` is the kind constructor of label-indexed types which map type-level strings to other types.\nFor example, the kind of `Record` is `Row Type -> Type`, mapping field names to values.\n"}],"tag":"SearchResult"}]],["rowlist",[{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":[],"tag":"Builtin"},"name":"RowList","moduleName":"Prim.RowList","info":{"values":[{"kind":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"A type level list representation of a row of types.\n"}],"tag":"SearchResult"}]],["rowtolist",[{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":[],"tag":"Builtin"},"name":"RowToList","moduleName":"Prim.RowList","info":{"values":[{"superclasses":[],"fundeps":[[["row"],["list"]]],"arguments":[["row",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Row"]},{"tag":"TypeVar","contents":"k"}]}],["list",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim","RowList"],"RowList"]},{"tag":"TypeVar","contents":"k"}]}]]}],"tag":"TypeClassResult"},"hashAnchor":"t","comments":"Compiler solved type class for generating a `RowList` from a closed row\nof types.  Entries are sorted by label and duplicates are preserved in\nthe order they appeared in the row.\n"}],"tag":"SearchResult"}]],["or",[{"values":[{"sourceSpan":{"start":[364,1],"name":".spago/foldable-traversable/v5.0.1/src/Data/Foldable.purs","end":[364,61]},"score":15,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"or","moduleName":"Data.Foldable","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","HeytingAlgebra"],"HeytingAlgebra"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The disjunction of all the values in a data structure. When specialized\nto `Boolean`, this function will test whether any of the values in a data\nstructure is `true`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[18,1],"name":".spago/integers/v5.0.0/src/Data/Int/Bits.purs","end":[18,39]},"score":3,"packageInfo":{"values":["integers"],"tag":"Package"},"name":"or","moduleName":"Data.Int.Bits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Bitwise OR.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[270,1],"name":".spago/pipes/v7.0.1/src/Pipes/Prelude.purs","end":[270,64]},"score":1,"packageInfo":{"values":["pipes"],"tag":"Package"},"name":"or","moduleName":"Pipes.Prelude","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Pipes","Core"],"Producer"]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Determines whether any element is `True`\n"}],"tag":"SearchResult"}]],["ord",[{"values":[{"sourceSpan":{"start":[37,1],"name":".spago/prelude/v5.0.1/src/Data/Ord.purs","end":[38,32]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"Ord","moduleName":"Data.Ord","info":{"values":[{"superclasses":[{"constraintClass":[["Data","Eq"],"Eq"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]}],"fundeps":[],"arguments":[["a",null]]}],"tag":"TypeClassResult"},"hashAnchor":"t","comments":"The `Ord` type class represents types which support comparisons with a\n_total order_.\n\n`Ord` instances should satisfy the laws of total orderings:\n\n- Reflexivity: `a <= a`\n- Antisymmetry: if `a <= b` and `b <= a` then `a = b`\n- Transitivity: if `a <= b` and `b <= c` then `a <= c`\n"}],"tag":"SearchResult"}]],["ord1",[{"values":[{"sourceSpan":{"start":[220,1],"name":".spago/prelude/v5.0.1/src/Data/Ord.purs","end":[221,56]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"Ord1","moduleName":"Data.Ord","info":{"values":[{"superclasses":[{"constraintClass":[["Data","Eq"],"Eq1"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]}],"fundeps":[],"arguments":[["f",null]]}],"tag":"TypeClassResult"},"hashAnchor":"t","comments":"The `Ord1` type class represents totally ordered type constructors.\n"}],"tag":"SearchResult"}]],["ordering",[{"values":[{"sourceSpan":{"start":[13,1],"name":".spago/prelude/v5.0.1/src/Data/Ordering.purs","end":[13,29]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"Ordering","moduleName":"Data.Ordering","info":{"values":[{"typeArguments":[],"dataDeclType":"data"}],"tag":"DataResult"},"hashAnchor":"t","comments":"The `Ordering` data type represents the three possible outcomes of\ncomparing two values:\n\n`LT` - The first value is _less than_ the second.\n`GT` - The first value is _greater than_ the second.\n`EQ` - The first value is _equal to_ the second.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":[],"tag":"Builtin"},"name":"Ordering","moduleName":"Prim.Ordering","info":{"values":[{"kind":{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"The `Ordering` kind represents the three possibilities of comparing two\ntypes of the same kind: `LT` (less than), `EQ` (equal to), and\n`GT` (greater than).\n"}],"tag":"SearchResult"}]],["ordrecord",[{"values":[{"sourceSpan":{"start":[227,1],"name":".spago/prelude/v5.0.1/src/Data/Ord.purs","end":[228,91]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"OrdRecord","moduleName":"Data.Ord","info":{"values":[{"superclasses":[{"constraintClass":[["Data","Eq"],"EqRecord"],"constraintArgs":[{"tag":"TypeVar","contents":"rowlist"},{"tag":"TypeVar","contents":"row"}]}],"fundeps":[],"arguments":[["rowlist",null],["row",null]]}],"tag":"TypeClassResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"}]]]