// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1829857997"] = [{"values":[{"sourceSpan":{"start":[33,1],"name":".spago/spec/v5.0.1/src/Test/Spec/Assertions/String.purs","end":[33,76]},"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"shouldEndWith","moduleName":"Test.Spec.Assertions.String","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Error","Class"],"MonadThrow"],"constraintArgs":[{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Asserts `string` ends with `suffix`\n\n```purescript\nstring `shouldEndWith` suffix\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[23,1],"name":".spago/spec/v5.0.1/src/Test/Spec/Assertions/String.purs","end":[23,78]},"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"shouldStartWith","moduleName":"Test.Spec.Assertions.String","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Error","Class"],"MonadThrow"],"constraintArgs":[{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Asserts `string` starts with `prefix`\n\n```purescript\nstring `shouldStartWith` prefix\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[53,1],"name":".spago/spec/v5.0.1/src/Test/Spec/Assertions/String.purs","end":[53,79]},"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"shouldNotContain","moduleName":"Test.Spec.Assertions.String","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Error","Class"],"MonadThrow"],"constraintArgs":[{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Asserts `string` does not contain `subs`\n\n```purescript\nstring `shouldContain` subs\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[43,1],"name":".spago/spec/v5.0.1/src/Test/Spec/Assertions/String.purs","end":[43,76]},"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"shouldContain","moduleName":"Test.Spec.Assertions.String","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Error","Class"],"MonadThrow"],"constraintArgs":[{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Asserts `string` contains `subs`\n\n```purescript\nstring `shouldContain` subs\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[107,1],"name":".spago/unfoldable/v5.0.0/src/Data/Unfoldable1.purs","end":[107,56]},"score":9,"packageInfo":{"values":["unfoldable"],"tag":"Package"},"name":"range","moduleName":"Data.Unfoldable1","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Unfoldable1"],"Unfoldable1"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create an `Unfoldable1` containing a range of values, including both\nendpoints.\n\n``` purescript\nrange 0 0 == (NEL.singleton 0 :: NEL.NonEmptyList Int)\nrange 1 2 == (NEL.cons 1 (NEL.singleton 2) :: NEL.NonEmptyList Int)\nrange 2 0 == (NEL.cons 2 (NEL.cons 1 (NEL.singleton 0)) :: NEL.NonEmptyList Int)\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[160,1],"name":".spago/lists/v6.0.1/src/Data/List/Lazy.purs","end":[160,42]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"replicate","moduleName":"Data.List.Lazy","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Lazy","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create a list with repeated instances of a value.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[57,1],"name":".spago/datetime/v5.0.2/src/Data/DateTime.purs","end":[57,66]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"adjust","moduleName":"Data.DateTime","info":{"values":[{"type":{"tag":"ForAll","contents":["d",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Time","Duration"],"Duration"],"constraintArgs":[{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","DateTime"],"DateTime"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","DateTime"],"DateTime"]}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Adjusts a date/time value with a duration offset. `Nothing` is returned\nif the resulting date would be outside of the range of valid dates.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[195,1],"name":".spago/arrays/v6.0.1/src/Data/Array.purs","end":[195,58]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"replicate","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create an array containing a value repeated the specified number of times.\n```purescript\nreplicate 2 \"Hi\" = [\"Hi\", \"Hi\"]\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[205,1],"name":".spago/arrays/v6.0.1/src/Data/Array/NonEmpty.purs","end":[205,51]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"replicate","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Replicate an item at least once\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[18,3],"name":".spago/gen/v3.0.0/src/Control/Monad/Gen/Class.purs","end":[18,46]},"score":4,"packageInfo":{"values":["gen"],"tag":"Package"},"name":"chooseFloat","moduleName":"Control.Monad.Gen.Class","info":{"values":[{"typeClassArguments":[["m",null]],"typeClass":[["Control","Monad","Gen","Class"],"MonadGen"],"type":{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Gen","Class"],"MonadGen"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Number"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Number"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeConstructor","contents":[["Prim"],"Number"]}]}]}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":"Chooses an floating point number in the specified (inclusive) range.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[15,3],"name":".spago/gen/v3.0.0/src/Control/Monad/Gen/Class.purs","end":[15,35]},"score":4,"packageInfo":{"values":["gen"],"tag":"Package"},"name":"chooseInt","moduleName":"Control.Monad.Gen.Class","info":{"values":[{"typeClassArguments":[["m",null]],"typeClass":[["Control","Monad","Gen","Class"],"MonadGen"],"type":{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Gen","Class"],"MonadGen"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":"Chooses an integer in the specified (inclusive) range.\n"}],"tag":"SearchResult"}]