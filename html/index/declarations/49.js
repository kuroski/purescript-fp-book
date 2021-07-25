// This file was generated by purescript-docs-search.
window.DocsSearchIndex["49"] = [["facility",[{"values":[{"sourceSpan":null,"score":200000,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"Facility","moduleName":"Ch6","info":{"values":[{"arguments":[{"tag":"TypeConstructor","contents":[["Ch6"],"Address"]}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[72,1],"name":"src/Ch6.purs","end":[72,22]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"facility","moduleName":"Ch6","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Ch6"],"Residence"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["fail",[{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":[],"tag":"Builtin"},"name":"Fail","moduleName":"Prim.TypeError","info":{"values":[{"superclasses":[],"fundeps":[],"arguments":[["message",{"tag":"TypeConstructor","contents":[["Prim","TypeError"],"Doc"]}]]}],"tag":"TypeClassResult"},"hashAnchor":"t","comments":"The Fail type class is part of the custom type errors feature. To provide\na custom type error when someone tries to use a particular instance,\nwrite that instance out with a Fail constraint.\n\nFor more information, see\n[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md).\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[21,1],"name":".spago/spec/v5.0.1/src/Test/Spec/Assertions.purs","end":[21,57]},"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"fail","moduleName":"Test.Spec.Assertions","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Error","Class"],"MonadThrow"],"constraintArgs":[{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["failed",[{"values":[{"sourceSpan":null,"score":1,"packageInfo":{"values":["fork"],"tag":"Package"},"name":"Failed","moduleName":"Control.Monad.Fork.Class","info":{"values":[{"arguments":[{"tag":"TypeVar","contents":"e"}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["failure",[{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"Failure","moduleName":"Test.Spec.Result","info":{"values":[{"arguments":[{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["false",[{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":[],"tag":"Builtin"},"name":"False","moduleName":"Prim.Boolean","info":{"values":[{"kind":{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"The 'False' boolean type.\n"}],"tag":"SearchResult"}]],["fanin",[{"values":[{"sourceSpan":{"start":[74,1],"name":".spago/profunctor/v5.0.0/src/Data/Profunctor/Choice.purs","end":[80,22]},"score":2,"packageInfo":{"values":["profunctor"],"tag":"Package"},"name":"fanin","moduleName":"Data.Profunctor.Choice","info":{"values":[{"type":{"tag":"ForAll","contents":["p",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["c",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Category"],"Category"],"constraintArgs":[{"tag":"TypeVar","contents":"p"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Profunctor","Choice"],"Choice"],"constraintArgs":[{"tag":"TypeVar","contents":"p"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"p"},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"c"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"p"},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"p"},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}}]},{"tag":"TypeVar","contents":"c"}]}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Compose a value which eliminates a sum from two values, each eliminating\none side of the sum.\n\nThis combinator is useful when assembling values from smaller components,\nbecause it provides a way to support two different types of input.\n\nSpecializing `(|||)` to function application would look like this:\n```\n(|||) :: forall a b c d. (a -> c) -> (b -> c) -> Either a b -> c\n```\nWe take two functions, `f` and `g`, which both return the same type `c` and we transform them into a\nsingle function which takes an `Either` value with the parameter type of `f` on the left side and\nthe parameter type of `g` on the right side. The function then runs either `f` or `g`, depending on\nwhether the `Either` value is a `Left` or a `Right`.\nThis allows us to bundle two different computations which both have the same result type into one\nfunction which will run the approriate computation based on the parameter supplied in the `Either` value.\n"}],"tag":"SearchResult"}]],["fanout",[{"values":[{"sourceSpan":{"start":[71,1],"name":".spago/profunctor/v5.0.0/src/Data/Profunctor/Strong.purs","end":[77,21]},"score":2,"packageInfo":{"values":["profunctor"],"tag":"Package"},"name":"fanout","moduleName":"Data.Profunctor.Strong","info":{"values":[{"type":{"tag":"ForAll","contents":["p",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["c",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Category"],"Category"],"constraintArgs":[{"tag":"TypeVar","contents":"p"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Profunctor","Strong"],"Strong"],"constraintArgs":[{"tag":"TypeVar","contents":"p"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"p"},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"p"},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"c"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"p"},{"tag":"TypeVar","contents":"a"}]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple"],"Tuple"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]}}]}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Compose a value which introduces a `Tuple` from two values, each introducing\none side of the `Tuple`.\n\nThis combinator is useful when assembling values from smaller components,\nbecause it provides a way to support two different types of output.\n\nSpecializing `(&&&)` to function application would look like this:\n```\n(&&&) :: forall a b c. (a -> b) -> (a -> c) -> (a -> (Tuple b c))\n```\nWe take two functions, `f` and `g`, with the same parameter type and we transform them into a\nsingle function which takes one parameter and returns a `Tuple` of the results of running\n`f` and `g` on the parameter, respectively.  This allows us to run two parallel computations\non the same input and return both results in a `Tuple`.\n"}],"tag":"SearchResult"}]],["fast",[{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"Fast","moduleName":"Test.Spec.Speed","info":{"values":[{"arguments":[]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["c2",[{"values":[{"sourceSpan":{"start":[32,1],"name":".spago/functors/v4.1.1/src/Data/Functor/Coproduct/Nested.purs","end":[32,28]},"score":4,"packageInfo":{"values":["functors"],"tag":"Package"},"name":"C2","moduleName":"Data.Functor.Coproduct.Nested","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Functor","Coproduct"],"Coproduct"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"z"}]},"arguments":[["a",null],["z",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"}]],["before",[{"values":[{"sourceSpan":{"start":[286,1],"name":".spago/spec/v5.0.1/src/Test/Spec.purs","end":[286,89]},"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"before","moduleName":"Test.Spec","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["g",{"tag":"ForAll","contents":["i",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"g"},{"tag":"TypeVar","contents":"i"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Run a custom action before every spec item.\n"}],"tag":"SearchResult"}]],["before_",[{"values":[{"sourceSpan":{"start":[290,1],"name":".spago/spec/v5.0.1/src/Test/Spec.purs","end":[290,90]},"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"before_","moduleName":"Test.Spec","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["g",{"tag":"ForAll","contents":["i",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"g"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Run a custom action before every spec item.\n"}],"tag":"SearchResult"}]],["beforeall",[{"values":[{"sourceSpan":{"start":[298,1],"name":".spago/spec/v5.0.1/src/Test/Spec.purs","end":[298,123]},"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"beforeAll","moduleName":"Test.Spec","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["g",{"tag":"ForAll","contents":["i",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Effect","Class"],"MonadEffect"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Effect","Aff","Class"],"MonadAff"],"constraintArgs":[{"tag":"TypeVar","contents":"g"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Error","Class"],"MonadError"],"constraintArgs":[{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"g"},{"tag":"TypeVar","contents":"i"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Run a custom action before the first spec item.\n"}],"tag":"SearchResult"}]],["beforeall_",[{"values":[{"sourceSpan":{"start":[304,1],"name":".spago/spec/v5.0.1/src/Test/Spec.purs","end":[304,124]},"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"beforeAll_","moduleName":"Test.Spec","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["g",{"tag":"ForAll","contents":["i",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Effect","Class"],"MonadEffect"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Effect","Aff","Class"],"MonadAff"],"constraintArgs":[{"tag":"TypeVar","contents":"g"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Error","Class"],"MonadError"],"constraintArgs":[{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"g"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Run a custom action before the first spec item.\n"}],"tag":"SearchResult"}]],["beforewith",[{"values":[{"sourceSpan":{"start":[294,1],"name":".spago/spec/v5.0.1/src/Test/Spec.purs","end":[294,102]},"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"beforeWith","moduleName":"Test.Spec","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["g",{"tag":"ForAll","contents":["i",{"tag":"ForAll","contents":["i'",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"i'"}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"g"},{"tag":"TypeVar","contents":"i"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i'"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]}]}]}]},null]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Run a custom action before every spec item.\n"}],"tag":"SearchResult"}]],["beside",[{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":[],"tag":"Builtin"},"name":"Beside","moduleName":"Prim.TypeError","info":{"values":[{"kind":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim","TypeError"],"Doc"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim","TypeError"],"Doc"]}]},{"tag":"TypeConstructor","contents":[["Prim","TypeError"],"Doc"]}]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"The Beside type constructor combines two Docs horizontally\nto be used in a custom type error.\n\nFor more information, see\n[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md).\n"}],"tag":"SearchResult"}]],["between",[{"values":[{"sourceSpan":{"start":[203,1],"name":".spago/prelude/v5.0.1/src/Data/Ord.purs","end":[203,53]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"between","moduleName":"Data.Ord","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Test whether a value is between a minimum and a maximum (inclusive).\nFor example:\n\n``` purescript\nlet f = between 0 10\nf 0    == true\nf (-5) == false\nf 5    == true\nf 10   == true\nf 15   == false\n```\n"}],"tag":"SearchResult"}]],["aff",[{"values":[{"sourceSpan":{"start":[64,1],"name":".spago/aff/v6.0.0/src/Effect/Aff.purs","end":[64,38]},"score":4,"packageInfo":{"values":["aff"],"tag":"Package"},"name":"Aff","moduleName":"Effect.Aff","info":{"values":[{"kind":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"An `Aff a` is an asynchronous computation with effects. The\ncomputation may either error with an exception, or produce a result of\ntype `a`. `Aff` effects are assembled from primitive `Effect` effects using\n`makeAff` or `liftEffect`.\n"}],"tag":"SearchResult"}]],["after",[{"values":[{"sourceSpan":{"start":[271,1],"name":".spago/spec/v5.0.1/src/Test/Spec.purs","end":[271,111]},"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"after","moduleName":"Test.Spec","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["g",{"tag":"ForAll","contents":["e",{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["i",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Fork","Class"],"MonadBracket"],"constraintArgs":[{"tag":"TypeVar","contents":"e"},{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec","Tree"],"ActionWith"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]}]}]}]},null]},null]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Run a custom action after every spec item.\n"}],"tag":"SearchResult"}]],["after_",[{"values":[{"sourceSpan":{"start":[278,1],"name":".spago/spec/v5.0.1/src/Test/Spec.purs","end":[278,104]},"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"after_","moduleName":"Test.Spec","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["g",{"tag":"ForAll","contents":["e",{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["i",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Fork","Class"],"MonadBracket"],"constraintArgs":[{"tag":"TypeVar","contents":"e"},{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"g"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]}]}]}]},null]},null]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Run a custom action after every spec item.\n"}],"tag":"SearchResult"}]],["afterall",[{"values":[{"sourceSpan":{"start":[325,1],"name":".spago/spec/v5.0.1/src/Test/Spec.purs","end":[325,88]},"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"afterAll","moduleName":"Test.Spec","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["g",{"tag":"ForAll","contents":["i",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec","Tree"],"ActionWith"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Run a custom action after the last spec item.\n"}],"tag":"SearchResult"}]],["afterall_",[{"values":[{"sourceSpan":{"start":[329,1],"name":".spago/spec/v5.0.1/src/Test/Spec.purs","end":[329,81]},"score":0,"packageInfo":{"values":["spec"],"tag":"Package"},"name":"afterAll_","moduleName":"Test.Spec","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["g",{"tag":"ForAll","contents":["i",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"g"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Spec"],"SpecT"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Run a custom action after the last spec item.\n"}],"tag":"SearchResult"}]]]