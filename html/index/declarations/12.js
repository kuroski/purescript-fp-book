// This file was generated by purescript-docs-search.
window.DocsSearchIndex["12"] = [["saturday",[{"values":[{"sourceSpan":null,"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"Saturday","moduleName":"Data.Date.Component","info":{"values":[{"arguments":[]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["saveposition",[{"values":[{"sourceSpan":null,"score":1,"packageInfo":{"values":["ansi"],"tag":"Package"},"name":"SavePosition","moduleName":"Ansi.Codes","info":{"values":[{"arguments":[]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["kill",[{"values":[{"sourceSpan":{"start":[54,3],"name":".spago/fork/v5.0.0/src/Control/Monad/Fork/Class.purs","end":[54,31]},"score":1,"packageInfo":{"values":["fork"],"tag":"Package"},"name":"kill","moduleName":"Control.Monad.Fork.Class","info":{"values":[{"typeClassArguments":[["e",null],["f",null],["m",null]],"typeClass":[["Control","Monad","Fork","Class"],"MonadKill"],"type":{"tag":"ForAll","contents":["e",{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Fork","Class"],"MonadKill"],"constraintArgs":[{"tag":"TypeVar","contents":"e"},{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]}]},null]},null]},null]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[76,1],"name":".spago/avar/v4.0.0/src/Effect/Aff/AVar.purs","end":[76,38]},"score":1,"packageInfo":{"values":["avar"],"tag":"Package"},"name":"kill","moduleName":"Effect.Aff.AVar","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","AVar"],"AVar"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Kills the AVar with an exception. All pending and future actions will\nresolve immediately with the provided exception.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[47,1],"name":".spago/avar/v4.0.0/src/Effect/AVar.purs","end":[47,41]},"score":1,"packageInfo":{"values":["avar"],"tag":"Package"},"name":"kill","moduleName":"Effect.AVar","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","AVar"],"AVar"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Kills the AVar with an exception. All pending and future actions will\nresolve immediately with the provided exception.\n"}],"tag":"SearchResult"}]],["killed",[{"values":[{"sourceSpan":null,"score":1,"packageInfo":{"values":["fork"],"tag":"Package"},"name":"Killed","moduleName":"Control.Monad.Fork.Class","info":{"values":[{"arguments":[{"tag":"TypeVar","contents":"e"}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":1,"packageInfo":{"values":["avar"],"tag":"Package"},"name":"Killed","moduleName":"Effect.AVar","info":{"values":[{"arguments":[{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["killfiber",[{"values":[{"sourceSpan":{"start":[181,1],"name":".spago/aff/v6.0.0/src/Effect/Aff.purs","end":[181,44]},"score":4,"packageInfo":{"values":["aff"],"tag":"Package"},"name":"killFiber","moduleName":"Effect.Aff","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Fiber"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Invokes pending cancelers in a fiber and runs cleanup effects. Blocks\nuntil the fiber has fully exited.\n"}],"tag":"SearchResult"}]],["fn0",[{"values":[{"sourceSpan":{"start":[6,1],"name":".spago/functions/v5.0.0/src/Data/Function/Uncurried.purs","end":[6,40]},"score":5,"packageInfo":{"values":["functions"],"tag":"Package"},"name":"Fn0","moduleName":"Data.Function.Uncurried","info":{"values":[{"kind":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"A function of zero arguments\n"}],"tag":"SearchResult"}]],["fn1",[{"values":[{"sourceSpan":{"start":[11,1],"name":".spago/functions/v5.0.0/src/Data/Function/Uncurried.purs","end":[11,22]},"score":5,"packageInfo":{"values":["functions"],"tag":"Package"},"name":"Fn1","moduleName":"Data.Function.Uncurried","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},"arguments":[["a",null],["b",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":"A function of one argument\n"}],"tag":"SearchResult"}]],["fn10",[{"values":[{"sourceSpan":{"start":[54,1],"name":".spago/functions/v5.0.0/src/Data/Function/Uncurried.purs","end":[54,121]},"score":5,"packageInfo":{"values":["functions"],"tag":"Package"},"name":"Fn10","moduleName":"Data.Function.Uncurried","info":{"values":[{"kind":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]}]}]}]}]}]}]}]}]}]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"A function of ten arguments\n"}],"tag":"SearchResult"}]],["fn2",[{"values":[{"sourceSpan":{"start":[14,1],"name":".spago/functions/v5.0.0/src/Data/Function/Uncurried.purs","end":[14,56]},"score":5,"packageInfo":{"values":["functions"],"tag":"Package"},"name":"Fn2","moduleName":"Data.Function.Uncurried","info":{"values":[{"kind":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]}]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"A function of two arguments\n"}],"tag":"SearchResult"}]],["fn3",[{"values":[{"sourceSpan":{"start":[19,1],"name":".spago/functions/v5.0.0/src/Data/Function/Uncurried.purs","end":[19,64]},"score":5,"packageInfo":{"values":["functions"],"tag":"Package"},"name":"Fn3","moduleName":"Data.Function.Uncurried","info":{"values":[{"kind":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]}]}]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"A function of three arguments\n"}],"tag":"SearchResult"}]],["fn4",[{"values":[{"sourceSpan":{"start":[24,1],"name":".spago/functions/v5.0.0/src/Data/Function/Uncurried.purs","end":[24,72]},"score":5,"packageInfo":{"values":["functions"],"tag":"Package"},"name":"Fn4","moduleName":"Data.Function.Uncurried","info":{"values":[{"kind":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]}]}]}]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"A function of four arguments\n"}],"tag":"SearchResult"}]],["fn5",[{"values":[{"sourceSpan":{"start":[29,1],"name":".spago/functions/v5.0.0/src/Data/Function/Uncurried.purs","end":[29,80]},"score":5,"packageInfo":{"values":["functions"],"tag":"Package"},"name":"Fn5","moduleName":"Data.Function.Uncurried","info":{"values":[{"kind":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]}]}]}]}]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"A function of five arguments\n"}],"tag":"SearchResult"}]],["fn6",[{"values":[{"sourceSpan":{"start":[34,1],"name":".spago/functions/v5.0.0/src/Data/Function/Uncurried.purs","end":[34,88]},"score":5,"packageInfo":{"values":["functions"],"tag":"Package"},"name":"Fn6","moduleName":"Data.Function.Uncurried","info":{"values":[{"kind":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]}]}]}]}]}]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"A function of six arguments\n"}],"tag":"SearchResult"}]],["fn7",[{"values":[{"sourceSpan":{"start":[39,1],"name":".spago/functions/v5.0.0/src/Data/Function/Uncurried.purs","end":[39,96]},"score":5,"packageInfo":{"values":["functions"],"tag":"Package"},"name":"Fn7","moduleName":"Data.Function.Uncurried","info":{"values":[{"kind":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]}]}]}]}]}]}]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"A function of seven arguments\n"}],"tag":"SearchResult"}]],["fn8",[{"values":[{"sourceSpan":{"start":[44,1],"name":".spago/functions/v5.0.0/src/Data/Function/Uncurried.purs","end":[44,104]},"score":5,"packageInfo":{"values":["functions"],"tag":"Package"},"name":"Fn8","moduleName":"Data.Function.Uncurried","info":{"values":[{"kind":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]}]}]}]}]}]}]}]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"A function of eight arguments\n"}],"tag":"SearchResult"}]],["fn9",[{"values":[{"sourceSpan":{"start":[49,1],"name":".spago/functions/v5.0.0/src/Data/Function/Uncurried.purs","end":[49,112]},"score":5,"packageInfo":{"values":["functions"],"tag":"Package"},"name":"Fn9","moduleName":"Data.Function.Uncurried","info":{"values":[{"kind":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]}]}]}]}]}]}]}]}]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"A function of nine arguments\n"}],"tag":"SearchResult"}]],["bracket",[{"values":[{"sourceSpan":{"start":[84,3],"name":".spago/fork/v5.0.0/src/Control/Monad/Fork/Class.purs","end":[84,79]},"score":1,"packageInfo":{"values":["fork"],"tag":"Package"},"name":"bracket","moduleName":"Control.Monad.Fork.Class","info":{"values":[{"typeClassArguments":[["e",null],["f",null],["m",null]],"typeClass":[["Control","Monad","Fork","Class"],"MonadBracket"],"type":{"tag":"ForAll","contents":["e",{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["r",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Fork","Class"],"MonadBracket"],"constraintArgs":[{"tag":"TypeVar","contents":"e"},{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"r"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","Fork","Class"],"BracketCondition"]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"a"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"a"}]}]}]}]}]},null]},null]},null]},null]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[300,1],"name":".spago/aff/v6.0.0/src/Effect/Aff.purs","end":[300,62]},"score":4,"packageInfo":{"values":["aff"],"tag":"Package"},"name":"bracket","moduleName":"Effect.Aff","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["b",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeVar","contents":"b"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeVar","contents":"b"}]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Guarantees resource acquisition and cleanup. The first effect may acquire\nsome resource, while the second will dispose of it. The third effect makes\nuse of the resource. Disposal is always run last, regardless. Neither\nacquisition nor disposal may be cancelled and are guaranteed to run until\nthey complete.\n"}],"tag":"SearchResult"}]],["bracketcondition",[{"values":[{"sourceSpan":{"start":[62,1],"name":".spago/fork/v5.0.0/src/Control/Monad/Fork/Class.purs","end":[65,13]},"score":1,"packageInfo":{"values":["fork"],"tag":"Package"},"name":"BracketCondition","moduleName":"Control.Monad.Fork.Class","info":{"values":[{"typeArguments":[["e",null],["a",null]],"dataDeclType":"data"}],"tag":"DataResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"}]],["bracketconditions",[{"values":[{"sourceSpan":{"start":[356,1],"name":".spago/aff/v6.0.0/src/Effect/Aff.purs","end":[360,4]},"score":4,"packageInfo":{"values":["aff"],"tag":"Package"},"name":"BracketConditions","moduleName":"Effect.Aff","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"RCons","contents":["killed",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},{"tag":"RCons","contents":["failed",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},{"tag":"RCons","contents":["completed",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},{"tag":"REmpty","contents":{}}]}]}]}]},"arguments":[["a",null],["b",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"}]],["brightblack",[{"values":[{"sourceSpan":null,"score":1,"packageInfo":{"values":["ansi"],"tag":"Package"},"name":"BrightBlack","moduleName":"Ansi.Codes","info":{"values":[{"arguments":[]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["brightblue",[{"values":[{"sourceSpan":null,"score":1,"packageInfo":{"values":["ansi"],"tag":"Package"},"name":"BrightBlue","moduleName":"Ansi.Codes","info":{"values":[{"arguments":[]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["brightcyan",[{"values":[{"sourceSpan":null,"score":1,"packageInfo":{"values":["ansi"],"tag":"Package"},"name":"BrightCyan","moduleName":"Ansi.Codes","info":{"values":[{"arguments":[]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["brightgreen",[{"values":[{"sourceSpan":null,"score":1,"packageInfo":{"values":["ansi"],"tag":"Package"},"name":"BrightGreen","moduleName":"Ansi.Codes","info":{"values":[{"arguments":[]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["brightmagenta",[{"values":[{"sourceSpan":null,"score":1,"packageInfo":{"values":["ansi"],"tag":"Package"},"name":"BrightMagenta","moduleName":"Ansi.Codes","info":{"values":[{"arguments":[]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["brightred",[{"values":[{"sourceSpan":null,"score":1,"packageInfo":{"values":["ansi"],"tag":"Package"},"name":"BrightRed","moduleName":"Ansi.Codes","info":{"values":[{"arguments":[]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["brightwhite",[{"values":[{"sourceSpan":null,"score":1,"packageInfo":{"values":["ansi"],"tag":"Package"},"name":"BrightWhite","moduleName":"Ansi.Codes","info":{"values":[{"arguments":[]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["brightyellow",[{"values":[{"sourceSpan":null,"score":1,"packageInfo":{"values":["ansi"],"tag":"Package"},"name":"BrightYellow","moduleName":"Ansi.Codes","info":{"values":[{"arguments":[]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["asin",[{"values":[{"sourceSpan":{"start":[15,1],"name":".spago/math/v3.0.0/src/Math.purs","end":[15,41]},"score":6,"packageInfo":{"values":["math"],"tag":"Package"},"name":"asin","moduleName":"Math","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Number"]}]},{"tag":"TypeConstructor","contents":[["Math"],"Radians"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the inverse sine of the argument.\n"}],"tag":"SearchResult"}]],["ask",[{"values":[{"sourceSpan":{"start":[15,3],"name":".spago/transformers/v5.1.0/src/Control/Comonad/Env/Class.purs","end":[15,28]},"score":8,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"ask","moduleName":"Control.Comonad.Env.Class","info":{"values":[{"typeClassArguments":[["e",null],["w",null]],"typeClass":[["Control","Comonad","Env","Class"],"ComonadAsk"],"type":{"tag":"ForAll","contents":["e",{"tag":"ForAll","contents":["w",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Comonad","Env","Class"],"ComonadAsk"],"constraintArgs":[{"tag":"TypeVar","contents":"e"},{"tag":"TypeVar","contents":"w"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"w"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"e"}]}]},null]},null]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[17,3],"name":".spago/transformers/v5.1.0/src/Control/Monad/Reader/Class.purs","end":[17,13]},"score":8,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"ask","moduleName":"Control.Monad.Reader.Class","info":{"values":[{"typeClassArguments":[["r",null],["m",null]],"typeClass":[["Control","Monad","Reader","Class"],"MonadAsk"],"type":{"tag":"ForAll","contents":["r",{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Reader","Class"],"MonadAsk"],"constraintArgs":[{"tag":"TypeVar","contents":"r"},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"r"}]}]},null]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["asks",[{"values":[{"sourceSpan":{"start":[18,1],"name":".spago/transformers/v5.1.0/src/Control/Comonad/Env/Class.purs","end":[18,69]},"score":8,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"asks","moduleName":"Control.Comonad.Env.Class","info":{"values":[{"type":{"tag":"ForAll","contents":["e1",{"tag":"ForAll","contents":["e2",{"tag":"ForAll","contents":["w",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Comonad","Env","Class"],"ComonadAsk"],"constraintArgs":[{"tag":"TypeVar","contents":"e1"},{"tag":"TypeVar","contents":"w"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"e1"}]},{"tag":"TypeVar","contents":"e2"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"w"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"e2"}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Get a value which depends on the environment.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[23,1],"name":".spago/transformers/v5.1.0/src/Control/Monad/Reader/Class.purs","end":[23,54]},"score":8,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"asks","moduleName":"Control.Monad.Reader.Class","info":{"values":[{"type":{"tag":"ForAll","contents":["r",{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Reader","Class"],"MonadAsk"],"constraintArgs":[{"tag":"TypeVar","contents":"r"},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeVar","contents":"a"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Projects a value from the global context in a `MonadAsk`.\n"}],"tag":"SearchResult"}]],["assoc",[{"values":[{"sourceSpan":{"start":[51,1],"name":".spago/arrays/v6.0.1/src/Data/Array/ST.purs","end":[51,44]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"Assoc","moduleName":"Data.Array.ST","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"RCons","contents":["value",{"tag":"TypeVar","contents":"a"},{"tag":"RCons","contents":["index",{"tag":"TypeConstructor","contents":[["Prim"],"Int"]},{"tag":"REmpty","contents":{}}]}]}]},"arguments":[["a",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":"An element and its index.\n"}],"tag":"SearchResult"}]],[">",[{"values":[{"sourceSpan":{"start":[156,1],"name":".spago/prelude/v5.0.1/src/Data/Ord.purs","end":[156,26]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"(>)","moduleName":"Data.Ord","info":{"values":[],"tag":"ValueAliasResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]]]