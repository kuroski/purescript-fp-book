// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["63437464"] = [{"values":[{"sourceSpan":{"start":[181,1],"name":".spago/pipes/v7.0.1/src/Pipes/Internal.purs","end":[181,27]},"score":1,"packageInfo":{"values":["pipes"],"tag":"Package"},"name":"closed","moduleName":"Pipes.Internal","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Pipes","Internal"],"X"]}]},{"tag":"TypeVar","contents":"a"}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[12,1],"name":".spago/partial/v3.0.0/src/Partial.purs","end":[12,46]},"score":11,"packageInfo":{"values":["partial"],"tag":"Package"},"name":"crashWith","moduleName":"Partial","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Prim"],"Partial"],"constraintArgs":[]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeVar","contents":"a"}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"A partial function which crashes on any input with the specified message.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[23,1],"name":".spago/partial/v3.0.0/src/Partial/Unsafe.purs","end":[23,41]},"score":11,"packageInfo":{"values":["partial"],"tag":"Package"},"name":"unsafeCrashWith","moduleName":"Partial.Unsafe","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeVar","contents":"a"}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"A function which crashes with the specified error message.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[19,1],"name":".spago/partial/v3.0.0/src/Partial/Unsafe.purs","end":[19,47]},"score":11,"packageInfo":{"values":["partial"],"tag":"Package"},"name":"unsafePartial","moduleName":"Partial.Unsafe","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"ConstrainedType","contents":[{"constraintClass":[["Prim"],"Partial"],"constraintArgs":[]},{"tag":"TypeVar","contents":"a"}]}}]},{"tag":"TypeVar","contents":"a"}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Discharge a partiality constraint, unsafely.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[13,1],"name":".spago/exceptions/v5.0.0/src/Effect/Exception/Unsafe.purs","end":[13,37]},"score":4,"packageInfo":{"values":["exceptions"],"tag":"Package"},"name":"unsafeThrow","moduleName":"Effect.Exception.Unsafe","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeVar","contents":"a"}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Defined as `unsafeThrowException <<< error`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[9,1],"name":".spago/exceptions/v5.0.0/src/Effect/Exception/Unsafe.purs","end":[9,45]},"score":4,"packageInfo":{"values":["exceptions"],"tag":"Package"},"name":"unsafeThrowException","moduleName":"Effect.Exception.Unsafe","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]}]},{"tag":"TypeVar","contents":"a"}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Throw an exception in pure code. This function should be used very\nsparingly, as it can cause unexpected crashes at runtime.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[36,1],"name":".spago/prelude/v5.0.1/src/Data/Void.purs","end":[36,30]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"absurd","moduleName":"Data.Void","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Void"],"Void"]}]},{"tag":"TypeVar","contents":"a"}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Eliminator for the `Void` type.\nUseful for stating that some code branch is impossible because you've\n\"acquired\" a value of type `Void` (which you can't).\n\n```purescript\nrightOnly :: forall t . Either Void t -> t\nrightOnly (Left v) = absurd v\nrightOnly (Right t) = t\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[98,1],"name":".spago/datetime/v5.0.2/src/Data/Time/Duration.purs","end":[98,49]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"negateDuration","moduleName":"Data.Time.Duration","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Time","Duration"],"Duration"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Negates a duration, turning a positive duration negative or a negative\nduration positive.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[90,3],"name":".spago/datetime/v5.0.2/src/Data/Time/Duration.purs","end":[90,34]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"toDuration","moduleName":"Data.Time.Duration","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Data","Time","Duration"],"Duration"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Time","Duration"],"Duration"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Time","Duration"],"Milliseconds"]}]},{"tag":"TypeVar","contents":"a"}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[89,3],"name":".spago/datetime/v5.0.2/src/Data/Time/Duration.purs","end":[89,36]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"fromDuration","moduleName":"Data.Time.Duration","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Data","Time","Duration"],"Duration"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Time","Duration"],"Duration"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Time","Duration"],"Milliseconds"]}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[18,3],"name":".spago/prelude/v5.0.1/src/Data/Show.purs","end":[18,22]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"show","moduleName":"Data.Show","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Data","Show"],"Show"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Show"],"Show"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[15,3],"name":".spago/prelude/v5.0.1/src/Data/Show/Generic.purs","end":[15,30]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"genericShow'","moduleName":"Data.Show.Generic","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Data","Show","Generic"],"GenericShow"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Show","Generic"],"GenericShow"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[53,1],"name":".spago/prelude/v5.0.1/src/Data/Ring.purs","end":[53,37]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"negate","moduleName":"Data.Ring","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ring"],"Ring"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"`negate x` can be used as a shorthand for `zero - x`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[216,1],"name":".spago/prelude/v5.0.1/src/Data/Ord.purs","end":[216,46]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"signum","moduleName":"Data.Ord","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ring"],"Ring"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The sign function; always evaluates to either `one` or `negate one`. For\nany `x`, we should have `signum x * abs x == x`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[211,1],"name":".spago/prelude/v5.0.1/src/Data/Ord.purs","end":[211,43]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"abs","moduleName":"Data.Ord","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ring"],"Ring"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The absolute value function. `abs x` is defined as `if x >= zero then x\nelse negate x`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[44,3],"name":".spago/prelude/v5.0.1/src/Data/HeytingAlgebra.purs","end":[44,16]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"not","moduleName":"Data.HeytingAlgebra","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Data","HeytingAlgebra"],"HeytingAlgebra"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","HeytingAlgebra"],"HeytingAlgebra"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[14,3],"name":".spago/prelude/v5.0.1/src/Data/HeytingAlgebra/Generic.purs","end":[14,24]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"genericNot'","moduleName":"Data.HeytingAlgebra.Generic","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Data","HeytingAlgebra","Generic"],"GenericHeytingAlgebra"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","HeytingAlgebra","Generic"],"GenericHeytingAlgebra"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[64,3],"name":".spago/prelude/v5.0.1/src/Data/EuclideanRing.purs","end":[64,21]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"degree","moduleName":"Data.EuclideanRing","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Data","EuclideanRing"],"EuclideanRing"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","EuclideanRing"],"EuclideanRing"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[306,1],"name":".spago/enums/v5.0.0/src/Data/Enum.purs","end":[306,48]},"score":2,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"defaultFromEnum","moduleName":"Data.Enum","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Enum"],"Enum"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Provides a default implementation for `fromEnum`.\n\n- Assumes `toEnum 0 = Just bottom`.\n- Cannot be used in conjuction with `defaultPred`.\n\nRuns in `O(n)` where `n` is `fromEnum a`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[116,3],"name":".spago/enums/v5.0.0/src/Data/Enum.purs","end":[116,23]},"score":2,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"fromEnum","moduleName":"Data.Enum","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Data","Enum"],"BoundedEnum"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Enum"],"BoundedEnum"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[59,3],"name":".spago/enums/v5.0.0/src/Data/Enum/Generic.purs","end":[59,31]},"score":2,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"genericFromEnum'","moduleName":"Data.Enum.Generic","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Data","Enum","Generic"],"GenericBoundedEnum"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Enum","Generic"],"GenericBoundedEnum"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[30,3],"name":".spago/prelude/v5.0.1/src/Data/DivisionRing.purs","end":[30,18]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"recip","moduleName":"Data.DivisionRing","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Data","DivisionRing"],"DivisionRing"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","DivisionRing"],"DivisionRing"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[122,1],"name":"src/Ch6.purs","end":[122,54]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"getDirections","moduleName":"Ch6","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Ch6"],"HasAddress"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Ch6"],"Directions"]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[11,3],"name":"src/Ch6.purs","end":[11,29]},"score":200000,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"getAddress","moduleName":"Ch6","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Ch6"],"HasAddress"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Ch6"],"HasAddress"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Ch6"],"Address"]}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]