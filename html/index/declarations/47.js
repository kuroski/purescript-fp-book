// This file was generated by purescript-docs-search.
window.DocsSearchIndex["47"] = [["date",[{"values":[{"sourceSpan":{"start":[27,1],"name":".spago/datetime/v5.0.2/src/Data/Date.purs","end":[27,32]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"Date","moduleName":"Data.Date","info":{"values":[{"typeArguments":[],"dataDeclType":"data"}],"tag":"DataResult"},"hashAnchor":"t","comments":"A date value in the Gregorian calendar.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[37,1],"name":".spago/datetime/v5.0.2/src/Data/DateTime.purs","end":[37,25]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"date","moduleName":"Data.DateTime","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","DateTime"],"DateTime"]}]},{"tag":"TypeConstructor","contents":[["Data","Date"],"Date"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["datetime",[{"values":[{"sourceSpan":{"start":[25,1],"name":".spago/datetime/v5.0.2/src/Data/DateTime.purs","end":[25,35]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"DateTime","moduleName":"Data.DateTime","info":{"values":[{"typeArguments":[],"dataDeclType":"data"}],"tag":"DataResult"},"hashAnchor":"t","comments":"A date/time value in the Gregorian calendar/UTC time zone.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"DateTime","moduleName":"Data.DateTime","info":{"values":[{"arguments":[{"tag":"TypeConstructor","contents":[["Data","Date"],"Date"]},{"tag":"TypeConstructor","contents":[["Data","Time"],"Time"]}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["day",[{"values":[{"sourceSpan":{"start":[120,1],"name":".spago/datetime/v5.0.2/src/Data/Date/Component.purs","end":[120,22]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"Day","moduleName":"Data.Date.Component","info":{"values":[{"typeArguments":[],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"A day component for a date.\n\nThe constructor is private as the `Day` type is bounded to the range\n1 to 31, inclusive. The `toEnum` function can be used to safely\nacquire a day value from an integer.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[83,1],"name":".spago/datetime/v5.0.2/src/Data/Date.purs","end":[83,19]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"day","moduleName":"Data.Date","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Date"],"Date"]}]},{"tag":"TypeConstructor","contents":[["Data","Date","Component"],"Day"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The day component of a date value.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"Day","moduleName":"Data.Interval.Duration","info":{"values":[{"arguments":[]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[57,1],"name":".spago/datetime/v5.0.2/src/Data/Interval/Duration.purs","end":[57,26]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"day","moduleName":"Data.Interval.Duration","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Number"]}]},{"tag":"TypeConstructor","contents":[["Data","Interval","Duration"],"Duration"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["days",[{"values":[{"sourceSpan":{"start":[72,1],"name":".spago/datetime/v5.0.2/src/Data/Time/Duration.purs","end":[72,27]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"Days","moduleName":"Data.Time.Duration","info":{"values":[{"typeArguments":[],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"A duration measured in days, where a day is assumed to be exactly 24 hours.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"Days","moduleName":"Data.Time.Duration","info":{"values":[{"arguments":[{"tag":"TypeConstructor","contents":[["Prim"],"Number"]}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["add",[{"values":[{"sourceSpan":{"start":[35,3],"name":".spago/prelude/v5.0.1/src/Data/Semiring.purs","end":[35,22]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"add","moduleName":"Data.Semiring","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Data","Semiring"],"Semiring"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semiring"],"Semiring"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["additive",[{"values":[{"sourceSpan":{"start":[14,1],"name":".spago/prelude/v5.0.1/src/Data/Monoid/Additive.purs","end":[14,32]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"Additive","moduleName":"Data.Monoid.Additive","info":{"values":[{"typeArguments":[["a",null]],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"Monoid and semigroup for semirings under addition.\n\n``` purescript\nAdditive x <> Additive y == Additive (x + y)\n(mempty :: Additive _) == Additive zero\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"Additive","moduleName":"Data.Monoid.Additive","info":{"values":[{"arguments":[{"tag":"TypeVar","contents":"a"}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["addrecord",[{"values":[{"sourceSpan":{"start":[100,3],"name":".spago/prelude/v5.0.1/src/Data/Semiring.purs","end":[100,92]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"addRecord","moduleName":"Data.Semiring","info":{"values":[{"typeClassArguments":[["rowlist",null],["row",null],["subrow",null]],"typeClass":[["Data","Semiring"],"SemiringRecord"],"type":{"tag":"ForAll","contents":["rowlist",{"tag":"ForAll","contents":["row",{"tag":"ForAll","contents":["subrow",{"tag":"ForAll","contents":["rlproxy",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semiring"],"SemiringRecord"],"constraintArgs":[{"tag":"TypeVar","contents":"rowlist"},{"tag":"TypeVar","contents":"row"},{"tag":"TypeVar","contents":"subrow"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"rlproxy"},{"tag":"TypeVar","contents":"rowlist"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"TypeVar","contents":"row"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"TypeVar","contents":"row"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"TypeVar","contents":"subrow"}]}]}]}]}]},null]},null]},null]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["address",[{"values":[{"sourceSpan":{"start":[7,1],"name":"src/Ch6.purs","end":[13,6]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"Address","moduleName":"Ch6","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"RCons","contents":["street1",{"tag":"TypeConstructor","contents":[["Prim"],"String"]},{"tag":"RCons","contents":["street2",{"tag":"TypeConstructor","contents":[["Prim"],"String"]},{"tag":"RCons","contents":["city",{"tag":"TypeConstructor","contents":[["Prim"],"String"]},{"tag":"RCons","contents":["state",{"tag":"TypeConstructor","contents":[["Prim"],"String"]},{"tag":"RCons","contents":["zip",{"tag":"TypeConstructor","contents":[["Prim"],"String"]},{"tag":"REmpty","contents":{}}]}]}]}]}]}]},"arguments":[]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"}]],["adjust",[{"values":[{"sourceSpan":{"start":[95,1],"name":".spago/datetime/v5.0.2/src/Data/Date.purs","end":[95,37]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"adjust","moduleName":"Data.Date","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Time","Duration"],"Days"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Date"],"Date"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","Date"],"Date"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Adjusts a date with a Duration in days. The number of days must\nalready be an integer and fall within the valid range of values\nfor the Int type.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[57,1],"name":".spago/datetime/v5.0.2/src/Data/DateTime.purs","end":[57,66]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"adjust","moduleName":"Data.DateTime","info":{"values":[{"type":{"tag":"ForAll","contents":["d",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Time","Duration"],"Duration"],"constraintArgs":[{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","DateTime"],"DateTime"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","DateTime"],"DateTime"]}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Adjusts a date/time value with a duration offset. `Nothing` is returned\nif the resulting date would be outside of the range of valid dates.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[74,1],"name":".spago/datetime/v5.0.2/src/Data/Time.purs","end":[74,63]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"adjust","moduleName":"Data.Time","info":{"values":[{"type":{"tag":"ForAll","contents":["d",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Time","Duration"],"Duration"],"constraintArgs":[{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Time"],"Time"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple"],"Tuple"]},{"tag":"TypeConstructor","contents":[["Data","Time","Duration"],"Days"]}]},{"tag":"TypeConstructor","contents":[["Data","Time"],"Time"]}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Adjusts a time value with a duration offset. The result includes a\nremainder value of the whole number of days involved in the adjustment,\nfor example, if a time of 23:00:00:00 has a duration of +2 hours added to\nit, the result will be 1 day, and 01:00:00:00. Correspondingly, if the\nduration is negative, a negative number of days may also be returned as\nthe remainder.\n"}],"tag":"SearchResult"}]],[">#<",[{"values":[{"sourceSpan":{"start":[25,1],"name":".spago/contravariant/v5.0.0/src/Data/Functor/Contravariant.purs","end":[25,28]},"score":1,"packageInfo":{"values":["contravariant"],"tag":"Package"},"name":"(>#<)","moduleName":"Data.Functor.Contravariant","info":{"values":[],"tag":"ValueAliasResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["/",[{"values":[{"sourceSpan":{"start":[68,1],"name":".spago/prelude/v5.0.1/src/Data/EuclideanRing.purs","end":[68,18]},"score":40,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"(/)","moduleName":"Data.EuclideanRing","info":{"values":[],"tag":"ValueAliasResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]]]