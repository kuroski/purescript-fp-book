// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["2064303202"] = [{"values":[{"sourceSpan":{"start":[131,1],"name":".spago/strings/v5.0.0/src/Data/String/Regex.purs","end":[131,56]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"split","moduleName":"Data.String.Regex","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Regex"],"Regex"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Split the string into an array of substrings along occurrences of the `Regex`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[127,1],"name":".spago/strings/v5.0.0/src/Data/String/Regex.purs","end":[127,39]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"search","moduleName":"Data.String.Regex","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Regex"],"Regex"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns `Just` the index of the first match of the `Regex` in the string,\nor `Nothing` if there is no match.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[123,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/Internal.purs","end":[123,65]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"stripSuffix","moduleName":"Data.String.NonEmpty.Internal","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"If the string ends with the given suffix, return the portion of the\nstring left after removing it. If the suffix does not match or there is no\nremainder, the result will be `Nothing`.\n\n```purescript\nstripSuffix (Pattern \".exe\") (NonEmptyString \"purs.exe\") == Just (NonEmptyString \"purs\")\nstripSuffix (Pattern \".exe\") (NonEmptyString \"purs\") == Nothing\nstripSuffix (Pattern \"Hello!\") (NonEmptyString \"Hello!\") == Nothing\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[111,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/Internal.purs","end":[111,65]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"stripPrefix","moduleName":"Data.String.NonEmpty.Internal","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"If the string starts with the given prefix, return the portion of the\nstring left after removing it. If the prefix does not match or there is no\nremainder, the result will be `Nothing`.\n\n```purescript\nstripPrefix (Pattern \"http:\") (NonEmptyString \"http://purescript.org\") == Just (NonEmptyString \"//purescript.org\")\nstripPrefix (Pattern \"http:\") (NonEmptyString \"https://purescript.org\") == Nothing\nstripPrefix (Pattern \"Hello!\") (NonEmptyString \"Hello!\") == Nothing\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[269,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodeUnits.purs","end":[269,59]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"dropRight","moduleName":"Data.String.NonEmpty.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the string without the last `n` characters. Returns `Nothing` if\nmore characters are dropped than the string is long.\n\n```purescript\ndropRight 6 (NonEmptyString \"Hello World\") == Just (NonEmptyString \"Hello\")\ndropRight 20 (NonEmptyString \"Hello World\") == Nothing\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[253,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodeUnits.purs","end":[253,54]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"drop","moduleName":"Data.String.NonEmpty.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the string without the first `n` characters. Returns `Nothing` if\nmore characters are dropped than the string is long.\n\n```purescript\ndrop 6 (NonEmptyString \"Hello World\") == Just (NonEmptyString \"World\")\ndrop 20 (NonEmptyString \"Hello World\") == Nothing\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[227,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodeUnits.purs","end":[227,59]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"takeRight","moduleName":"Data.String.NonEmpty.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the last `n` characters of the string. Returns `Nothing` if `n` is\nless than 1.\n\n```purescript\ntake 5 (NonEmptyString \"Hello World\") == Just (NonEmptyString \"World\")\ntake 0 (NonEmptyString \"Hello World\") == Nothing\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[211,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodeUnits.purs","end":[211,54]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"take","moduleName":"Data.String.NonEmpty.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the first `n` characters of the string. Returns `Nothing` if `n` is\nless than 1.\n\n```purescript\ntake 5 (NonEmptyString \"Hello World\") == Just (NonEmptyString \"Hello\")\ntake 0 (NonEmptyString \"Hello World\") == Nothing\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[158,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodeUnits.purs","end":[158,54]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"lastIndexOf","moduleName":"Data.String.NonEmpty.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the index of the last occurrence of the pattern in the\ngiven string. Returns `Nothing` if there is no match.\n\n```purescript\nlastIndexOf (Pattern \"c\") (NonEmptyString \"abcdc\") == Just 4\nlastIndexOf (Pattern \"c\") (NonEmptyString \"aaa\") == Nothing\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[137,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodeUnits.purs","end":[137,50]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"indexOf","moduleName":"Data.String.NonEmpty.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the index of the first occurrence of the pattern in the\ngiven string. Returns `Nothing` if there is no match.\n\n```purescript\nindexOf (Pattern \"c\") (NonEmptyString \"abcdc\") == Just 2\nindexOf (Pattern \"c\") (NonEmptyString \"aaa\") == Nothing\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[117,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodeUnits.purs","end":[117,46]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"charAt","moduleName":"Data.String.NonEmpty.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Char"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the character at the given index, if the index is within bounds.\n\n```purescript\ncharAt 2 (NonEmptyString \"Hello\") == Just 'l'\ncharAt 10 (NonEmptyString \"Hello\") == Nothing\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[117,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodePoints.purs","end":[117,54]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"drop","moduleName":"Data.String.NonEmpty.CodePoints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[105,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodePoints.purs","end":[105,54]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"take","moduleName":"Data.String.NonEmpty.CodePoints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[87,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodePoints.purs","end":[87,54]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"lastIndexOf","moduleName":"Data.String.NonEmpty.CodePoints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[81,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodePoints.purs","end":[81,50]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"indexOf","moduleName":"Data.String.NonEmpty.CodePoints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[78,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodePoints.purs","end":[78,56]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"codePointAt","moduleName":"Data.String.NonEmpty.CodePoints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","String","CodePoints"],"CodePoint"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[65,1],"name":".spago/strings/v5.0.0/src/Data/String/Common.purs","end":[65,58]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"split","moduleName":"Data.String.Common","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the substrings of the second string separated along occurences\nof the first string.\n\n```purescript\nsplit (Pattern \" \") \"hello world\" == [\"hello\", \"world\"]\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[207,1],"name":".spago/strings/v5.0.0/src/Data/String/CodeUnits.purs","end":[207,46]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"lastIndexOf","moduleName":"Data.String.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the index of the last occurrence of the pattern in the\ngiven string. Returns `Nothing` if there is no match.\n\n```purescript\nlastIndexOf (Pattern \"c\") \"abcdc\" == Just 4\nlastIndexOf (Pattern \"c\") \"aaa\" == Nothing\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[169,1],"name":".spago/strings/v5.0.0/src/Data/String/CodeUnits.purs","end":[169,42]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"indexOf","moduleName":"Data.String.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the index of the first occurrence of the pattern in the\ngiven string. Returns `Nothing` if there is no match.\n\n```purescript\nindexOf (Pattern \"c\") \"abcdc\" == Just 2\nindexOf (Pattern \"c\") \"aaa\" == Nothing\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[106,1],"name":".spago/strings/v5.0.0/src/Data/String/CodeUnits.purs","end":[106,38]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"charAt","moduleName":"Data.String.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Char"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the character at the given index, if the index is within bounds.\n\n```purescript\ncharAt 2 \"Hello\" == Just 'l'\ncharAt 10 \"Hello\" == Nothing\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[59,1],"name":".spago/strings/v5.0.0/src/Data/String/CodeUnits.purs","end":[59,49]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"stripSuffix","moduleName":"Data.String.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"If the string ends with the given suffix, return the portion of the\nstring left after removing it, as a `Just` value. Otherwise, return\n`Nothing`.\n\n```purescript\nstripSuffix (Pattern \".exe\") \"psc.exe\" == Just \"psc\"\nstripSuffix (Pattern \".exe\") \"psc\" == Nothing\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[46,1],"name":".spago/strings/v5.0.0/src/Data/String/CodeUnits.purs","end":[46,49]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"stripPrefix","moduleName":"Data.String.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"If the string starts with the given prefix, return the portion of the\nstring left after removing it, as a Just value. Otherwise, return Nothing.\n\n```purescript\nstripPrefix (Pattern \"http:\") \"http://purescript.org\" == Just \"//purescript.org\"\nstripPrefix (Pattern \"http:\") \"https://purescript.org\" == Nothing\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[284,1],"name":".spago/strings/v5.0.0/src/Data/String/CodePoints.purs","end":[284,46]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"lastIndexOf","moduleName":"Data.String.CodePoints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the number of code points preceding the last match of the given\npattern in the string. Returns Nothing when no matches are found.\n\n```purescript\n>>> lastIndexOf (Pattern \"𝐀\") \"b 𝐀𝐀 c 𝐀\"\nJust 7\n>>> lastIndexOf (Pattern \"o\") \"b 𝐀𝐀 c 𝐀\"\nNothing\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[255,1],"name":".spago/strings/v5.0.0/src/Data/String/CodePoints.purs","end":[255,42]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"indexOf","moduleName":"Data.String.CodePoints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the number of code points preceding the first match of the given\npattern in the string. Returns Nothing when no matches are found.\n\n```purescript\n>>> indexOf (Pattern \"𝐀\") \"b 𝐀𝐀 c 𝐀\"\nJust 2\n>>> indexOf (Pattern \"o\") \"b 𝐀𝐀 c 𝐀\"\nNothing\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[160,1],"name":".spago/strings/v5.0.0/src/Data/String/CodePoints.purs","end":[160,48]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"codePointAt","moduleName":"Data.String.CodePoints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","String","CodePoints"],"CodePoint"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the first code point of the string after dropping the given number\nof code points from the beginning, if there is such a code point. Operates\nin constant space and in time linear to the given index.\n\n```purescript\n>>> codePointAt 1 \"𝐀𝐀𝐀𝐀\"\nJust (CodePoint 0x1D400) -- represents \"𝐀\"\n-- compare to Data.String:\n>>> charAt 1 \"𝐀𝐀𝐀𝐀\"\nJust '�'\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[148,1],"name":".spago/lists/v6.0.1/src/Data/List.purs","end":[148,32]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"range","moduleName":"Data.List","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create a list containing a range of integers, including both endpoints.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[148,1],"name":".spago/lists/v6.0.1/src/Data/List/Lazy.purs","end":[148,32]},"score":5,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"range","moduleName":"Data.List.Lazy","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Lazy","Types"],"List"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create a list containing a range of integers, including both endpoints.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[203,1],"name":".spago/integers/v5.0.0/src/Data/Int.purs","end":[203,45]},"score":3,"packageInfo":{"values":["integers"],"tag":"Package"},"name":"fromStringAs","moduleName":"Data.Int","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Int"],"Radix"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Like `fromString`, but the integer can be specified in a different base.\n\nExample:\n``` purs\nfromStringAs binary      \"100\" == Just 4\nfromStringAs hexadecimal \"ff\"  == Just 255\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[95,1],"name":".spago/datetime/v5.0.2/src/Data/Date.purs","end":[95,37]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"adjust","moduleName":"Data.Date","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Time","Duration"],"Days"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Date"],"Date"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","Date"],"Date"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Adjusts a date with a Duration in days. The number of days must\nalready be an integer and fall within the valid range of values\nfor the Int type.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[189,1],"name":".spago/arrays/v6.0.1/src/Data/Array.purs","end":[189,48]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"range","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create an array containing a range of integers, including both endpoints.\n```purescript\nrange 2 5 = [2, 3, 4, 5]\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[199,1],"name":".spago/arrays/v6.0.1/src/Data/Array/NonEmpty.purs","end":[199,41]},"score":5,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"range","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[271,1],"name":"src/Ch5.purs","end":[271,32]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"range","moduleName":"Ch5","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create a list containing a range of integers, including both endpoints.\n\n```purescript\nrange 1 5 = (1 : 2 : 3 : 4 : 5 : Nil)\nrange 3 (-3) = (3 : 2 : 1 : 0 : -1 : -2 : -3 : Nil)\n```\n"}],"tag":"SearchResult"}]