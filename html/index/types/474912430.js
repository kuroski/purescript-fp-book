// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["474912430"] = [{"values":[{"sourceSpan":{"start":[178,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodeUnits.purs","end":[178,62]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"lastIndexOf'","moduleName":"Data.String.NonEmpty.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the index of the last occurrence of the pattern in the\ngiven string, starting at the specified index and searching\nbackwards towards the beginning of the string.\n\nStarting at a negative index is equivalent to starting at 0 and\nstarting at an index greater than the string length is equivalent\nto searching in the whole string.\n\nReturns `Nothing` if there is no match.\n\n```purescript\nlastIndexOf' (Pattern \"a\") (-1) (NonEmptyString \"ababa\") == Just 0\nlastIndexOf' (Pattern \"a\") 1 (NonEmptyString \"ababa\") == Just 0\nlastIndexOf' (Pattern \"a\") 3 (NonEmptyString \"ababa\") == Just 2\nlastIndexOf' (Pattern \"a\") 4 (NonEmptyString \"ababa\") == Just 4\nlastIndexOf' (Pattern \"a\") 5 (NonEmptyString \"ababa\") == Just 4\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[148,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodeUnits.purs","end":[148,58]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"indexOf'","moduleName":"Data.String.NonEmpty.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the index of the first occurrence of the pattern in the\ngiven string, starting at the specified index. Returns `Nothing` if there is\nno match.\n\n```purescript\nindexOf' (Pattern \"a\") 2 (NonEmptyString \"ababa\") == Just 2\nindexOf' (Pattern \"a\") 3 (NonEmptyString \"ababa\") == Just 4\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[90,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodePoints.purs","end":[90,62]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"lastIndexOf'","moduleName":"Data.String.NonEmpty.CodePoints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[84,1],"name":".spago/strings/v5.0.0/src/Data/String/NonEmpty/CodePoints.purs","end":[84,58]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"indexOf'","moduleName":"Data.String.NonEmpty.CodePoints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[311,1],"name":".spago/strings/v5.0.0/src/Data/String/CodeUnits.purs","end":[311,46]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"slice","moduleName":"Data.String.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the substring at indices `[begin, end)`.\nIf either index is negative, it is normalised to `length s - index`,\nwhere `s` is the input string. `Nothing` is returned if either\nindex is out of bounds or if `begin > end` after normalisation.\n\n```purescript\nslice 0 0   \"purescript\" == Just \"\"\nslice 0 1   \"purescript\" == Just \"p\"\nslice 3 6   \"purescript\" == Just \"esc\"\nslice (-4) (-1) \"purescript\" == Just \"rip\"\nslice (-4) 3  \"purescript\" == Nothing\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[235,1],"name":".spago/strings/v5.0.0/src/Data/String/CodeUnits.purs","end":[235,54]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"lastIndexOf'","moduleName":"Data.String.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the index of the last occurrence of the pattern in the\ngiven string, starting at the specified index and searching\nbackwards towards the beginning of the string.\n\nStarting at a negative index is equivalent to starting at 0 and\nstarting at an index greater than the string length is equivalent\nto searching in the whole string.\n\nReturns `Nothing` if there is no match.\n\n```purescript\nlastIndexOf' (Pattern \"a\") (-1) \"ababa\" == Just 0\nlastIndexOf' (Pattern \"a\") 1 \"ababa\" == Just 0\nlastIndexOf' (Pattern \"a\") 3 \"ababa\" == Just 2\nlastIndexOf' (Pattern \"a\") 4 \"ababa\" == Just 4\nlastIndexOf' (Pattern \"a\") 5 \"ababa\" == Just 4\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[188,1],"name":".spago/strings/v5.0.0/src/Data/String/CodeUnits.purs","end":[188,50]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"indexOf'","moduleName":"Data.String.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the index of the first occurrence of the pattern in the\ngiven string, starting at the specified index. Returns `Nothing` if there is\nno match.\n\n```purescript\nindexOf' (Pattern \"a\") 2 \"ababa\" == Just 2\nindexOf' (Pattern \"a\") 3 \"ababa\" == Just 4\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[310,1],"name":".spago/strings/v5.0.0/src/Data/String/CodePoints.purs","end":[310,54]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"lastIndexOf'","moduleName":"Data.String.CodePoints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the number of code points preceding the first match of the given\npattern in the string. Pattern matches following the given index will be\nignored.\n\nGiving a negative index is equivalent to giving 0 and giving an index\ngreater than the number of code points in the string is equivalent to\nsearching in the whole string.\n\nReturns Nothing when no matches are found.\n\n```purescript\n>>> lastIndexOf' (Pattern \"𝐀\") (-1) \"b 𝐀𝐀 c 𝐀\"\nNothing\n>>> lastIndexOf' (Pattern \"𝐀\") 0 \"b 𝐀𝐀 c 𝐀\"\nNothing\n>>> lastIndexOf' (Pattern \"𝐀\") 5 \"b 𝐀𝐀 c 𝐀\"\nJust 3\n>>> lastIndexOf' (Pattern \"𝐀\") 8 \"b 𝐀𝐀 c 𝐀\"\nJust 7\n>>> lastIndexOf' (Pattern \"o\") 5 \"b 𝐀𝐀 c 𝐀\"\nNothing\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[269,1],"name":".spago/strings/v5.0.0/src/Data/String/CodePoints.purs","end":[269,50]},"score":3,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"indexOf'","moduleName":"Data.String.CodePoints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Pattern"],"Pattern"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the number of code points preceding the first match of the given\npattern in the string. Pattern matches preceding the given index will be\nignored. Returns Nothing when no matches are found.\n\n```purescript\n>>> indexOf' (Pattern \"𝐀\") 4 \"b 𝐀𝐀 c 𝐀\"\nJust 7\n>>> indexOf' (Pattern \"o\") 4 \"b 𝐀𝐀 c 𝐀\"\nNothing\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[41,1],"name":".spago/datetime/v5.0.2/src/Data/Date.purs","end":[41,48]},"score":2,"packageInfo":{"values":["datetime"],"tag":"Package"},"name":"exactDate","moduleName":"Data.Date","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Date","Component"],"Year"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Date","Component"],"Month"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Date","Component"],"Day"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","Date"],"Date"]}]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Constructs a date from year, month, and day components. The result will be\n`Nothing` if the provided values result in an invalid date.\n"}],"tag":"SearchResult"}]