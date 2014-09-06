open String
open List

type jsonTokenType = 
  LCURLY
  | RCURLY
  | BOOL
  | STRING
  | LBRAK
  | RBRAK
  | NUMBER
  | DELIMITTER
  | COMMA

type jsonToken = {type: jsonTokenType; val: string }

let jsonTokenize jsonString =
  let tokenize partialString partialList =
    if length partialString == 0 then partialList
    else
      let currentChar = get partialString 0
      and rest = sub partialString (length partialString -1) in
      match currentChar with
       '{' -> tokenize rest (append partialList [{type: :LCURLY; val: "{"}])
       | '}' -> tokenize rest (append partialList [{type: RCURLY; val "}"}])

let jsonParse jsonString =
  let jsonTokenList = jsonTokenize(jsonString) in
  internalJsonParse jsonTokenList
