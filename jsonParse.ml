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

let isNumber s = 
  let c = get s 0 in
  match s with
  '0' -> true
 | '1' -> true
 | '2' -> true
 | '3' -> true
 | '4' -> true
 | '5' -> true
 | '6' -> true
 | '7' -> true
 | '8' -> true
 | '9' -> true
 | 'e' -> true
 false

let jsonTokenize jsonString =
  let tokenize partialString partialList =
    if length partialString == 0 then partialList
    else
      let currentChar = get partialString 0
      and rest = sub partialString (length partialString -1) in
      match currentChar with
       '{' -> tokenize rest (append partialList [{type: :LCURLY; val: "{"}])
       | '}' -> tokenize rest (append partialList [{type: RCURLY; val: "}"}])
       | 't' -> tokenize (parseTrue partialString) (append partialList [{type: BOOL; val: "true"}])
       | 'f' -> tokenize (parseFalse partialString) (append partialList [{type: BOOL; val: "false"}])
       | '"' -> tokenize (parseString partialString) (append partialList [{type: STRING; val: (parseString partialString)}])
       | '[' -> tokenize rest (append partiallist [{type: LBRAK; val: "["}])
       | ']' -> tokenize rest (append partiallist [{type: RBRAK; val: "]"}])
       | ':' -> tokenize rest (append partialList [{type: DELIMITTER; val: ":"}])
       | ',' -> tokenize rest (append partialList [{type: COMMA; val: ","}])
       if isNumber partialString then tokenize (parseNumber partialString) (append partialList [{type: NUMBER; val: parseNumber partialString}])
        

let jsonParse jsonString =
  let jsonTokenList = jsonTokenize(jsonString) in
  internalJsonParse jsonTokenList
