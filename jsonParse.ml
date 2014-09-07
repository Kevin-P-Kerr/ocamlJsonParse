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

type jsonToken = {tokenType: jsonTokenType; tokenVal: string; }

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
 | _   -> false

let parseNumber parseString =
  let currentChar = get parseString 0
  and rest = sub parseString (length parseString-1) in
  if isNumber parseString or currentChar == '.' then parseNumber rest else parseString

let parseString s =
  let rest = sub parseString (length s-1) and
  restOfrest = sub rest (length rest -1) in
  if length s == 0 then invalid_arg else
    let currentChar = get rest 0 in
    match currentChar with
    '"' -> restOfrest
    | _ -> parseString restOfrest

let jsonTokenize jsonString =
  let tokenize partialString partialList =
    if length partialString == 0 then partialList
    else
      let currentChar = get partialString 0
      and rest = sub partialString (length partialString -1) in
      match currentChar with
       '{' -> tokenize rest (append partialList [{tokenType=LCURLY; tokenVal="{"}])
       | '}' -> tokenize rest (append partialList [{tokenType=RCURLY; =tokenVal: "}"}])
       | 't' -> tokenize (parseTrue partialString) (append partialList [{tokenType=BOOL; tokenVal="true"}])
       | 'f' -> tokenize (parseFalse partialString) (append partialList [{tokenType=BOOL; tokenVal="false"}])
       | '"' -> tokenize (parseString partialString) (append partialList [{tokenType=STRING; tokenVal=(parseString partialString)}])
       | '[' -> tokenize rest (append partiallist [{tokenType=LBRAK; tokenVal="["}])
       | ']' -> tokenize rest (append partiallist [{tokenType=RBRAK; tokenVal="]"}])
       | ':' -> tokenize rest (append partialList [{tokenType=DELIMITTER; tokenVal=":"}])
       | ',' -> tokenize rest (append partialList [{tokenType=COMMA; tokenVal=","}])
       | _ -> if isNumber partialString then tokenize (parseNumber partialString) (append partialList [{tokenType=NUMBER; tokenVal=(parseNumber partialString)}]) else invalid_arg
        

let jsonParse jsonString =
  (* let jsonTokenList = jsonTokenize(jsonString) in
  internalJsonParse jsonTokenList *)
  jsonTokenize jsonString
