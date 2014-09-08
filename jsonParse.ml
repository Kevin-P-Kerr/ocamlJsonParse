open Printexc
open String
(* TODO: figure out how to not clobber library functions *)
let stringLength = length
open List

type parsedVal = {parsed:string; rest:string;}

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
  match c with
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

(* this is problematic TODO: make this more strict *)
let parseNumber s =
  let rec parse s p = 
    let currentChar = get s 0
    and rest = sub s 1 (stringLength s - 1) in
    if isNumber s or currentChar == '.' then parse rest (p^String.make 1 currentChar) else {rest=s;parsed=p}
  in
  parse s ""

let parseTrue s = 
  let truePart = sub s 0 4 and
  restPart = sub s 4 (stringLength s -4) in
  if truePart == "true" then restPart else invalid_arg "parseTrue"

let parseFalse s = 
  let truePart = sub s 0 4 and
  restPart = sub s 4 (stringLength s -4) in
  if truePart == "false" then restPart else invalid_arg "parseTrue"

let parseString s =
  let rest = sub s 1 (stringLength s-1)
  and currentChar = get s 0 in
  let rec parse s p = 
    if stringLength s == 0 then invalid_arg "bad string" else
    let currentChar = get s 0
    and rest = sub s 1 (stringLength s-1) in
    match currentChar with
    '"' -> {parsed=p;rest=rest}
    | _ -> parse rest (p^String.make 1 currentChar)
  in
  match currentChar with
  '"' -> parse rest ""
  | _ -> invalid_arg "bad string"

let jsonTokenize jsonString =
  let rec tokenize partialString partialList =
    if stringLength partialString == 0 then partialList
    else
      let currentChar = get partialString 0
      and rest = sub partialString 1 (stringLength partialString -1) in
      match currentChar with
       '{' -> tokenize rest (append partialList [{tokenType=LCURLY; tokenVal="{"}])
       | '}' -> tokenize rest (append partialList [{tokenType=RCURLY; tokenVal="}"}])
       | 't' -> tokenize (parseTrue partialString) (append partialList [{tokenType=BOOL; tokenVal="true"}])
       | 'f' -> tokenize (parseFalse partialString) (append partialList [{tokenType=BOOL; tokenVal="false"}])
       | '"' -> tokenize (parseString partialString).rest (append partialList [{tokenType=STRING; tokenVal=(parseString partialString).parsed}])
       | '[' -> tokenize rest (append partialList [{tokenType=LBRAK; tokenVal="["}])
       | ']' -> tokenize rest (append partialList [{tokenType=RBRAK; tokenVal="]"}])
       | ':' -> tokenize rest (append partialList [{tokenType=DELIMITTER; tokenVal=":"}])
       | ',' -> tokenize rest (append partialList [{tokenType=COMMA; tokenVal=","}])
       | ' ' -> tokenize rest partialList
       | _   -> if isNumber partialString then tokenize (parseNumber partialString).rest (append partialList [{tokenType=NUMBER; tokenVal=(parseNumber partialString).parsed}]) else invalid_arg "bad attempt to tokenize" in
        tokenize jsonString []
(* parsing functions *)
let validateTokenType token t = 
  if token.tokenType != t then false else true

let rec parseObject tokenList = 
  let currentToken = hd tokenList in
  if currentToken.tokenType != LCURLY 
    then invalid_arg "bad object" 
    else let truncatedList = parseMembers tokenList in
      let nextToken = hd truncatedList in
      if nextToken.tokenType != RCURLY then invalid_arg "unmatched curlies" else tl tokenList
and parseMembers tokenList =
  let truncatedList = parsePair tokenList in
  let token = hd truncatedList in
  if validateTokenType token COMMA then parseMembers (tl truncatedList) else truncatedList
and parsePair tokenList = 
  let currentToken = hd tokenList in
  if validateTokenType currentToken STRING 
    then 
    let nextToken = hd (tl tokenList) in
      if validateTokenType nextToken DELIMITTER then parseValue (tl (tl tokenList)) else invalid_arg "no delimitter in pair"
      else invalid_arg "pair does not lead with string"
and parseValue tokenList =
  let currentToken = hd tokenList in
  match currentToken.tokenType with
  STRING -> tl tokenList
  |NUMBER -> tl tokenList
  |BOOL   -> tl tokenList
  (* TODO: add null *)
  |LCURLY -> parseObject (tl tokenList)
  |LBRAK  -> parseArray (tl tokenList)
  |_      -> invalid_arg "failed to parse value"
and parseArray tokenList =  
  let tokenRest = parseElements  tokenList in
  if validateTokenType (hd tokenRest) RBRAK then tl tokenRest else invalid_arg "no rbrak in array"
and parseElements tokenList =
  let tokenRest = parseValue tokenList in
  if validateTokenType (hd tokenRest) COMMA then parseElements (tl tokenRest) else tokenRest

let jsonParse jsonString =
  let internalJsonParse tokenList =
    if length tokenList == 0 then tokenList else parseObject tokenList
  in
  let jsonTokenList = jsonTokenize(jsonString) in
  let finalList = internalJsonParse jsonTokenList in
  if length finalList == 0 then true else false

let x = jsonParse "{\"foo\":2}"
