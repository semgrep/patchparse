(* IMPORTANT! 
 * An expression cannot follow another expression.  
 * A symbol cannot follow another symbol. 
 *)
(* not fault tolerant *)

%{
open Parse_error
open Ast0

let parse_error _ =
  display_error "Syntax error"
    (Parsing.symbol_start ()) (Parsing.symbol_end ())

let mkinfo (s,(ln,ty,tag)) = (s,(ln,ty))
let mkbinfo (ln,ty,tag)    = (ln,ty)

let mkcall fn arg known =
  let atfront (_,(_,_,tag)) = tag = FRONT in
  if atfront fn
  then DECLARER(mkinfo fn,arg,known)
  else CALL(mkinfo fn,arg,known)

%}

%token <string * (int * Patch.linetype * Parse_error.atfront)> IDENT
%token <string * (int * Patch.linetype * Parse_error.atfront)> CST_CHAR
%token <string * (int * Patch.linetype * Parse_error.atfront)> CST_INT
%token <string * (int * Patch.linetype * Parse_error.atfront)> CST_STRING
%token <string * (int * Patch.linetype * Parse_error.atfront)> SEP
%token <string * (int * Patch.linetype * Parse_error.atfront)> ESEP
%token <string * (int * Patch.linetype * Parse_error.atfront)> OPERATOR
%token <string * (int * Patch.linetype * Parse_error.atfront)> EQ
%token <string * (int * Patch.linetype * Parse_error.atfront)> SYMOP
%token <string * (int * Patch.linetype * Parse_error.atfront)> DEREFOP
%token <string * (int * Patch.linetype * Parse_error.atfront)> TYPE
%token <string * (int * Patch.linetype * Parse_error.atfront)> PRIM
%token <string * (int * Patch.linetype * Parse_error.atfront)> INCLUDE

%token EOF 
%token <int * Patch.linetype * Parse_error.atfront>
  LPAREN "(" RPAREN ")" LBRACE "{" RBRACE "}" LBRACK "[" RBRACK "]" DEFINE

(* operator precedence *)
%left	ESEP
%right	RPAREN
%left	LPAREN
%right	EQ

(* Non-terminals informations *)
%start interpret
%type <Ast0.code list> interpret

%%

interpret:
  toplevel EOF {$1}
| EOF {[]}
;

(* cannot be empty *)
toplevel:
  expressions ltoplevel2            {EXPR($1)::$2}
| sep toplevel                      {SEP($1)::$2}
| definesym ltoplevel3              {$1@$2}
| define toplevel                   {$1@$2}
| expressionsend                    {[EXPR($1)]}
| sep                               {[SEP($1)]}
| defineend                         {$1}
| INCLUDE toplevel {EXPR([SYMBOL([IDENT(mkinfo $1)])])::$2}
| INCLUDE          {[EXPR([SYMBOL([IDENT(mkinfo $1)])])]}

(* cannot start with an expression *)
ltoplevel2:
  sep toplevel                      {SEP($1)::$2}
| sep                               {[SEP($1)]}
| definesym ltoplevel3              {$1@$2}
| define toplevel                   {$1@$2}
| defineend                         {$1}
| INCLUDE toplevel {EXPR([SYMBOL([IDENT(mkinfo $1)])])::$2}
| INCLUDE          {[EXPR([SYMBOL([IDENT(mkinfo $1)])])]}

(* cannot start with a lparen *)
ltoplevel3:
  expressionsd ltoplevel2           {EXPR($1)::$2}
| expressionsdend                   {[EXPR($1)]}
| sep toplevel                      {SEP($1)::$2}
| sep                               {[SEP($1)]}
| definesym ltoplevel3              {$1@$2}
| define toplevel                   {$1@$2}
| defineend                         {$1}
| INCLUDE toplevel {EXPR([SYMBOL([IDENT(mkinfo $1)])])::$2}
| INCLUDE          {[EXPR([SYMBOL([IDENT(mkinfo $1)])])]}

sep:
  SEP                               {mkinfo $1}
| "{"                            {"{",mkbinfo $1}
| "}"                            {"}",mkbinfo $1}
| ESEP                              {mkinfo $1}
| ")"                            {")",mkbinfo $1}
| "]"                            {"]",mkbinfo $1}


definesym:
  DEFINE IDENT
    {[SEP("#define",mkbinfo $1);
       EXPR([SYMBOL([IDENT(mkinfo $2)])])]}

define:
  DEFINE IDENT "(" args ")"
    {[SEP("#define",mkbinfo $1);
       EXPR([SYMBOL([IDENT(mkinfo $2)]);
		   PAREN($4,Ast.KNOWN)])]}

defineend:
  DEFINE IDENT
    {[SEP("#define",mkbinfo $1);
       EXPR([SYMBOL([IDENT(mkinfo $2)])])]}
| DEFINE
    {[SEP("#define",mkbinfo $1)]}
| DEFINE IDENT "(" args ")"
    {[SEP("#define",mkbinfo $1);
       EXPR([SYMBOL([IDENT(mkinfo $2)]);
		   PAREN($4,Ast.KNOWN)])]}
| DEFINE IDENT "(" args
    {[SEP("#define",mkbinfo $1);
       EXPR([SYMBOL([IDENT(mkinfo $2)]);
		   PAREN($4,Ast.ENDUNKNOWN)])]}
    

(* cannot be empty *)
expressions:
  symbol expressions2               {SYMBOL($1)::$2}
| symbol                            {[SYMBOL($1)]}
| dsymbol expressions3              {DSYMBOL($1)::$2}
| dsymbol                           {[DSYMBOL($1)]}
| expressions4                      {$1}

expressionsend:
  symbol expressions2end            {SYMBOL($1)::$2}
| symbolend                         {[SYMBOL($1)]}
| dsymbol expressions3end           {DSYMBOL($1)::$2}
| dsymbol                           {[DSYMBOL($1)]}
| expressions4end                   {$1}

(* cannot start with a symbol *)
expressions2:
  dsymbol expressions3              {DSYMBOL($1)::$2}
| dsymbol                           {[DSYMBOL($1)]}
| expressions4                      {$1}

expressions2end:
  dsymbol expressions3end           {DSYMBOL($1)::$2}
| dsymbol                           {[DSYMBOL($1)]}
| expressions4end                   {$1}

(* cannot start with a dsymbol *)
expressions3:
  symbol expressions2               {SYMBOL($1)::$2}
| symbol                            {[SYMBOL($1)]}
| expressions4                      {$1}

expressions3end:
  symbol expressions2end            {SYMBOL($1)::$2}
| symbol                            {[SYMBOL($1)]}
| expressions4end                   {$1}

(* cannot start with a symbol or dsymbol *)
expressions4:
  "(" args ")" expressions    {PAREN($2,Ast.KNOWN)::$4}
| OPERATOR expressions              {EOP(mkinfo $1)::$2}
| PRIM "(" args ")" expressions {(mkcall $1 $3 Ast.KNOWN)::$5}
| IDENT "(" args ")" expressions {(mkcall $1 $3 Ast.KNOWN)::$5}
| PRIM expressionsd                 {SYMBOL([IDENT(mkinfo $1)])::$2}
| EQ assignrhs                      {[ASSIGN(mkinfo $1,$2,Ast.KNOWN)]}
| "(" args ")"                {[PAREN($2,Ast.KNOWN)]}
| OPERATOR                          {[EOP(mkinfo $1)]}
| PRIM "(" args ")"           {[mkcall $1 $3 Ast.KNOWN]}
| IDENT "(" args ")"          {[mkcall $1 $3 Ast.KNOWN]}
| PRIM                              {[SYMBOL([IDENT(mkinfo $1)])]}

expressions4end:
  "(" args ")" expressionsend {PAREN($2,Ast.KNOWN)::$4}
| OPERATOR expressionsend           {EOP(mkinfo $1)::$2}
| PRIM "(" args ")" expressionsend {(mkcall $1 $3 Ast.KNOWN)::$5}
| IDENT "(" args ")" expressionsend {(mkcall $1 $3 Ast.KNOWN)::$5}
| PRIM expressionsdend              {SYMBOL([IDENT(mkinfo $1)])::$2}
| EQ assignrhsend                   {[ASSIGN(mkinfo $1,$2,Ast.KNOWN)]}
| EQ                               {[ASSIGN(mkinfo $1,[],Ast.ENDUNKNOWN)]}
| "(" args ")"                {[PAREN($2,Ast.KNOWN)]}
| "(" argsend                    {[PAREN($2,Ast.ENDUNKNOWN)]}
| OPERATOR                          {[EOP(mkinfo $1)]}
| PRIM "(" args ")"           {[mkcall $1 $3 Ast.KNOWN]}
| PRIM "(" argsend               {[mkcall $1 $3 Ast.ENDUNKNOWN]}
| IDENT "(" args ")"          {[mkcall $1 $3 Ast.KNOWN]}
| IDENT "(" argsend              {[mkcall $1 $3 Ast.ENDUNKNOWN]}
| PRIM                              {[SYMBOL([IDENT(mkinfo $1)])]}

(* cannot start with a lparen *)
expressionsd:
  symbol expressions2               {SYMBOL($1)::$2}
| symbol                            {[SYMBOL($1)]}
| dsymbol expressions3              {DSYMBOL($1)::$2}
| dsymbol                           {[DSYMBOL($1)]}
| OPERATOR expressions              {EOP(mkinfo $1)::$2}
| PRIM "(" args ")" expressions {(mkcall $1 $3 Ast.KNOWN)::$5}
| IDENT "(" args ")" expressions {(mkcall $1 $3 Ast.KNOWN)::$5}
| PRIM expressionsd                 {SYMBOL([IDENT(mkinfo $1)])::$2}
| EQ assignrhs                      {[ASSIGN(mkinfo $1,$2,Ast.KNOWN)]}
| OPERATOR                          {[EOP(mkinfo $1)]}
| PRIM "(" args ")"           {[mkcall $1 $3 Ast.KNOWN]}
| IDENT "(" args ")"          {[mkcall $1 $3 Ast.KNOWN]}
| PRIM                              {[SYMBOL([IDENT(mkinfo $1)])]}

expressionsdend:
  symbol expressions2end            {SYMBOL($1)::$2}
| symbol                            {[SYMBOL($1)]}
| dsymbol expressions3end           {DSYMBOL($1)::$2}
| dsymbol                           {[DSYMBOL($1)]}
| OPERATOR expressionsend           {EOP(mkinfo $1)::$2}
| PRIM "(" args ")" expressionsend {(mkcall $1 $3 Ast.KNOWN)::$5}
| IDENT "(" args ")" expressionsend {(mkcall $1 $3 Ast.KNOWN)::$5}
| PRIM expressionsdend              {SYMBOL([IDENT(mkinfo $1)])::$2}
| EQ assignrhsend                   {[ASSIGN(mkinfo $1,$2,Ast.KNOWN)]}
| EQ                               {[ASSIGN(mkinfo $1,[],Ast.ENDUNKNOWN)]}
| OPERATOR                          {[EOP(mkinfo $1)]}
| PRIM "(" args ")"           {[mkcall $1 $3 Ast.KNOWN]}
| PRIM "(" argsend               {[mkcall $1 $3 Ast.ENDUNKNOWN]}
| IDENT "(" args ")"          {[mkcall $1 $3 Ast.KNOWN]}
| IDENT "(" argsend              {[mkcall $1 $3 Ast.ENDUNKNOWN]}
| PRIM                              {[SYMBOL([IDENT(mkinfo $1)])]}

args:
  expressions                   {[EXPR($1)]}
| expressions args2             {EXPR($1)::$2}
| args2                         {$1}
|                               {[]}

args2:
  ESEP args                     {SEP(mkinfo $1)::$2}
| "{" args "}" args       {EXPR([STRUCT($2,Ast.KNOWN)])::$4}

argsend:
  expressionsend                {[EXPR($1)]}
| expressions args2end          {EXPR($1)::$2}
| args2end                      {$1}
|                               {[]}

args2end:
  ESEP argsend                  {SEP(mkinfo $1)::$2}
| "{" args "}" argsend    {EXPR([STRUCT($2,Ast.KNOWN)])::$4}
| "{" argsend                {[EXPR([STRUCT($2,Ast.ENDUNKNOWN)])]}

(* cannot be empty *)
assignrhs:
  symbol expressions2           {SYMBOL($1)::$2}
| symbol                        {[SYMBOL($1)]}
| dsymbol expressions3          {DSYMBOL($1)::$2}
| dsymbol                       {[DSYMBOL($1)]}
| expressions4                  {$1}
| "{" args "}"            {[STRUCT($2,Ast.KNOWN)]}

assignrhsend:
  symbol expressions2end        {SYMBOL($1)::$2}
| symbolend                     {[SYMBOL($1)]}
| dsymbol expressions3end       {DSYMBOL($1)::$2}
| dsymbol                       {[DSYMBOL($1)]}
| expressions4end               {$1}
| "{" args "}"            {[STRUCT($2,Ast.KNOWN)]}
| "{" argsend                {[STRUCT($2,Ast.ENDUNKNOWN)]}

(* cannot be empty *)
symbol:
  atoken symbol                       {$1::$2}
| atoken                              {[$1]}
| "[" expressions "]" symbol    {ARRAY($2,Ast.KNOWN)::$4}
| "[" expressions "]"           {[ARRAY($2,Ast.KNOWN)]}
| "[" "]" symbol                {ARRAY([],Ast.KNOWN)::$3}
| "[" "]"                       {[ARRAY([],Ast.KNOWN)]}

symbolend:
  atoken symbolend                    {$1::$2}
| atoken                              {[$1]}
| "[" expressions "]" symbolend {ARRAY($2,Ast.KNOWN)::$4}
| "[" expressions "]"           {[ARRAY($2,Ast.KNOWN)]}
| "[" expressionsend               {[ARRAY($2,Ast.ENDUNKNOWN)]}
| "[" "]" symbolend             {ARRAY([],Ast.KNOWN)::$3}
| "[" "]"                       {[ARRAY([],Ast.KNOWN)]}
| "["                              {[ARRAY([],Ast.ENDUNKNOWN)]}

atoken:
  IDENT                               {IDENT(mkinfo $1)}
| CST_CHAR                            {CHAR(mkinfo $1)}
| CST_INT                             {INT(mkinfo $1)}
| CST_STRING                          {STR(mkinfo $1)}
| SYMOP                               {SYMOP(mkinfo $1)}
| TYPE                                {TYPE(mkinfo $1)}

dsymbol:
  DEREFOP dsymbol                     {DEREFOP(mkinfo $1)::$2}
| DEREFOP                             {[DEREFOP(mkinfo $1)]}
