/* IMPORTANT! An expression cannot follow another expression.  A symbol
cannot follow another symbol. */
/* not fault tolerant */

%{
open Parse_error

let parse_error _ =
  display_error "Syntax error"
    (Parsing.symbol_start ()) (Parsing.symbol_end ())

%}

%token <string * int> IDENT
%token <string * int> CST_CHAR
%token <string * int> CST_INT
%token <string * int> CST_STRING
%token <string * int> SEP
%token <string * int> ESEP
%token <string * int> OPERATOR
%token <string * int> EQ
%token <string * int> SYMOP
%token <string * int> DEREFOP
%token <string * int> TYPE
%token <string * int> PRIM
%token <string * int> INCLUDE

%token EOF 
%token <int> LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK   DEFINE

/* operator precedence */
%left	ESEP
%right	RPAREN
%left	LPAREN
%right	EQ

/* Non-terminals informations */
%start interpret
%type <Ast0.code list> interpret

%%

interpret:
  toplevel EOF {$1}
| EOF {[]}
;

/* cannot be empty */
toplevel:
  expressions ltoplevel2            {Ast0.EXPR($1)::$2}
| sep toplevel                      {Ast0.SEP($1)::$2}
| definesym ltoplevel3              {$1@$2}
| define toplevel                   {$1@$2}
| expressionsend                    {[Ast0.EXPR($1)]}
| sep                               {[Ast0.SEP($1)]}
| defineend                         {$1}
| INCLUDE toplevel            {Ast0.EXPR([Ast0.SYMBOL([Ast0.IDENT($1)])])::$2}
| INCLUDE                     {[Ast0.EXPR([Ast0.SYMBOL([Ast0.IDENT($1)])])]}

/* cannot start with an expression */
ltoplevel2:
  sep toplevel                      {Ast0.SEP($1)::$2}
| sep                               {[Ast0.SEP($1)]}
| definesym ltoplevel3              {$1@$2}
| define toplevel                   {$1@$2}
| defineend                         {$1}
| INCLUDE toplevel            {Ast0.EXPR([Ast0.SYMBOL([Ast0.IDENT($1)])])::$2}
| INCLUDE                     {[Ast0.EXPR([Ast0.SYMBOL([Ast0.IDENT($1)])])]}

/* cannot start with a lparen */
ltoplevel3:
  expressionsd ltoplevel2           {Ast0.EXPR($1)::$2}
| expressionsdend                   {[Ast0.EXPR($1)]}
| sep toplevel                      {Ast0.SEP($1)::$2}
| sep                               {[Ast0.SEP($1)]}
| definesym ltoplevel3              {$1@$2}
| define toplevel                   {$1@$2}
| defineend                         {$1}
| INCLUDE toplevel            {Ast0.EXPR([Ast0.SYMBOL([Ast0.IDENT($1)])])::$2}
| INCLUDE                     {[Ast0.EXPR([Ast0.SYMBOL([Ast0.IDENT($1)])])]}

sep:
  SEP                               {$1}
| LBRACE                            {"{",$1}
| RBRACE                            {"}",$1}
| ESEP                              {$1}
| RPAREN                            {")",$1}
| RBRACK                            {"]",$1}


definesym:
  DEFINE IDENT
    {[Ast0.SEP("#define",$1);Ast0.EXPR([Ast0.SYMBOL([Ast0.IDENT($2)])])]}

define:
  DEFINE IDENT LPAREN args RPAREN
    {[Ast0.SEP("#define",$1);
       Ast0.EXPR([Ast0.SYMBOL([Ast0.IDENT($2)]);Ast0.PAREN($4,Ast.KNOWN)])]}

defineend:
  DEFINE IDENT
    {[Ast0.SEP("#define",$1);Ast0.EXPR([Ast0.SYMBOL([Ast0.IDENT($2)])])]}
| DEFINE
    {[Ast0.SEP("#define",$1)]}
| DEFINE IDENT LPAREN args RPAREN
    {[Ast0.SEP("#define",$1);
       Ast0.EXPR([Ast0.SYMBOL([Ast0.IDENT($2)]);Ast0.PAREN($4,Ast.KNOWN)])]}
| DEFINE IDENT LPAREN args
    {[Ast0.SEP("#define",$1);
       Ast0.EXPR([Ast0.SYMBOL([Ast0.IDENT($2)]);
		   Ast0.PAREN($4,Ast.ENDUNKNOWN)])]}
    

/* cannot be empty */
expressions:
  symbol expressions2               {Ast0.SYMBOL($1)::$2}
| symbol                            {[Ast0.SYMBOL($1)]}
| dsymbol expressions3              {Ast0.DSYMBOL($1)::$2}
| dsymbol                           {[Ast0.DSYMBOL($1)]}
| expressions4                      {$1}

expressionsend:
  symbol expressions2end            {Ast0.SYMBOL($1)::$2}
| symbolend                         {[Ast0.SYMBOL($1)]}
| dsymbol expressions3end           {Ast0.DSYMBOL($1)::$2}
| dsymbol                           {[Ast0.DSYMBOL($1)]}
| expressions4end                   {$1}

/* cannot start with a symbol */
expressions2:
  dsymbol expressions3              {Ast0.DSYMBOL($1)::$2}
| dsymbol                           {[Ast0.DSYMBOL($1)]}
| expressions4                      {$1}

expressions2end:
  dsymbol expressions3end           {Ast0.DSYMBOL($1)::$2}
| dsymbol                           {[Ast0.DSYMBOL($1)]}
| expressions4end                   {$1}

/* cannot start with a dsymbol */
expressions3:
  symbol expressions2               {Ast0.SYMBOL($1)::$2}
| symbol                            {[Ast0.SYMBOL($1)]}
| expressions4                      {$1}

expressions3end:
  symbol expressions2end            {Ast0.SYMBOL($1)::$2}
| symbol                            {[Ast0.SYMBOL($1)]}
| expressions4end                   {$1}

/* cannot start with a symbol or dsymbol */
expressions4:
  LPAREN args RPAREN expressions    {Ast0.PAREN($2,Ast.KNOWN)::$4}
| OPERATOR expressions              {Ast0.EOP($1)::$2}
| PRIM LPAREN args RPAREN expressions {Ast0.CALL($1,$3,Ast.KNOWN)::$5}
| PRIM expressionsd                 {Ast0.SYMBOL([Ast0.IDENT($1)])::$2}
| EQ assignrhs                      {[Ast0.ASSIGN($1,$2,Ast.KNOWN)]}
| LPAREN args RPAREN                {[Ast0.PAREN($2,Ast.KNOWN)]}
| OPERATOR                          {[Ast0.EOP($1)]}
| PRIM LPAREN args RPAREN           {[Ast0.CALL($1,$3,Ast.KNOWN)]}
| PRIM                              {[Ast0.SYMBOL([Ast0.IDENT($1)])]}

expressions4end:
  LPAREN args RPAREN expressionsend {Ast0.PAREN($2,Ast.KNOWN)::$4}
| OPERATOR expressionsend           {Ast0.EOP($1)::$2}
| PRIM LPAREN args RPAREN expressionsend {Ast0.CALL($1,$3,Ast.KNOWN)::$5}
| PRIM expressionsdend              {Ast0.SYMBOL([Ast0.IDENT($1)])::$2}
| EQ assignrhsend                   {[Ast0.ASSIGN($1,$2,Ast.KNOWN)]}
| EQ                                {[Ast0.ASSIGN($1,[],Ast.ENDUNKNOWN)]}
| LPAREN args RPAREN                {[Ast0.PAREN($2,Ast.KNOWN)]}
| LPAREN argsend                    {[Ast0.PAREN($2,Ast.ENDUNKNOWN)]}
| OPERATOR                          {[Ast0.EOP($1)]}
| PRIM LPAREN args RPAREN           {[Ast0.CALL($1,$3,Ast.KNOWN)]}
| PRIM LPAREN argsend               {[Ast0.CALL($1,$3,Ast.ENDUNKNOWN)]}
| PRIM                              {[Ast0.SYMBOL([Ast0.IDENT($1)])]}

/* cannot start with a lparen */
expressionsd:
  symbol expressions2               {Ast0.SYMBOL($1)::$2}
| symbol                            {[Ast0.SYMBOL($1)]}
| dsymbol expressions3              {Ast0.DSYMBOL($1)::$2}
| dsymbol                           {[Ast0.DSYMBOL($1)]}
| OPERATOR expressions              {Ast0.EOP($1)::$2}
| PRIM LPAREN args RPAREN expressions {Ast0.CALL($1,$3,Ast.KNOWN)::$5}
| PRIM expressionsd                 {Ast0.SYMBOL([Ast0.IDENT($1)])::$2}
| EQ assignrhs                      {[Ast0.ASSIGN($1,$2,Ast.KNOWN)]}
| OPERATOR                          {[Ast0.EOP($1)]}
| PRIM LPAREN args RPAREN           {[Ast0.CALL($1,$3,Ast.KNOWN)]}
| PRIM                              {[Ast0.SYMBOL([Ast0.IDENT($1)])]}

expressionsdend:
  symbol expressions2end            {Ast0.SYMBOL($1)::$2}
| symbol                            {[Ast0.SYMBOL($1)]}
| dsymbol expressions3end           {Ast0.DSYMBOL($1)::$2}
| dsymbol                           {[Ast0.DSYMBOL($1)]}
| OPERATOR expressionsend           {Ast0.EOP($1)::$2}
| PRIM LPAREN args RPAREN expressionsend {Ast0.CALL($1,$3,Ast.KNOWN)::$5}
| PRIM expressionsdend              {Ast0.SYMBOL([Ast0.IDENT($1)])::$2}
| EQ assignrhsend                   {[Ast0.ASSIGN($1,$2,Ast.KNOWN)]}
| EQ                                {[Ast0.ASSIGN($1,[],Ast.ENDUNKNOWN)]}
| OPERATOR                          {[Ast0.EOP($1)]}
| PRIM LPAREN args RPAREN           {[Ast0.CALL($1,$3,Ast.KNOWN)]}
| PRIM LPAREN argsend               {[Ast0.CALL($1,$3,Ast.ENDUNKNOWN)]}
| PRIM                              {[Ast0.SYMBOL([Ast0.IDENT($1)])]}

args:
  expressions                   {[Ast0.EXPR($1)]}
| expressions args2             {Ast0.EXPR($1)::$2}
| args2                         {$1}
|                               {[]}

args2:
  ESEP args                     {Ast0.SEP($1)::$2}
| LBRACE args RBRACE args       {Ast0.EXPR([Ast0.STRUCT($2,Ast.KNOWN)])::$4}

argsend:
  expressionsend                {[Ast0.EXPR($1)]}
| expressions args2end          {Ast0.EXPR($1)::$2}
| args2end                      {$1}
|                               {[]}

args2end:
  ESEP argsend                  {Ast0.SEP($1)::$2}
| LBRACE args RBRACE argsend    {Ast0.EXPR([Ast0.STRUCT($2,Ast.KNOWN)])::$4}
| LBRACE argsend                {[Ast0.EXPR([Ast0.STRUCT($2,Ast.ENDUNKNOWN)])]}

/* cannot be empty */
assignrhs:
  symbol expressions2           {Ast0.SYMBOL($1)::$2}
| symbol                        {[Ast0.SYMBOL($1)]}
| dsymbol expressions3          {Ast0.DSYMBOL($1)::$2}
| dsymbol                       {[Ast0.DSYMBOL($1)]}
| expressions4                  {$1}
| LBRACE args RBRACE            {[Ast0.STRUCT($2,Ast.KNOWN)]}

assignrhsend:
  symbol expressions2end        {Ast0.SYMBOL($1)::$2}
| symbolend                     {[Ast0.SYMBOL($1)]}
| dsymbol expressions3end       {Ast0.DSYMBOL($1)::$2}
| dsymbol                       {[Ast0.DSYMBOL($1)]}
| expressions4end               {$1}
| LBRACE args RBRACE            {[Ast0.STRUCT($2,Ast.KNOWN)]}
| LBRACE argsend                {[Ast0.STRUCT($2,Ast.ENDUNKNOWN)]}

/* cannot be empty */
symbol:
  atoken symbol                       {$1::$2}
| atoken                              {[$1]}
| LBRACK expressions RBRACK symbol    {Ast0.ARRAY($2,Ast.KNOWN)::$4}
| LBRACK expressions RBRACK           {[Ast0.ARRAY($2,Ast.KNOWN)]}
| LBRACK RBRACK symbol                {Ast0.ARRAY([],Ast.KNOWN)::$3}
| LBRACK RBRACK                       {[Ast0.ARRAY([],Ast.KNOWN)]}

symbolend:
  atoken symbolend                    {$1::$2}
| atoken                              {[$1]}
| LBRACK expressions RBRACK symbolend {Ast0.ARRAY($2,Ast.KNOWN)::$4}
| LBRACK expressions RBRACK           {[Ast0.ARRAY($2,Ast.KNOWN)]}
| LBRACK expressionsend               {[Ast0.ARRAY($2,Ast.ENDUNKNOWN)]}
| LBRACK RBRACK symbolend             {Ast0.ARRAY([],Ast.KNOWN)::$3}
| LBRACK RBRACK                       {[Ast0.ARRAY([],Ast.KNOWN)]}
| LBRACK                              {[Ast0.ARRAY([],Ast.ENDUNKNOWN)]}

atoken:
  IDENT                               {Ast0.IDENT($1)}
| CST_CHAR                            {Ast0.CHAR($1)}
| CST_INT                             {Ast0.INT($1)}
| CST_STRING                          {Ast0.STR($1)}
| SYMOP                               {Ast0.SYMOP($1)}
| TYPE                                {Ast0.TYPE($1)}

dsymbol:
  DEREFOP dsymbol                     {Ast0.DEREFOP($1)::$2}
| DEREFOP                             {[Ast0.DEREFOP($1)]}
