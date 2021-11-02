
lexer grammar MiniScalaLexer;

WS: [ \t\n]+ -> skip;

INT : DIGIT+ ;
BOOL : ('true' | 'false');

VAR: 'var';
VAL: 'val';
DEF: 'def';
PLUS: '+';
MINUS: '-';
STAR: '*';
SLASH: '/';
MOD: 'mod';
SEMI: ';';
ARROW: '=>';
GT: '>';
GEQ: '>=';
ASN: ':=';
ISZERO: 'iszero';
IF: 'if';
THEN: 'then';
ELSE: 'else';
EQ: '=';
LPAREN: '(';
RPAREN: ')';
LBRACKET: '{';
RBRACKET: '}';
LSQBRACKET: '[';
RSQBRACKET: ']';
COMMA: ',';
CONS: 'Cons';
NIL: 'Nil';
ID: ALPHA+;
fragment DIGIT: [0-9];
fragment ALPHA: [a-zA-Z];
