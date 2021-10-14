
lexer grammar IntLexer;

WS: [ \t\n]+ -> skip;

INT : DIGIT+ ;


PLUS: '+';
MINUS: '-';
STAR: '*';
SIGMA: 'sigma';
PI: 'pi';
POW: 'pow';
ID: 'x';

fragment DIGIT: [0-9];

