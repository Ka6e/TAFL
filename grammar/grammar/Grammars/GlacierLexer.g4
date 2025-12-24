lexer grammar GlacierLexer;

options {
    caseInsensitive = true;
}

MODULE: 'module';
IMPORT: 'import';

CLASS: 'class';
ENUM: 'enum';
FUNC: 'func';
LET: 'let';
VAR: 'var';
RETURN: 'return';
IF: 'if';
THEN: 'then';
ELSE: 'else';
FOR: 'for';
IN: 'in';
MATCH: 'match';
CASE: 'case';
NEW: 'new';
THIS: 'this';

PI: 'PI';
EULER: 'EULER';

ABS: 'ABS';
MIN: 'MIN';
MAX: 'MAX';
POW: 'POW';
ROUND: 'ROUND';
CEIL: 'CEIL';
FLOOR: 'FLOOR';

PRINT: 'print';
READ_INT: 'readInt';
READ_LINE: 'readLine';

WHILE: 'while';
DO: 'do';
BREAK: 'break';
CONTINUE: 'continue';

UNDERSCORE: '_';

PLUS: '+';
MINUS: '-';
MULT: '*';
DIV: '/';
MOD: '%';
INT_DIV: '//';
POW_OP: '**';

BIT_AND: '&';
BIT_OR: '|';
BIT_XOR: '^';
BIT_NOT: '~';

EQ: '==';
NEQ: '!=';
LT: '<';
LTE: '<=';
GT: '>';
GTE: '>=';

LOGICAL_NOT: '!';
LOGICAL_AND: '&&';
LOGICAL_OR: '||';

LPAREN: '(';
RPAREN: ')';
LBRACE: '{';
RBRACE: '}';
COLON: ':';
COMMA: ',';
SEMICOLON: ';';
DOT: '.';

ASSIGN: '=';  

fragment DECIMAL_LITERAL: [0-9] ([0-9_]* [0-9])?;
fragment HEX_LITERAL: '0' [xX] [0-9a-fA-F] ([0-9a-fA-F_]* [0-9a-fA-F])?;
fragment BINARY_LITERAL: '0' [bB] [01] ([01_]* [01])?;
fragment EXPONENT: [eE] [+-]? [0-9]+;

INTEGER_LITERAL: DECIMAL_LITERAL | HEX_LITERAL | BINARY_LITERAL;
FLOAT_LITERAL: [0-9]* '.' [0-9]+ EXPONENT? | [0-9]+ EXPONENT;

IDENTIFIER: [a-zA-Z_][a-zA-Z_0-9]*;

WS: [ \t\r\n]+ -> skip;
SINGLE_LINE_COMMENT: '//' ~[\r\n]* -> skip;
MULTILINE_COMMENT: '/*' .*? '*/' -> skip;
