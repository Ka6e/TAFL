lexer grammar GlacierLexer;

options {
    caseInsensitive = true;  // ключевые слова не чувствительны к регистру
}

// Константы
PI: 'PI';
EULER: 'EULER';

// Функции
ABS: 'ABS';
MIN: 'MIN';
MAX: 'MAX';
POW: 'POW';
ROUND: 'ROUND';
CEIL: 'CEIL';
FLOOR: 'FLOOR';

// Арифметические операторы
PLUS: '+';
MINUS: '-';
MULT: '*';
DIV: '/';
MOD: '%';
INT_DIV: '//';
POW_OP: '**';

// Побитовые операторы
BIT_AND: '&';
BIT_OR: '|';
BIT_XOR: '^';
BIT_NOT: '~';

// Логические операторы
EQ: '==';
NEQ: '!=';
LT: '<';
LTE: '<=';
GT: '>';
GTE: '>=';

LOGICAL_NOT: '!';
LOGICAL_AND: '&&';
LOGICAL_OR: '||';

// Скобки и разделители
LPAREN: '(';
RPAREN: ')';
COMMA: ',';

// Литералы
fragment DECIMAL_LITERAL: [0-9] ([0-9_]* [0-9])?;
fragment HEX_LITERAL: '0' [xX] [0-9a-fA-F] ([0-9a-fA-F_]* [0-9a-fA-F])?;
fragment BINARY_LITERAL: '0' [bB] [01] ([01_]* [01])?;
fragment EXPONENT: [eE] [+-]? [0-9]+;

INTEGER_LITERAL: DECIMAL_LITERAL | HEX_LITERAL | BINARY_LITERAL;
FLOAT_LITERAL: [0-9]* '.' [0-9]+ EXPONENT? | [0-9]+ EXPONENT;

// Идентификаторы
IDENTIFIER: [a-zA-Z_][a-zA-Z_0-9]*;

// Пробелы и комментарии
WS: [ \t\r\n]+ -> skip;
SINGLE_LINE_COMMENT: '//' ~[\r\n]* -> skip;
MULTILINE_COMMENT: '/*' .*? '*/' -> skip;
