parser grammar GlacierParser;

options { tokenVocab=GlacierLexer; }

expressionRoot
    : expression EOF
    ;

expression
    : logicalOrExpression
    ;

logicalOrExpression
    : logicalAndExpression (LOGICAL_OR logicalAndExpression)*
    ;

logicalAndExpression
    : comparisonExpression (LOGICAL_AND comparisonExpression)*
    ;

comparisonExpression
    : bitOrExpression ((EQ | NEQ | LT | LTE | GT | GTE) bitOrExpression)*
    ;

bitOrExpression
    : bitXorExpression (BIT_OR bitXorExpression)*
    ;

bitXorExpression
    : bitAndExpression (BIT_XOR bitAndExpression)*
    ;

bitAndExpression
    : additiveExpression (BIT_AND additiveExpression)*
    ;

additiveExpression
    : multiplicativeExpression ((PLUS | MINUS) multiplicativeExpression)*
    ;

multiplicativeExpression
    : powerExpression ((MULT | DIV | MOD | INT_DIV) powerExpression)*
    ;

powerExpression
    : unaryExpression (POW_OP powerExpression)?
    ;

unaryExpression
    : (PLUS | MINUS | BIT_NOT | LOGICAL_NOT)? primaryExpression
    ;

primaryExpression
    : literal
    | functionCall
    | LPAREN expression RPAREN
    | IDENTIFIER
    | PI
    | EULER
    ;

literal
    : INTEGER_LITERAL
    | FLOAT_LITERAL
    ;

functionCall
    : (IDENTIFIER | ABS | MIN | MAX | POW | ROUND | CEIL | FLOOR)
      LPAREN (argumentList)? RPAREN
    ;

argumentList
    : expression (COMMA expression)*
    ;
