parser grammar GlacierParser;

options { tokenVocab=GlacierLexer; }

program
    : moduleDecl importDecl* topLevelDecl* EOF
    ;

moduleDecl
    : MODULE IDENTIFIER
    ;

importDecl
    : IMPORT IDENTIFIER
    ;

// Верхнеуровневые объявления
topLevelDecl
    : classDecl
    | enumDecl
    | functionDecl
    | statement
    ;

classDecl
    : CLASS IDENTIFIER LBRACE classMember* RBRACE
    ;

classMember
    : variableDecl
    | functionDecl
    ;

enumDecl
    : ENUM IDENTIFIER LBRACE enumCase* RBRACE
    ;

enumCase
    : IDENTIFIER (LPAREN parameterList RPAREN)?
    ;

functionDecl
    : FUNC IDENTIFIER LPAREN parameterList? RPAREN (COLON typeAnnotation)? block
    ;

parameterList
    : parameter (COMMA parameter)*
    ;

parameter
    : IDENTIFIER (COLON typeAnnotation)?
    ;

statement
    : variableDecl
    | assignment
    | ifStatement
    | forStatement
    | returnStatement
    | matchStatement
    | expressionStatement
    ;

variableDecl
    : (LET | VAR) IDENTIFIER (COLON typeAnnotation)? (ASSIGN expression)? SEMICOLON
    ;

assignment
    : IDENTIFIER ASSIGN expression SEMICOLON
    ;

ifStatement
    : IF expression THEN block (ELSE block)?
    ;

forStatement
    : FOR assignment expression COMMA expression IN block
    ;

returnStatement
    : RETURN expression? SEMICOLON
    ;

matchStatement
    : MATCH expression LBRACE matchCase* RBRACE
    ;

matchCase
    : CASE IDENTIFIER (LPAREN IDENTIFIER RPAREN)? COLON statement
    ;

expressionStatement
    : expression SEMICOLON
    ;

block
    : LBRACE statement* RBRACE
    ;

typeAnnotation
    : IDENTIFIER
    ;

// Expressions 
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
    | THIS
    ;

literal
    : INTEGER_LITERAL
    | FLOAT_LITERAL
    ;

functionCall
    : (IDENTIFIER | ABS | MIN | MAX | POW | ROUND | CEIL | FLOOR | PRINT | READ_INT | READ_LINE)
      LPAREN argumentList? RPAREN
    ;

argumentList
    : expression (COMMA expression)*
    ;
