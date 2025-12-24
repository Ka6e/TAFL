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
    | whileStatement
    | doWhileStatement
    | breakStatement
    | continueStatement
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

assignmentExpr
    : IDENTIFIER ASSIGN expression
    ;

ifStatement
    : IF expression THEN block (ELSE block)?
    ;

forStatement
    : FOR assignmentOrEmpty COMMA expression COMMA assignmentOrEmpty IN block
    ;

assignmentOrEmpty
    : assignmentExpr
    | /* empty */
    ;

whileStatement
    : WHILE LPAREN expression RPAREN block
    ;

doWhileStatement
    : DO block WHILE LPAREN expression RPAREN SEMICOLON
    ;
breakStatement
    : BREAK SEMICOLON
    ;

continueStatement
    : CONTINUE SEMICOLON
    ;

returnStatement
    : RETURN expression? SEMICOLON
    ;

matchStatement
    : MATCH expression LBRACE matchCase* RBRACE
    ;

matchCase
    : CASE pattern COLON block
    ;

pattern
    : IDENTIFIER
    | enumPattern
    | UNDERSCORE
    ;

enumPattern
    : IDENTIFIER LPAREN identifierList? RPAREN
    ;

identifierList
    : IDENTIFIER (COMMA IDENTIFIER)*
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
