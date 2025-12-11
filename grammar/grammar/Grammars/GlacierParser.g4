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

// ---------- Statements ----------
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

// переменная (включая let/var)
variableDecl
    : (LET | VAR) IDENTIFIER (COLON typeAnnotation)? (ASSIGN expression)? SEMICOLON
    ;

// присваивание как инструкция (с точкой с запятой)
assignment
    : IDENTIFIER ASSIGN expression SEMICOLON
    ;

// присваивание как выражение/в заголовке for (без ; )
assignmentExpr
    : IDENTIFIER ASSIGN expression
    ;

// если-ветвление (в спецификации требуем THEN и блоки)
ifStatement
    : IF expression THEN block (ELSE block)?
    ;

// for: трёхчастный императивный цикл: init, condition, post in block
// init/post могут быть пустыми (assignmentExpr | empty)
forStatement
    : FOR assignmentOrEmpty COMMA expression COMMA assignmentOrEmpty IN block
    ;

// вспомогательное: assignmentExpr или пусто
assignmentOrEmpty
    : assignmentExpr
    | /* empty */
    ;

// while (с явными скобками)
whileStatement
    : WHILE LPAREN expression RPAREN block
    ;

// do ... while (проверка после тела), завершается точкой с запятой
doWhileStatement
    : DO block WHILE LPAREN expression RPAREN SEMICOLON
    ;

// break / continue
breakStatement
    : BREAK SEMICOLON
    ;

continueStatement
    : CONTINUE SEMICOLON
    ;

// return
returnStatement
    : RETURN expression? SEMICOLON
    ;

// match / pattern matching
matchStatement
    : MATCH expression LBRACE matchCase* RBRACE
    ;

matchCase
    : CASE pattern COLON block
    ;

// pattern: идентификатор, enum-паттерн или wildcard '_'
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

// ---------- Expressions ----------
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
