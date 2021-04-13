:- style_check(-singleton).
:- use_module(library(pcre)).

program(t_program(X)) -->[begin],statements(X),[";"],[end].
program(t_comment(X,Y)) -->comment(X),[";"],[begin],statements(Y),[";"],[end].

statements(t_statement(X,Y))-->allstatements(X),[";"],statements(Y).
statements(t_singlestatement(X))-->allstatements(X).

allstatements(t_printSt(X)) --> print(X).
allstatements(t_declarationSt(X)) --> declaration(X).
allstatements(t_assign(X)) --> assign(X).
allstatements(t_ifelse(X)) --> ifelse(X).
allstatements(t_for(X)) --> for(X).
allstatements(t_whileBlock(X)) --> whileloop(X).

declaration(t_declaration(A,B,C,D))-->datatype(A),["//"],identifier(B),assign(C),data(D).
declaration(t_declaration(X,Y))-->datatype(X),["//"],identifier(Y).

assign(t_assign(X,Y,Z))-->identifier(X),assign(Y),expression(Z).

print(t_printString(X))-->["printout"],["//"],["'"],{string(X)},["'"],[";"].
print(t_printString(X))-->["printout"],["'"],{string(X)},["'"],[";"].
print(t_printIdentifier(X))-->["printout"],["//"],identifier(X),[";"].

print(t_printIdentifier(X))-->["printout"],identifier(X),[";"].

ifelse(t_if(X,Y))-->[if],["("],condition(X),[")"],["{"],statements(Y),[";"],["}"].
ifelse(t_ifelseif(X,Y,A,B))-->[if],["("],condition(X),[")"],["{"],statements(Y),[";"],["}"],elseifLoop(A),["else"],["{"],statements(B),[;],["}"].
ifelse(t_ifelse(X,Y,A))-->[if],['('],condition(X),[')'],['{'],statements(Y),[";"],['}'],[else],['{'],statements(A),[;],['}'].

elseifLoop(t_elseif(X,Y))-->elseifLoop1(X),[;],elseifLoop(Y).
elseifLoop(t_elseif(X))-->elseifLoop1(X).

elseifLoop1(t_else(X,Y))-->[elseif],['('],condition(X),[')'],['{'],statements(Y),[;],['}'].

whileloop(t_while(X,While)) --> ["while"], ["("],condition(X), [")"],["{"] , statements(While), [";"], ["}"].
      
for(t_for(X,Y)) --> [for],forrange(X),["{"],statements(Y),["}"].
forrange(t_forrange(X,Y,Z,A,B)) --> ["("],identifier(X), [=], expression(Y) ,[";"] ,identifier(Z),comparision(A),expression(B),[";"],[")"]. 
forrange(t_forrange(X,Y,Z,A,B,C)) --> ["("] ,identifier(X) ,[=] ,expression(Y) ,[";"] ,identifier(Z) ,comparision(A), expression(B), [";"] ,expression(C) ,[")"].
forrange(t_forrange(X,A,B)) --> identifier(X), [in],["("], expression(A), [","], expression(B) ,[")"].
condition(t_cond(A,B,C)) --> identifier(A), ["//"], comparision(B), ["//"], expression(C).
condition(t_cond(X,Y,Z,A,B)) --> identifier(X),["//"],comparision(Y),expression(Z), condop(A), condition(B). 
condition(t_cond_bool(X))--> bool(X).
         
comment(t_singleComment(X)) --> [X], {string(X)}.
         
expression(t_add(X,Y)) --> term(X),[+],expression(Y).
expression(t_sub(X,Y)) --> term(X),[-],expression(Y).
expression(X) --> term(X).
term(t_multiply(X,Y)) --> factor(X),[*],term(Y).
term(t_divide(X,Y))-->factor(X),[/],term(Y). 
term(X)-->factor(X).
factor(X) --> ["("],expression(X),[")"].
factor(X) -->data(X).
factor(X) -->identifier(X).
         
bool(t_booltrue(true))--> [true].
bool(t_boolfalse(false))--> [false].
bool(t_bool(T1,T2))-->expression(T1), [=], expression(T2).
bool(X)--> [not],bool(X).
/*integer(0) --> [0].
integer(1) --> [1].
integer(2) --> [2].
integer(3) --> [3].
integer(4) --> [4].
integer(5) --> [5].
integer(6) --> [6].
integer(7) --> [7].
integer(8) --> [8].
integer(9) --> [9].*/
%string --> /\”[\x00-\x7F]*\”/
%CHAR := /'[\x00-\x7F]'/
%FLOAT := /([0-9]*[.])?[0-9]+/

 datatype(t_datatype(int)) --> ["int"].
datatype(t_datatype(bool)) --> ["bool"].
datatype(t_datatype(string)) --> ["string"].
 datatype(t_datatype(float)) --> ["float"].
 datatype(t_datatype(char)) --> ["char"].

     
identifier(t_identifier(I)) --> [I], {re_match("^[a-zA-Z_$][a-zA-Z_$0-9]+", I)}.
     
 data(t_integer(N)) --> [N], {re_match("^[0-9]+", N)}.
data(t_string(S)) --> ["'"], [S], {string(S)}, ["'"], !.
data(t_bool(true)) --> ["true"].
data(t_bool(false)) --> ["false"].
      data(t_char(N)) --> [N], {re_match("^[\x00-\x7F]+", N)}.
     data(t_float(N)) --> [N], {re_match("^([0-9]*[.])?[0-9]+", N)}.
     

comparision(t_greater(>)) --> [">"].
comparision(t_lesser(<)) --> ["<"].
comparision(t_greaterequal(>=)) --> [">"], ["="].
comparision(t_lesserequal(<=)) --> ["<"], ["="].
comparision(t_equal(==)) --> ["="], ["="].

condop(t_and(and))--> [and].
condop(t_or(or))--> [or].
condop(t_not(not))--> [not].
%comment --> [@].
%print --> [printout].
