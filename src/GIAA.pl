giaa(Lexername, Filename) :-
    process_create(path('python3.9'), [Lexername, Filename], [stdout(pipe(In))]),
    read_string(In, _, X),
    term_to_atom(Y, X),
    write('GIAA Programming Language v1.0'), nl,
    write('SER 502 - Spring 2021 - Team 7'), nl,
    write('@Authors - Abhishek Mohabe, Apoorva Giliyal, Gautham Krishna, Itiparna Mahala'), nl, nl,
    program(Tree, Y, []),
    write('Executing......'), write(Filename), nl, nl,
    write('List of Tokens:'), nl, write(Y),nl, nl,
    write('Parse Tree:'), nl, write(Tree),nl, nl, write('Output:'), nl,
    eval_program(Tree, Output).

%----------------------------------------------------------------------------------------------------------------
:- table boolean/3, expression/3, term/3.

%to parse the program 
program(t_program(A)) -->['begin'], block(A), ['end'].

%to parse the block 
block(t_block(A)) --> ['{'], block_section(A), ['}']. 
block_section(t_block(A, B)) --> statements(A), block_section(B).
block_section(t_block(A)) --> statements(A).

%to parse the different type of statements 
statements(t_statements(X)) --> declaration(X), [;].
statements(t_statements(X)) --> assignment(X), [;].
statements(t_statements(X)) --> expression(X), [;].
statements(t_statements(X)) --> boolean(X), [;].
statements(t_statements(X)) --> printstatements(X), [;].
statements(t_statements(X)) --> ifcondition(X).
statements(t_statements(X)) --> ternarycondition(X), [;].
statements(t_statements(X)) --> forloop(X).
statements(t_statements(X)) --> whileloop(X).
statements(t_statements(X)) --> forrange(X).
statements(t_statements(X)) --> iterator(X), [;].

%to parse variable declaration
declaration(t_declareint(int, X, Y)) --> ['int'], identifier(X), ['='], expression(Y).
declaration(t_declarestr(string, X, Y)) --> ['string'], identifier(X), ['='], string(Y).
declaration(t_declarebool(bool, X, true)) --> ['bool'], identifier(X), [=], ['true'].
declaration(t_declarebool(bool, X, false)) --> ['bool'], identifier(X), [=], ['false'].
declaration(t_declare(X, Y)) --> type(X), identifier(Y).

%to parse assignment operation
assignment(t_assign(X, Y)) --> identifier(X), ['='], expression(Y).
assignment(t_assign(X, Y)) --> identifier(X), ['='], boolean(Y).

%to parse datatype
type(int) --> ['int'].
type(string) --> ['string'].
type(bool) --> ['bool'].

%to parse whileloop
whileloop(t_whileloop(A, B)) --> ['while'], ['('], (condition(A);boolean(A)), [')'], block(B).

%to parse forloop
forloop(t_forloop(A, B, C, D)) --> ['for'], ['('], declaration(A), [';'], (condition(B);boolean(B)), [';'], iterator(C), [')'], block(D).
forloop(t_forloop(A, B, C, D)) --> ['for'], ['('], declaration(A), [';'], (condition(B);boolean(B)), [';'], assignment(C), [')'], block(D).
forloop(t_forloop(A, B, C, D)) --> ['for'], ['('], assignment(A), [';'], (condition(B);boolean(B)), [';'], iterator(C), [')'], block(D).
forloop(t_forloop(A, B, C, D)) --> ['for'], ['('], assignment(A), [';'], (condition(B);boolean(B)), [';'], expression(C), [')'], block(D).

%to parse forrange
forrange(t_forrange(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], num(B), [':'], num(C), [')'], block(D).
forrange(t_forrange(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], identifier(B), [':'], identifier(C), [')'], block(D).
forrange(t_forrange(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], num(B), [':'], identifier(C), [')'], block(D).
forrange(t_forrange(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], identifier(B), [':'], num(C), [')'], block(D).

%to parse if condition
ifcondition(t_if_cond(A, B)) --> ['if'], ['('], (condition(A);boolean(A)), [')'], block(B).
ifcondition(t_if_cond(A, B, C)) --> ['if'], ['('], (condition(A);boolean(A)), [')'], block(B), ['else'], block(C).

%to parse ternary condition
ternarycondition(t_tern_cond(A, B, C)) --> (condition(A);boolean(A)), ['?'], statements(B), [':'], statements(C).

%to parse the boolean expression 
boolean(true) --> ['true'].
boolean(false) --> ['false'].
boolean(t_bool_not(X)) --> ['not'],['('], boolean(X), [')'].
boolean(t_bool_not(X)) --> ['not'],['('], condition(X), [')'].
boolean(t_bool_and(X, Y)) --> boolean(X), ['and'], boolean(Y).
boolean(t_bool_and(X, Y)) --> condition(X), ['and'], condition(Y).
boolean(t_bool_or(X, Y)) --> boolean(X), ['or'], boolean(Y).
boolean(t_bool_or(X, Y)) --> condition(X), ['or'], condition(Y).

%to parse print statements
printstatements(t_print(X)) --> ['print'], identifier(X).
printstatements(t_print(X)) --> ['print'], num(X).
printstatements(t_print(X)) --> ['print'], string(X).

%to parse condition checks
condition(t_condition(X, Y, Z)) --> expression(X), operator(Y), expression(Z).
condition(t_condition(X, Y, Z)) --> string(X), operator(Y), string(Z).
condition(t_condition(X, Y, Z)) --> identifier(X), operator(Y), string(Z).

%to parse conditional operator
operator(==) --> ['=='].
operator('!=') --> ['!='].
operator(>) --> ['>'].
operator(<) --> ['<'].
operator(>=) --> ['>='].
operator(<=) --> ['<='].

%to parse addition ,subtraction,multiplication and division
expression(t_add(X, Y)) --> expression(X), ['+'], term(Y).
expression(t_sub(X, Y)) --> expression(X), ['-'], term(Y).
expression(X) --> term(X).
term(t_mult(X, Y)) --> term(X), ['*'], term(Y).
term(t_div(X, Y)) --> term(X), ['/'], term(Y).
term(X) --> ['('], expression(X), [')'].
term(X) --> num(X).
term(X) --> identifier(X).

%to parse unary increment and decrement operation
iterator(t_incre(X)) --> identifier(X), ['+'], ['+'] .
iterator(t_decre(X)) --> identifier(X), ['-'], ['-'].

%to parse number, identifier, and string
num(t_num(Y)) --> [Y], {number(Y)}.
identifier(identifier(Y)) --> [Y], {atom(Y)}.
string(Y) --> onlystring(Y).
onlystring(t_str(Y)) --> [Y], {atom(Y)}.

check_type(Val, Temp) :- string(Val), Temp = string.
check_type(Val, Temp) :- integer(Val), Temp = int.
check_type(Val, Temp) :- (Val = true ; Val = false), Temp = bool.

not(true, false).
not(false, true).

and(false, _, false).
and(_, false, false).
and(true, true, true).

or(true, _, true).
or(_, true, true).
or(false, false, false).

%----------------------------------------------------------------------------------------------------------------

%lookup predicate find the respective values from the environment

lookup(Id, [(_Type, Id, Temp)|_], Temp).
lookup(Id, [_|Tail], Temp) :- lookup(Id, Tail, Temp).

lookup_type(Id, [_|Tail], Temp) :- lookup_type(Id, Tail, Temp).
lookup_type(Id, [(Type,Id,_X)|_], Type).

%update predicate updates the value of the identifier

update(Type, Id, Val, [], [(Type, Id, Val)]).
update(Type, Id, Val, [(Type, Id, _)|Tail], [(Type, Id, Val)|Tail]).
update(Type, Id, Val, [Head|Tail], [Head|Rest]) :- update(Type, Id, Val, Tail, Rest).

%----------------------------------------------------------------------------------------------------------------

%to evaluate the program
eval_program(t_program(X), FinalEnv) :- eval_block(X, [], FinalEnv), !.

%to evaluate the block
eval_block(t_block(X), Env, FinalEnv) :- eval_block_section(X, Env, FinalEnv).
eval_block_section(t_block(X, Y), Env, FinalEnv) :- eval_statements(X, Env, Env1), 
    eval_block_section(Y, Env1, FinalEnv).
eval_block_section(t_block(X), Env, FinalEnv) :- eval_statements(X, Env, FinalEnv).

%to evaluate the statements
eval_statements(t_statements(X), Env, FinalEnv) :- 
    eval_declare(X, Env, FinalEnv);
    eval_assign(X, Env, FinalEnv);
    eval_boolean(X, Env, FinalEnv, _Val);
    eval_print(X, Env, FinalEnv);
    if_eval(X, Env, FinalEnv);
    eval_while(X, Env, FinalEnv);
    eval_forloop(X, Env, FinalEnv);
    eval_forrange(X, Env, FinalEnv);
    eval_terncond(X, Env, FinalEnv);
    eval_iter(X, Env, FinalEnv).

%to evaluate different types of declaration
eval_declare(t_declare(X, Y), Env, NewEnv):- 
    eval_char_tree(Y, Id),
    update(X, Id, _, Env, NewEnv).
eval_declare(t_declareint(int, Y, Z), Env, NewEnv):- 
    eval_char_tree(Y, Id),
    eval_expr(Z, Env, Env1, Val),
    update(int, Id, Val, Env1, NewEnv).
eval_declare(t_declarestr(string, Y, Z), Env, NewEnv):- 
    eval_char_tree(Y, Id),
    eval_str(Z, Env, NewEnv1, Val),
    update(string, Id, Val, NewEnv1, NewEnv).
eval_declare(t_declarebool(bool, Y, true), Env, NewEnv):- 
    eval_char_tree(Y, Id),
    update(bool, Id, true, Env, NewEnv).
eval_declare(t_declarebool(bool, Y, false), Env, NewEnv):- 
    eval_char_tree(Y, Id),
    update(bool, Id, false, Env, NewEnv).

%to evaluate the assignment operation
eval_assign(t_assign(X, Y), Env, NewEnv) :- 
    eval_expr(Y, Env, Env1, Val),
    check_type(Val, T),
    eval_char_tree(X, Id),
    lookup_type(Id, Env1, T1),
    T =@= T1,
    update(T, Id, Val, Env1, NewEnv).
eval_assign(t_assign(X, Y), Env, NewEnv) :- 
    eval_str(Y, Env, Env, Val),
    check_type(Val, T),
    eval_char_tree(X, Id),
    lookup_type(Id, Env, T1),
    T =@= T1,
    update(T, Id, Val, Env, NewEnv).
eval_assign(t_assign(X, Y), Env, NewEnv) :- 
   eval_boolean(Y, Env, Env, Val),
    check_type(Val, T),
    eval_char_tree(X, Id),
   lookup_type(Id, Env, T1),
    T =@= T1,
    update(T, Id, Val, Env, NewEnv).

%to evaluate boolean condition
eval_boolean(true, _Env1, _NewEnv, true).
eval_boolean(false, _Env1, _NewEnv,false).
eval_boolean(t_bool_not(B), Env, NewEnv, Val) :- 
    (eval_boolean(B, Env, NewEnv, Val1);eval_condition(B, Env, NewEnv, Val1)), 
    not(Val1, Val2), 
    Val = Val2.
eval_boolean(t_bool_and(X, Y), Env, NewEnv, Val) :- 
    eval_boolean(X, Env, NewEnv, Val1),
    eval_boolean(Y, Env, NewEnv, Val2),
    and(Val1, Val2, Val).
eval_boolean(t_bool_and(X, Y), Env, NewEnv, Val) :- 
    eval_condition(X, Env, NewEnv, Val1),
    eval_condition(Y, Env, NewEnv, Val2), 
    and(Val1, Val2, Val).
eval_boolean(t_bool_or(X, Y), Env, NewEnv, Val) :- 
    eval_boolean(X, Env, NewEnv, Val1),
    eval_boolean(Y, Env, NewEnv, Val2),
    or(Val1, Val2, Val).
eval_boolean(t_bool_or(X, Y), Env, NewEnv, Val) :- 
    eval_condition(X, Env, NewEnv, Val1),
    eval_condition(Y, Env, NewEnv, Val2),
    or(Val1, Val2, Val).

%to evaluate conditional operation
eval_condition(t_condition(X, ==, Y), Env, NewEnv, Val) :- 
    eval_expr(X, Env, NewEnv, Val1),
    eval_expr(Y, Env, NewEnv, Val2),
    (( Val1 =:= Val2, Val = true); ( \+(Val1 =:= Val2), Val = false)).
eval_condition(t_condition(X, '!=', Y), Env, NewEnv, Val) :- 
    eval_expr(X, Env, NewEnv, Val1),
    eval_expr(Y, Env, NewEnv, Val2),
    (( Val1 =\= Val2, Val = true);( \+(Val1 =\= Val2), Val = false)).
eval_condition(t_condition(X, '>', Y), Env, NewEnv, Val) :-
    eval_expr(X, Env, NewEnv, Val1),
    eval_expr(Y, Env, NewEnv, Val2),
    (( Val1 > Val2, Val = true);( \+(Val1 > Val2), Val = false)).
eval_condition(t_condition(X, '<', Y), Env, NewEnv, Val) :- 
    eval_expr(X, Env, NewEnv, Val1),
    eval_expr(Y, Env, NewEnv, Val2),
    (( Val1 < Val2, Val = true);( \+(Val1 < Val2), Val = false)).
eval_condition(t_condition(X, '>=', Y), Env, NewEnv, Val) :- 
    eval_expr(X, Env, NewEnv, Val1),
    eval_expr(Y, Env, NewEnv, Val2),
    (( Val1 >= Val2, Val = true);( \+(Val1 >= Val2), Val = false)).
eval_condition(t_condition(X, '<=', Y), Env, NewEnv, Val) :- 
    eval_expr(X, Env, NewEnv, Val1),
    eval_expr(Y, Env, NewEnv, Val2),
    (( Val1 =< Val2, Val = true);( \+(Val1 =< Val2), Val = false)).
eval_condition(t_condition(X, ==, Y), Env, NewEnv, Val) :- 
    eval_str(X, Env, NewEnv, Val1),
    eval_str(Y, Env, NewEnv, Val2),
    ((Val1 = Val2, Val = true);(\+(Val1 = Val2), Val = false)).
eval_condition(t_condition(X,'!=',Y), Env, NewEnv, Val) :-
    eval_str(X, Env, NewEnv, Val1),
    eval_str(Y, Env, NewEnv, Val2),
    ((Val1 = Val2, Val = false);(\+(Val1 = Val2), Val = true)).
eval_condition(t_condition(X,'>',Y), Env, NewEnv,_Val) :- 
    eval_str(X, Env, NewEnv,_Val1),
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").
eval_condition(t_condition(X,'<',Y), Env, NewEnv,_Val) :- 
    eval_str(X, Env, NewEnv,_Val1),
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").
eval_condition(t_condition(X,'>=',Y), Env, NewEnv,_Val) :- 
    eval_str(X, Env, NewEnv,_Val1),
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").
eval_condition(t_condition(X,'<=',Y), Env, NewEnv,_Val) :- 
    eval_str(X, Env, NewEnv,_Val1),
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").
eval_condition(t_condition(X,==,Y), Env, NewEnv, Val) :-
    eval_char_tree(X,Id),
    lookup(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    eval_str(Y, Env, NewEnv, Val2),
    ((Val1 =@= Val2, Val = true);(\+(Val1 =@= Val2), Val = false)).
eval_condition(t_condition(X,'!=',Y), Env, NewEnv, Val) :- 
    eval_char_tree(X,Id),
    lookup(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    eval_str(Y, Env, NewEnv, Val2),
    ((Val1 = Val2, Val = false);(\+(Val1 = Val2), Val = true)).
eval_condition(t_condition(X,'>',Y), Env, NewEnv,_Val) :- 
    eval_char_tree(X,Id),
    lookup(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").
eval_condition(t_condition(X,'<',Y), Env, NewEnv,_Val) :- 
    eval_char_tree(X,Id),
    lookup(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").
eval_condition(t_condition(X,'>=',Y), Env, NewEnv,_Val) :- 
    eval_char_tree(X,Id),
    lookup(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").
eval_condition(t_condition(X,'<=',Y), Env, NewEnv,_Val) :- 
    eval_char_tree(X,Id),
    lookup(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").

%to evaluate the print statement
eval_print(t_print(X), Env, Env) :- 
    eval_char_tree(X,Id),
    lookup(Id, Env, Val),
    writeln(Val).
eval_print(t_print(X), Env, Env) :- 
    eval_numtree(X, Val),
    writeln(Val).
eval_print(t_print(X), Env, Env) :- 
    eval_str(X, Env, Env, Val),
    writeln(Val).

%to evaluate if condition
if_eval(t_if_cond(X,Y), Env,FinalEnv):- 
    ((eval_condition(X, Env, NewEnv,true);eval_boolean(X, Env, NewEnv,true)),eval_block(Y, NewEnv,FinalEnv)).
if_eval(t_if_cond(X,_Y), Env, NewEnv):- 
    eval_condition(X, Env, NewEnv,false);eval_boolean(X, Env, NewEnv,false).
if_eval(t_if_cond(X,Y,_Z), Env,FinalEnv):- 
    (eval_condition(X, Env, NewEnv,true);eval_boolean(X, Env, NewEnv,true)),
    eval_block(Y, NewEnv,FinalEnv).
if_eval(t_if_cond(X,_Y,Z), Env,FinalEnv):- 
    (eval_condition(X, Env, NewEnv,false);eval_boolean(X, Env, NewEnv,false)),
    eval_block(Z, NewEnv,FinalEnv).

%to evaluate the while loop
eval_while(t_whileloop(X,Y), Env,FinalEnv):- 
    eval_boolean(X, Env, NewEnv,true),
    eval_block(Y, NewEnv, NewEnv1),
    eval_while(t_whileloop(X,Y), NewEnv1,FinalEnv).
eval_while(t_whileloop(X,_Y), Env, Env) :- 
    eval_boolean(X, Env, Env,false).
eval_while(t_whileloop(X,Y), Env,FinalEnv):- 
    eval_condition(X, Env, NewEnv,true),
    eval_block(Y, NewEnv, NewEnv1),
    eval_while(t_whileloop(X,Y), NewEnv1,FinalEnv).
eval_while(t_whileloop(X,_Y), Env, Env) :- 
    eval_condition(X, Env, Env,false).

%to evaluate the forloop
eval_forloop(t_forloop(X,Y,Z,W), Env,FinalEnv):- 
    eval_declare(X, Env, NewEnv),
    loops(Y,Z,W, NewEnv,FinalEnv).
eval_forloop(t_forloop(X,Y,Z,W), Env,FinalEnv):- 
    eval_assign(X, Env, NewEnv),
    loops(Y,Z,W, NewEnv,FinalEnv).
loops(X,Y,Z, Env,FinalEnv) :- 
    eval_condition(X, Env, Env,true),
    eval_block(Z, Env, NewEnv),
    (eval_iter(Y, NewEnv, NewEnv1);eval_expr(Y, NewEnv, NewEnv1)),
    loops(X,Y,Z, NewEnv1,FinalEnv).
loops(X,_Y,_Z, Env, Env) :- 
    eval_condition(X, Env, Env,false).
loops(X,Y,Z, Env,FinalEnv) :- 
    eval_boolean(X, Env, Env,true),
    eval_block(Z, Env, NewEnv),
    (eval_iter(Y, NewEnv, NewEnv1);eval_expr(Y, NewEnv, NewEnv1)),
    loops(X,Y,Z, NewEnv1,FinalEnv).
loops(X,_Y,_Z, Env, Env) :- 
    eval_boolean(X, Env, Env,false).

%to evaluate the forrange
eval_forrange(t_forrange(X,Y,Z,W), Env,FinalEnv):- 
    eval_char_tree(X,Id),
    ((eval_numtree(Y, Val),update(int,Id, Val, Env, NewEnv));
    (lookup(Y, Env, Val),update(int,Id, Val, Env, NewEnv))),
    ((eval_numtree(Z,N));
    (eval_char_tree(Z,Id1),lookup(Id1, NewEnv,N))),
    looping(Id,N,W, NewEnv,FinalEnv).
looping(X,Z,W, Env,FinalEnv):- 
    lookup(X, Env, Val),
    Val < Z, 
    eval_block(W, Env, NewEnv),
    Val1 is Val + 1,
    update(int, X, Val1, NewEnv, NewEnv1),
    looping(X,Z,W, NewEnv1,FinalEnv).
looping(X,Z,_W, Env, Env) :- 
    lookup(X, Env, Val), 
    Val >= Z.

%to evaluate ternary condition
eval_terncond(t_tern_cond(X,Y,_Z), Env,FinalEnv):- 
    (eval_condition(X, Env, NewEnv,true);eval_boolean(X, Env, NewEnv,true)),
    eval_statements(Y, NewEnv,FinalEnv).
eval_terncond(t_tern_cond(X,_Y,Z), Env,FinalEnv):- 
    (eval_condition(X, Env, NewEnv,false);eval_boolean(X, Env, NewEnv,false)),
    eval_statements(Z, NewEnv,FinalEnv).

%to evaluate the increment,decrement operation
eval_iter(t_incre(X), Env, NewEnv) :- 
    eval_char_tree(X,Id),
    lookup_type(Id, Env,int),
    lookup(Id, Env, Val),
    Val1 is Val + 1, 
    update(int,Id, Val1, Env, NewEnv).
eval_iter(t_decre(X), Env, NewEnv) :- 
    eval_char_tree(X,Id),
    lookup_type(Id, Env,int),
    lookup(Id, Env, Val),
    Val1 is Val - 1, 
    update(int,Id, Val1, Env, NewEnv).

%to evaluate addition,subtraction,multiplication and division
eval_expr(X, Env, NewEnv) :- 
    eval_assign(X, Env, NewEnv).
eval_expr(X, Env, NewEnv, Val) :- 
    eval_term(X, Env, NewEnv, Val).
eval_expr(t_sub(X,Y), Env, NewEnv, Val):-
    eval_expr(X, Env, Env1, Val1),
    eval_term(Y, Env1, NewEnv, Val2),
    Val is Val1 - Val2.
eval_term(X, Env, NewEnv, Val) :- 
    eval_term1(X, Env, NewEnv, Val).
eval_term(t_add(X,Y), Env, NewEnv, Val):-
    eval_term(X, Env, Env1, Val1),
    eval_term1(Y, Env1, NewEnv, Val2),
    Val is Val1 + Val2.
eval_term1(X, Env, NewEnv, Val) :- 
    eval_term2(X, Env, NewEnv, Val).
eval_term1(t_mult(X,Y), Env, NewEnv, Val):-
    eval_term1(X, Env, Env1, Val1),
    eval_term2(Y, Env1, NewEnv, Val2),
    Val is Val1 * Val2.
eval_term2(X, Env, NewEnv, Val) :- 
    eval_term3(X, Env, NewEnv, Val).
eval_term2(t_div(X,Y),  Env, NewEnv, Val):-
    eval_term2(X, Env, Env1, Val1), 
    eval_term3(Y, Env1, NewEnv, Val2),
    Val is floor(Val1 / Val2).
eval_term3(X,  Env, NewEnv, Val) :- 
    eval_num(X, Env, NewEnv, Val).
eval_term3(t_parentheses(X), Env, NewEnv, Val):-
    eval_expr(X, Env, NewEnv, Val).

%to evaluate the number and string
eval_num(t_num(Val), Env, Env, Val).
eval_num(identifier(I), Env, Env, Val) :-
    term_to_atom(Id,I),
    lookup(Id, Env, Val).
eval_numtree(t_num(Val), Val).
eval_char_tree(identifier(I),Id):- 
    term_to_atom(Id,I).
eval_str(t_str(I), Env, Env, Val) :- 
    atom_string(I, Val).
