% type checking for an ad-hoc language of typed terms.
% refusing to type a bare atom or string


deep_term(X) :- nonvar(X), X =.. [_,_|_].

gen(A,A).
gen(A,B):- ty(A,B).
gen(A,B):- ty(A,C), gen(C,B).
gen(A,B):- deep_term(A),
	deep_term(B),
	A =.. LA,
	B =.. LB,
	gen_L(LA, LB).

gen_L([],[]).
gen_L([H|T], [GH|GT]) :-
	gen(H,GH),
	gen_L(T,GT).


ty(int, num).
ty(float,num).
ty(s_num,num).

ty(year,time).
ty(year_month,time).
ty(dur(time,time), time).



type(X,num) :- number(X).
type(unh(_), _).

type(y(_), year).
type(ym(_,_), year_month).
type(ymd(_,_,_), year_month_day).
type(now, time).
type(before, time).

type(we, actor).
type(co(X), actor) :- atom(X).
type(true,decl).
type(is(_,_),decl).
type(X,str) :- nonvar(X), (string(X); atom(X)).

decl(prop(_,_,_), t([actor, decl, time], top)).
decl(action(_,_), t([actor, decl], top)).
decl(action(_,_,_), t([actor,decl,time], top)).


tyc_top(P) :-
	nonground(P,_),
	numbervars(P,0,_),
	write('  *** WARNING:non ground'(P)),
	write('  ***  '),
	fail.
tyc_top(P) :- 
	deep_term(P),
	decl(P,t(ArgTypes,top)),
	P =.. [_|Args],
	tyc_list(Args,ArgTypes).

%tyc_list(A,B) :- writeln(checking(A,B)),fail.
tyc_list([],[]).
tyc_list([H|T],[TypeH|TypeT]) :-
	(type(H,TH) ->
	(gen(TH,  TypeH) ->
	
	   tyc_list(T,TypeT)
	;
	   numbervars(H,0,_),
	   writeln('Type Error'-(H,'has type:',TH,' expected:', TypeH)),
	   fail
	   )
	  ;
	 writeln('*** Error: no type for'(H)),
	 fail).

	

test :- test(X), (write(testing(X)), once(X), write(':ok'), fail; writeln('.'),fail).
test.

test(gen(_,num)).
test(gen(dur(_,_), time)).
test(gen(time,num)).
test(tyc_top(prop(_,_,_))).
test(tyc_top(prop(we,is(frog,amphibian), now))).
test(tyc_top(prop(we,is(frog,amphibian), anytime_dude))).



