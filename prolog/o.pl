:- op(250,xfy,'::').
% Assumption: a


lf(a::label(1)).
lf(b:a).
lf(c::ref(1)).

p_member(X,X).
p_member(X, X::_).
p_member(X,_::Y) :- p_member(X,Y).

is_dep(reason(_)).
is_dep(assumption(_)).

deps(A,L) :- (
  is_dep(A) -> L = [A]
;
  L = []).

deps(A::B, L) :-
  (is_dep(A) ->
    L = [A|B_deps]
  ;
    L = B_deps
  ),
  deps(B,B_deps).


mf(X,Label,Deps):-
  lf(Y),
  Y = [X|_],
  (p_member(label(Label),Y) -> True; Label = 'none'),
  p_filt(Y,is_dep(Y),L).
