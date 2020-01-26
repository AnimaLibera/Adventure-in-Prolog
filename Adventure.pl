room(kitchen).
room(office).
room(hall).
room('dining room').
room(cellar).

:- dynamic location/2.
location(desk, office).
location(apple, kitchen). 
location(flashlight, desk).
location(flashlight, office).
location('washing machine', cellar).
location(nani, 'washing machine').
location(broccolie, kitchen).
location(crackers, kitchen).
location(computer, office).

:- dynamic location_s/2.
location_s(object(candle, red, small, 1), kitchen).
location_s(object(apple, red, small, 1), kitchen).
location_s(object(apple, green, small, 1), kitchen).
location_s(object(table, blue, big, 50), kitchen).

:- dynamic door/3.
door(office, hall, close).
door(kitchen, office, close).
door(hall, 'dining room', close).
door(kitchen, cellar, open).
door('dining room', kitchen, close).

edible(apple).
edible(crackers).
taste_yucky(broccolie).

:- dynamic turned_on/1.
turned_on().

:- dynamic turned_off/1.
turned_off(flashlight).

:- dynamic here/1.
here(kitchen).

:- dynamic have/1.
have().

connect(X,Y,Door) :- door(X,Y,Door).
connect(X,Y,Door) :- door(Y,X,Door).

%Does not work well.
command_loop :-
	write('Welcome to Nani Search.'), nl,	
	repeat,
	write('>nani> '),
	read(X),
	puzzle(X),
	do(X), nl,
	end_condition(X).

do(goto(X)) :- goto(X),!.
do(go(X)) :- goto(X),!.
do(inventory) :- inventory,!.
do(look) :- look,!.
do(take(X)) :- take(X),!.
do(end).
do(_) :- write('Invalid Command.').

end_condition(end).
end_condition(_) :-
	have(nani),
	write('Congraulations').

list_things(Place) :-
	location(X, Place),
	tab(2),
	write(X),
	nl,
	fail.
list_things(_).

list_things_s(Place):-
	location_s(object(Thing, Color, Size, Weight), Place),
	write('A '), write(Size), tab(1),
	write(Color), tab(1),
	write(Thing), write(', weighting '),
	write_weight(Weight), nl,
	fail.
list_things_s(_).

write_weight(1):-
	write('1 pound.').
write_weight(W):-
	W > 1,
	write(W), write(' pounds.').

list_connections(Place) :-
	connect(Place, X, Door),
	tab(2),
	write(X),
	tab(2),
	write(Door),
	nl,
	fail.
list_connections(_).

look :-
	here(Place),
	write('You are in the '), write(Place),
	nl,
	write('You can see:'), nl,
	list_things(Place),
	list_things_s(Place),
	write('You can go to:'), nl,
	list_connections(Place).


%op(30,fx,look_in).
look_in(Place) :-
	location(X, Place),
	write(X), nl,
	fail.
look_in(Place) :-
	location_s(X, Place),
	write(X), nl,
	fail.
look_in(_).

%op(30, fx, 'goto').
goto(Place) :-
	puzzle(goto(Place)),
	can_go(Place),
	move(Place),
	look.

can_go(Place) :-
	here(X),
	connect(X, Place, open).
can_go(Place) :-
	here(X),
	connect(X, Place, close),
	write('Door is closed.'), nl,
	fail.
can_go(Place) :-
	write('You can''t get there from here.'), nl,
	fail.

move(Place) :-
	retract(here(X)),
	asserta(here(Place)).

puzzle(goto(cellar)) :-
	have(flashlight),
	turned_on(flashlight),
	!.
puzzle(goto(cellar)) :-
	write('Scard of Darkness.'),
	!, fail.
puzzle(_).


%Muss beide Richtungen der Verbindung beachten.
open(Place) :-
	here(X),
	retract(door(X, Place, close)),
	assertz(door(X, Place, open)).
open(Place) :-
	here(X),
	retract(door(Place, X, close)),
	assertz(door(Place, X, open)).
open(_) :-
	write('Door not opend.').

%Muss beide Richtungen der Verbindung beachten.
close_door(Place) :-
	here(X),
	retract(door(X, Place, open)),
	assertz(door(X, Place, close)).
close_door(Place) :-
	here(X),
	retract(door(Place, X, open)),
	assertz(door(Place, X, close)).
close_door(_) :-
	write('Door not closed.').

take(X) :-
	can_take(X),
	take_object(X).
take(X) :-
	can_take_s(X),
	take_object_s(X).

can_take(Thing):-
	here(Place),
	location(Thing, Place).
can_take(Thing):-
	write('There is no '),
	write(Thing),
	write(' to take.'),
	nl, fail.

can_take_s(Thing):-
	here(Room),
	location_s(object(Thing,_,small,_),Room).
can_take_s(Thing):-
	here(Room),
	location_s(object(Thing,_,big,_),Room),
	write('The '), write(Thing),
	write(' is to big to carry.'), nl,
	fail.
can_take_s(Thing):-
	here(Room),
	not(location_s(object(Thing,_,_,_),Room)),
	write('There is no '), write(Thing),
	write(' here.'), nl,
	fail.

take_object(X) :-
	retract(location(X,_)),
	asserta(have(X)),
	write('taken'), nl.

take_object_s(X) :-
	retract(location_s(object(X,_,_,_),_)),
	asserta(have(object(X,_,_,_))),
	write('taken'), nl.	

put(X) :-
	can_put(X),
	put_object(X).

can_put(Thing) :-
	have(Thing).
can_put(Thing) :-
	write('There is no '),
	write(Thing),
	write(' to put.'), nl.

put_object(X) :-
	here(Place),
	retract(have(X)),
	assertz(location(X, Place)),
	write('puted').

inventory :-
	write('Inventory:'), nl,
	list_have().

list_have :-
	have(X),
	tab(2),
	write(X),
	nl,
	fail.
list_have.

turn_on :-
	have(flashlight),
	retract(turned_off(flashlight)),
	asserta(turned_on(flashlight)).

turn_off :-
	have(flashlight),
	retract(turned_on(flashlight)),
	asserta(turned_off(flashlight)).

light :-
	have(flashlight),
	turned_on(flashlight),
	write('Flashlight is on.').
light :-
	have(flashlight),
	turned_off(flashlight),
	write('Flashlight is off.').
light :-
	write('Don''t have Flashlight.').