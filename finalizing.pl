:- use_module(library(http/http_server)).
:- use_module(library(pce)).
:- use_module(contrib(contrib)).
:- use_module(library(persistent_frame)).



t1 :-
        new(Dialog, dialog('Shortest Path')),
        send_list(Dialog, append,
                  [ new(N1, text_item(origin)),
                    new(N2, text_item(destination)),
                    button(cancel, message(Dialog, destroy)),
                    button(enter, and(message(@prolog,
                                              find_shortest_path,
                                              N1?selection,
                                              N2?selection)
                                     ))
                  ]),
        send(Dialog, default_button, enter),
        send(Dialog, open).




:- initialization
    http_server([port(8080)]).
:- module(html_handlers).
:- http_handler('/values',say_something('Hello'), [priority(100)]).
:- http_handler('/findall',say_something_else('These are all the '), [priority(100)]).



say_something_else(What, Request) :-
	        http_parameters(Request,
                        [ title(Title, [ default('The Default Title') ]),
			  origin(Origin, [ length >= 2 ]),
			  destination(Destination, [ length >= 2 ])
                        ]),
	reply_html_page(
	    [title(Title)],
	    [

		p(['These are all the paths that exist: '])
	    ]),
	find_all(Origin,Destination).



say_something(What, Request) :-
	format('Content-type: text/plain~n~n'),
	format('~w User!, there exists the following paths in the system: ~n', [What]),
	format('\n 1) E11                  2) F11'),
	format('\n 3) G11                  4) F6'),
	format('\n 5) F8                   6) F10'),
	format('\n 7) D12'),
	format('\n\nIn order to search for a path, use the following link and change path accordingly:'),
	format('\nlocalhost:8080/params?origin=E11&destination=F6').





area(1,'E11').
area(2,'F11').
area(3,'G11').
area(4,'F6').
area(5,'F8').
area(6,'F10').
area(7,'D12').


/* Distance between areas */
distance(1,2,4).    /* From E11 to F11 */
distance(1,7,3).
distance(1,3,6).
distance(2,4,11).
distance(2,5,8).
distance(3,7,9).
distance(3,6,3).
distance(3,4,12).
distance(5,7,12).
distance(4,6,10).
distance(4,5,6).

/* Heuristic that was used                */
/* distance with straight line (assumed) */
h(1,3).
h(2,2).
h(3,5).
h(4,10).
h(5,7).
h(6,8).
h(7,2).
h(8,5).

/* Road connecting areas */
road(1,2,'Service Road, E Main Double Road').
road(1,3,'Ahmad Fraz Road Service').
road(2,4,'Jinnah Avenue').
road(4,5,'Main Margala Service Road').
road(3,4,'Jinnah Avenue').
road(3,7,'Ahmad Fraz Service Road').
road(3,6,'Hammad-uddin Road').
road(5,7,'Main Margalla Service Road').
road(2,5,'Nazi ud Road Jinnah Avenue').
road(4,6,'Nazi ud Road Jinnah Avenue').


shortest_path:-
write('Enter your location: '),nl,read(X),
    write('Enter destination: '), read(Y),
    find_shortest_path(X,Y).

find_shortest_path(Origin, Destination):-
	area(C1,Origin),
	area(C2,Destination),
	a_star([[0,C1]],C2,ReversePath),
	reverse(ReversePath, Path),
	write('Shortest Path: '),
	print_path(Path,Roads),
	write('.  Route: '),print_roads(Roads),!.

/* This message will be shown if the origin or destination were not typed correctly */
find_shortest_path(_,_):- write('There was an error with origin or destination city, please type again').



all_path:-
write('Enter your location: '),nl,read(X),
    write('Enter destination: '), read(Y),
    find_all(X,Y).



find_all(Origin, Destination):-
	area(C1,Origin),
	area(C2,Destination),
	a_star([[0,C1]],C2,ReversePath),
	reverse(ReversePath, Path),

        write('A Path was found: '), print_path(Path,Roads),
	write('Road to be traveled will be: '),print_roads(Roads),fail.
find_all(_,_):- write('That is all!').

a_star(Paths, Dest, [C,Dest|Path]):-
	member([C,Dest|Path],Paths),
	decide_best(Paths, [C1|_]),
	C1 == C.
a_star(Paths, Destination, BestPath):-
	decide_best(Paths, Best),
	delete(Paths, Best, PreviousPaths),
	expand_border(Best, NewPaths),
	append(PreviousPaths, NewPaths, L),
	a_star(L, Destination, BestPath).

decide_best([X],X):-!.
decide_best([[C1,Ci1|Y],[C2,Ci2|_]|Z], Best):-
	h(Ci1, H1),
	h(Ci2, H2),
	H1 +  C1 =< H2 +  C2,
	decide_best([[C1,Ci1|Y]|Z], Best).
decide_best([[C1,Ci1|_],[C2,Ci2|Y]|Z], Best):-
	h(Ci1, H1),
	h(Ci2, H2),
	H1  + C1 > H2 +  C2,
	decide_best([[C2,Ci2|Y]|Z], Best).


expand_border([Cost,Area|Path],Paths):-
	findall([Cost,NewArea,Area|Path],
		(distance(Area, NewArea,_),
		not(member(Area,Path))),
		L),
	change_costs(L, Paths).

change_costs([],[]):-!.
change_costs([[Total_Cost,Ci1,Ci2|Path]|Y],[[NewCost_Total,Ci1,Ci2|Path]|Z]):-
	distance(Ci2, Ci1, Distance),
	NewCost_Total is Total_Cost + Distance,
	change_costs(Y,Z).


print_path([Cost],[]):- nl, write('. Path: '), write(Cost), write(' kilometers'),nl.
print_path([Area,Cost],[]):- area(Area, Name), write(Name), write(' '), nl, write('.   Path: '), write(Cost), write(' kilometers'),nl.
print_path([Area,Area2|Y],Roads):-
	area(Area, Name),
	road(Area,Area2,Road),
	append([Road],R,Roads),
	write(Name),write(', '),
	print_path([Area2|Y],R).

print_roads([X]):- write(X), nl, nl.
print_roads([X|Y]):-
	write(X),write(' - '),
	print_roads(Y).



:- module(handle_params, []).
/** <module> demo of parameter conversion and validation
*/

%  needed to make termerized html
:- use_module(library(http/html_write)).
%  needed for http_handler
:- use_module(library(http/http_dispatch)).
%  needed to handle parameters
:- use_module(library(http/http_parameters)).

% a normal handler
:- http_handler(root(params), param_handler , []).



param_handler(Request) :-
	        http_parameters(Request,
                        [ title(Title, [ default('The Default Title') ]),
			  origin(Origin, [ length >= 2 ]),
			  destination(Destination, [ length >= 2 ])
                        ]),
	reply_html_page(
	    [title(Title)],
	    [

		p(['To find all paths, click the following link:']),
		p(a(href('/findall'+[origin=Origin, destination=Destination]), 'For all paths'))
	    ]),
	find_shortest_path(Origin,Destination).






























