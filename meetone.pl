#!/bin/gprolog --consult-file

:- include('data.pl').

% lte predicate for comparing time
% am vs am or pm vs pm
lte(time(H1, M1, Period), time(H2, M2, Period)) :-
    (H1 < H2 ; (H1 = H2, M1 =< M2)).
% am vs pm
lte(time(_, _, am), time(_, _, pm)).

% isAvailable takes a person and sees if they are available in the given time 
isAvailable(Person, StartTime, EndTime) :-
    free(Person, slot(AvailableStartTime, AvailableEndTime)),
    lte(AvailableStartTime, StartTime),
    lte(EndTime, AvailableEndTime).

% meetone gets a person and sees if they are available in that time
meetone(Person, MeetingSlot) :-
    MeetingSlot = slot(StartTime, EndTime),
    isAvailable(Person, StartTime, EndTime).

% Main function
main :- findall(Person, 
        meetone(Person,slot(time(8,30,am),time(8,45,am))), 
        People),
    write(People), nl, halt.

:- initialization(main).