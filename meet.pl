#!/bin/gprolog --consult-file

:- include('data.pl').
:- include('uniq.pl').

% lte predicate for comparing time
% am vs am or pm vs pm
lte(time(H1, M1, Period), time(H2, M2, Period)) :-
    (H1 < H2 ; (H1 = H2, M1 =< M2)).
% am vs pm
lte(time(_, _, am), time(_, _, pm)).

% overlap finds overlapping time slots between 2 and returns the overlapping interval
overlap(slot(StartTime1, EndTime1), slot(StartTime2, EndTime2), slot(OverlapStart, OverlapEnd)) :-
    lte(StartTime1, EndTime2),
    lte(StartTime2, EndTime1),
    % Find the overlapping interval and set to overlapping slot
    (lte(StartTime1, StartTime2) -> OverlapStart = StartTime2 ; OverlapStart = StartTime1),
    (lte(EndTime1, EndTime2) -> OverlapEnd = EndTime1 ; OverlapEnd = EndTime2),
    OverlapStart \== OverlapEnd. % Avoid zero-length meetings

% checkTwoOverlap test predicate to compare two peoples schedules
checkTwoOverlap(Person1, Person2, OverlappingSlot) :- 
    Person1 \== Person2,
    free(Person1, FreeSlot1),
    free(Person2, FreeSlot2),
    overlap(FreeSlot1, FreeSlot2, OverlappingSlot).

% meet finds meeting times that overlap SlotToLookFor
meet(ReturnSlot) :-
    people(People),
    checkPeopleOverlap(People, OverlappingSlot),
    ReturnSlot = OverlappingSlot.
/*
compareSlots(Slot1, Slot2, ReturnSlot) :-
    Slot1 \== Slot2,
    Slot1 = slot(StartTime1, EndTime1),
    Slot2 = slot(StartTime2, EndTime2),
    (lte(StartTime1, StartTime2) -> OverlapStart = StartTime2 ; OverlapStart = StartTime1),
    (lte(EndTime1, EndTime2) -> OverlapEnd = EndTime1 ; OverlapEnd = EndTime2),
    OverlapStart \== OverlapEnd. % Avoid zero-length*/

checkPeopleOverlap([], _). % base case
checkPeopleOverlap([Person1], ReturnSlot) :- % one person left in list - make sure they are free
    free(Person1, FreeSlot),
    overlap(FreeSlot, ReturnSlot, OverlappingSlot).
    % todo: compare times and make the overlapping slot the returnslot if it is smaller
    
checkPeopleOverlap([Person1, Person2 | RestPeople], ReturnSlot) :- % see if the people in the list overlap
    Person1 \== Person2,
    free(Person1, FreeSlot1),
    free(Person2, FreeSlot2),
    overlap(FreeSlot1, FreeSlot2, ReturnSlot),
    checkPeopleOverlap(RestPeople, ReturnSlot).


people([ann, bob, carla]). % starting people to check schedules for

% Main function
main :- findall(Slot, meet(Slot), Slots),
        uniq(Slots, Uniq),
        write(Uniq), nl, halt.

:- initialization(main).
