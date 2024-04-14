% This line consults the knowledge bases from this file,
% instead of needing to consult the files individually.
% This line MUST be included in the final submission.
:- ['transport_kb', 'slots_kb'].
:- use_module(library(lists)).

/*
gets a list of all slots of a specific group in a certain week and day
*/
day_slots(Group, Week, Day, Slots):-
	findall(X, scheduled_slot(Week, Day, X, _, Group), Slots).

group_days(Group, L):-
    findall(day_timing(Week, Day), scheduled_slot(Week, Day, _, _, Group), L1),
    list_to_set(L1, L).
