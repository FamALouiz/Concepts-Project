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

/*
This predicate is used to get a list of all the days and weeks in which a specific group has scheduled slots.

Arguments:
Group: The group for which to find the scheduled days.
L: The resulting list of `day_timing(Week, Day)` structures

Example usage:
?- group_days(met_4, Day_Timings).
Day_Timings = [day_timing(1, mon), day_timing(1, tue), day_timing(1, wed), day_timing
(1, sat), day_timing(2, mon), day_timing(2, tue), day_timing(2, wed), day_timing
(2, thu), day_timing(..., ...)|...].
*/
group_days(Group, L):-
    findall(day_timing(Week, Day), scheduled_slot(Week, Day, _, _, Group), L1),
    list_to_set(L1, L).
