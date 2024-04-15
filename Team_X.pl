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
holds if Slot is the earliest slot for Group on Day of week Week.
Gets The Head of the list Slots that is the output of day_slots/4 (assumes that the list of slots is already sorted in ascending order of time)
*/
earliest_slot(Group, Week, Day, Slot) :-
    day_slots(Group, Week, Day, [Slot|_]).

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



 /*proper_connection(Station_A, Station_B, Duration, Line) holds if Station_A and Station_B are
connected on Line, and the time to go between them is Duration. All while taking into consideration
the bidirectionality or lack thereof of Line.
*/

proper_connection(Station_A, Station_B, Duration, Line):-
    \+ unidirectional(Line),
    (connection(Station_A, Station_B, Duration, Line);connection(Station_B, Station_A, Duration, Line)).
proper_connection(Station_A, Station_B, Duration, Line):-
    unidirectional(Line),
    connection(Station_A, Station_B, Duration, Line).



/*mins_to_twentyfour_hr/3
mins_to_twentyfour_hr(Minutes, TwentyFour_Hours, TwentyFour_Mins) holds if
TwentyFour_Hours: TwentyFour_Mins is the twenty-four hour representation of Minutes since midnight.*/

mins_to_twentyfour_hr(Minutes, TwentyFour_Hours, TwentyFour_Mins):-
    TwentyFour_Hours is (Minutes//60) mod 24,
    TwentyFour_Mins is Minutes mod 60.
/*
twentyfour_hr_to_mins(TwentyFour_Hours, TwentyFour_Mins, Minutes) holds if Minutes since midnight is equivalent to the twenty-four hour formatted TwentyFour_Hours:TwentyFour_Mins.
*/

twentyfour_hr_to_mins(TwentyFour_Hours, TwentyFour_Mins, Minutes) :-
    Minutes is TwentyFour_Hours * 60 + TwentyFour_Mins.