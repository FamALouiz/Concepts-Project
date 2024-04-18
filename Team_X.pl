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
This predicate holds if Day_Timings is a list of all the days and weeks in which a specific group has scheduled slots.


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

/*append_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line, Routes_So_Far
, Routes) holds if Routes (see section 3) is the result of appending a connection from Conn_Source to
Conn_Destination on Conn_Line which takes Conn_Duration minutes, to Routes_So_Far.
*/
app([],L,L).
app(L1,L2,L):-
    L1=[H|T], L=[H|T1], app(T,L2,T1).

append_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line, Routes_So_Far, Routes):-
    proper_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line),
    R1=route(Conn_Line,Conn_Source,Conn_Destination,Conn_Duration),
    reverse(Routes_So_Far, [H|T]),
    H=route(Conn_Line1,_,_,_),
    \+ Conn_Line=Conn_Line1,
    reverse([H|T], R),
    app(R,R1,Routes).
append_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line, Routes_So_Far, Routes):-
    proper_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line),
    reverse(Routes_So_Far, [H1|T]),
    H1=route(Conn_Line, Start, Conn_Source, Duration1),
    H=route(Conn_Line, Start, Conn_Destination, Duration),
    Duration is Duration1 + Conn_Duration,
    reverse([H|T], Routes).

/*slot_to_mins(Slot_Num, Minutes) holds if Minutes since midnight is equivalent to the start time of a
slot whose number is Slot_Num
*/
slot_to_mins(Slot_Num, Minutes):-
    slot(Slot_Num, Start_Hour, Start_Minute),
    Minutes is (Start_Hour * 60)+Start_Minute.






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

% ============================================================================================







% ============================================================================================


connected(Source, Source, _, _, _, _, _, []).

connected(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, _):-
    connected(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, [], 0).
    

conntected(Source, Source, _, _, _, _, Duration, _, Duration).

% Assuming the connection is only 1 line
connected(Source, Destination, Week, Day, Max_Duration, Max_Routes, _, Routes, Temp_Duration):-
    length(Routes) =< Max_Routes,
    Temp_Duration =< Max_Duration,
    not(strike(Transportation, Week, Day)), 
    proper_connection(Source, X, Duration_added, Transportation),  
    New_Duration is Temp_Duration + Duration_added,
    append_connection(Source, X, Duration_added, Transportation, Routes, New_Routes),
    connected(X, Destination, Week, Day, Max_Duration, Max_Routes, New_Duration, New_Routes). 

