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
sorts the Slots list from day_slots/4 and gets the head of the list.
*/
earliest_slot(Group, Week, Day, Slot) :-
    day_slots(Group, Week, Day, Slots), 
    sort(Slots, SortedSlots), 
    SortedSlots = [Slot|_].

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
    (connection(Station_A, Station_B, Duration, Line);connection(Station_B, Station_A, Duration, Line)),
    \+ unidirectional(Line).
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


append_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line, [], [route(Conn_Line,Conn_Source,Conn_Destination,Conn_Duration)]):-
    proper_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line).

append_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line, Routes_So_Far, Routes):-
    proper_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line),
    R1=[route(Conn_Line,Conn_Source,Conn_Destination,Conn_Duration)],
    reverse(Routes_So_Far, [H|T]),
    H=route(Conn_Line1,_,_,_),
    \+ Conn_Line=Conn_Line1,
    reverse([H|T], R),
    app(R,R1,Routes).

append_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line, Routes_So_Far, Routes):-
    proper_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line),
    reverse(Routes_So_Far, [H1|T]),
    H1=route(Conn_Line,Start,Conn_Source,Duration2),
    H=route(Conn_Line, Start, Conn_Destination, Duration),
    \+ Start=Conn_Source,
    \+ Start=Conn_Destination,
    Duration is Duration2 + Conn_Duration,
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

len([], 0).
len([_|T], N) :- len(T, N1), N is N1 + 1.

/*
Gives a list of routes where the combined Duration of Routes does not exceed Max_Duration, and the number
of Routes does not exceed Max_Routes.
Connected calls a connected_temp with an extra variable to store the till now total duration of routes
*/
connected(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes):-
    connected_temp(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes, 0, [], []).

connected(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes, _, _):-
    connected_temp(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes, 0, [], []).

connected_temp(Source, Source, _, _, Max_Duration, Max_Routes, Duration, Routes, Duration, Routes, _):-
    length(Routes, Length),
    Length =< Max_Routes,
    Duration =< Max_Duration, !.

connected_temp(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes, Temp_Duration, Temp_Ans, Temp_Routes):-
    Source \= Destination,
    len(Temp_Ans, Length),
    Length =< Max_Routes,
    line(Transportation, Line),
    \+ (strike(Line, Week, Day)),
    append(Temp_Routes, [Source], Temp_Routes_New),
    proper_connection(Source, Intermediate, Duration_Added, Transportation),
    \+ (member(Intermediate, Temp_Routes_New)),
    New_Duration is Temp_Duration + Duration_Added,
    New_Duration =< Max_Duration,
    Source\=Intermediate,
    append_connection(Source, Intermediate, Duration_Added, Transportation, Temp_Ans, Routes_New),
    connected_temp(Intermediate, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes, New_Duration, Routes_New,  Temp_Routes_New).



% Travel Plan calls helper procedure with an extra variable Day timings which is a list of day_timing(Week, Day) so to recursive call every home station with every day in a week for whole semester
travel_plan([],_,_,_,[],[]).

travel_plan([H|T], Group, Max_Duration, Max_Routes, Journeys):-
    group_days(Group, Day_timings),
    travel_planHelper([H|T], Group, Max_Duration, Max_Routes, Day_timings,Journeys).

travel_planHelper(_,_,_,_,[],[]).
travel_planHelper(Home_Stations, Group, Max_Duration, Max_Routes, [day_timing(Week, Day)|T1],Journeys):-
    earliest_slot(Group, Week, Day, Slot_Num),
    slot_to_mins(Slot_Num, Minutes),
    campus_reachable(Campus),
    member(Station, Home_Stations),
    connected(Station,Campus, Week, Day, Max_Duration, Max_Routes, Duration, Routes),
    Remaining_Minutes = Minutes - Duration,
    mins_to_twentyfour_hr(Remaining_Minutes, TwentyFour_Hours, TwentyFour_Mins),
    travel_planHelper(Home_Stations,Group, Max_Duration, Max_Routes,T1, Till_now_Journeys),
    append([journey(Week,Day, TwentyFour_Hours, TwentyFour_Mins, Duration, Routes)],Till_now_Journeys,Journeys).
