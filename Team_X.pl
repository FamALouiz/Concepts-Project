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
append_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line, [], [route(Conn_Line,Conn_Source,Conn_Destination,Conn_Duration)]):-
    proper_connection(Conn_Source, Conn_Destination, Conn_Duration, Conn_Line).

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

/*conencted/10 holds if possible to reach specific destination
from Source on a specific day taking in consideration that combined
duration doesn't exceed a maximum duration and number of routes
doesn't exceed a maximum number of routes and gives a list of 
previous stations which shouldn't be visited again and the 
traversed routes so far before reaching the Source*/

connected/10(Source,Destination, Week, Day, Max_Duration, Max_Routes, Duration, Prev_Stations, Routes_So_Far, Routes):-

    connected(amrumer_str, leopoldplatz, 1, mon, 2, 1, 2, [westhafen], [route(u9,
        westhafen, amrumer_str, 1)], [route(u9, westhafen, leopoldplatz, 3)]).

append_connection(friedrichstr, oranienburger_tor, 1, u6, [route(u5, schillingstr,
alexanderplatz, 2), route(s5, alexanderplatz, friedrichstr, 4)], Routes).
Routes = [route(u5, schillingstr, alexanderplatz, 2), route(s5, alexanderplatz,
friedrichstr, 4), route(u6, friedrichstr, oranienburger_tor, 1)].
proper_connection(albertinenstr, antonplatz, 1, m4).
• day_timing(Week, Day) encodes a timing on Day of week Week.

route(Line, Start_Station, End_Station, Duration)
journey(Week_Num, Week_Day, Start_Hour, Start_Minute, Total_Duration, Routes) encodes
a journey on Week_Day of week Week_Num which starts at Start_Hour:Start_Minute, takes a
total duration of Total_Duration minutes, and consist of taking Routes.

line(Line_Name, Line_Type) indicates that Line_Name is of type Line_Type. Example: line(u6 ubahn),
line(s5, sbahn).
• unidirectional(Line) indicates that Line is unidirectional (i.e. connections between stations are
one way). Example: unidirectional(s42).
• campus_reachable(Station) indicates that campus is reachable from Station.
Example: campus_reachable(borsigwerke).
• strike(Line_Type, Week_Num, Week_Day) indicates that there is a strike on Week_Day of a week
Week_Num for all lines of Line_Type. Example: strike(ubahn, 3, wed).
• connection(Station_A, Station_B, Duration, Line) indicates that Station_A is connected to
Station_B on Line, and the time to go between them is Duration.
Example: connection(hermannplatz, rathaus_neukoelln, 1, u7).


% ============================================================================================



connected(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes):-
    connected(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes, 0, []).

connected(Source, Source, _, _, _, _, Duration, Routes, Duration, Routes).

connected(Source, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes, Temp_Duration, Temp_Routes):-
    length(Temp_Routes, Length),
    Length =< Max_Routes,
    Temp_Duration =< Max_Duration,
    proper_connection(Source, Intermediate, Duration_Added, Transportation),
    New_Duration is Temp_Duration + Duration_Added,
    append_connection(Source, Intermediate, Duration_Added, Transportation, Temp_Routes, Routes_New),
    connected(Intermediate, Destination, Week, Day, Max_Duration, Max_Routes, Duration, Routes, New_Duration, Routes_New).