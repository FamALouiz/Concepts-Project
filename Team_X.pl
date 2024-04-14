% This line consults the knowledge bases from this file,
% instead of needing to consult the files individually.
% This line MUST be included in the final submission.
:- ['transport_kb', 'slots_kb'].

/*
gets a list of all slots of a specific group in a certain week and day
*/
day_slots(Group, Week, Day, Slots):-
	findall(X, scheduled_slot(Week, Day, X, _, Group), Slots).

	
	
	
