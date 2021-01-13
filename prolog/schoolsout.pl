teacher(appleton).
teacher(gross).
teacher(knight).
teacher(leroy).
teacher(parnell).

subject(english).
subject(gym).
subject(history).
subject(math).
subject(science).

state(california).
state(florida).
state(maine).
state(oregon).
state(virginia).

activity(antiquing).
activity(camping).
activity(sightseeing).
activity(spelunking).
activity(waterskiing).

solve :-
	% chooses the same subject for everyone, then repeatedly backtracks until all_different
	% predicate is satisfied. Then it does the same for states and activities.
	
	subject(AppletonSubject), subject(GrossSubject), subject(KnightSubject), subject(McevoySubject), subject(ParnellSubject),
	all_different([AppletonSubject, GrossSubject, KnightSubject, McevoySubject, ParnellSubject]),
	
	state(AppletonState), state(GrossState), state(KnightState), state(McevoyState), state(ParnellState),
	all_different([AppletonState, GrossState, KnightState, McevoyState, ParnellState]),
	
	activity(AppletonActivity), activity(GrossActivity), activity(KnightActivity), activity(McevoyActivity), activity(ParnellActivity),
	all_different([AppletonActivity, GrossActivity, KnightActivity, McevoyActivity, ParnellActivity]),
	
	% each list is a quadruple [teacher, subject, state, activity]
	% specific only teacher and leave the rest to be solved
	
	Quads = [ [appleton, AppletonSubject, AppletonState, AppletonActivity],
				[gross, GrossSubject, GrossState, GrossActivity],
				[knight, KnightSubject, KnightState, KnightActivity],
				[mcevoy, McevoySubject, McevoyState, McevoyActivity],
				[parnell, ParnellSubject, ParnellState, ParnellActivity] ],
				
	% 1. Ms. Gross teaches either math or science. If Ms. Gross is going antiquing, then she is going to Florida; otherwise,
	% she is going to California.
	
	(member([gross, math, _, _], Quads);
	member([gross, science, _, _], Quads)),
	(member([gross, _, florida, antiquing], Quads);
	member([gross, _, california, _], Quads),
	\+ member([gross, _, _, antiquing], Quads)),
	
	% 2. The science teacher is going waterskiing and will travel to either California or Florida.
	% Mr. McEvoy, the history teacher, is going to either Maine or Oregon.
	
	(member([_, science, california, waterskiing], Quads);
	member([_, science, florida, waterskiing], Quads)),
	(member([mcevoy, history, maine, _], Quads);
	member([mcevoy, history, oregon, _], Quads)),
	
	% 3. If the woman who is going to Virginia is the English teacher, then she is Ms. Appleton;
	% otherwise, she is Ms. Parnell. Ms. Parnell is going spelunking.
	
	\+ member([knight, _, virginia, _], Quads),
	\+ member([mcevoy, _, virginia, _], Quads),
	(member([appleton, english, virginia, _], Quads);
	member([parnell, _, virginia, _], Quads),
	\+ member([parnell, english, _, _], Quads)),
	member([parnell, _, _, spelunking], Quads),
	
	% 4. The person who is going to Maine is not the gym teacher and isn't going sightseeing.
	
	(\+ member([_, gym, maine, _], Quads),
	\+ member([_, _, maine, sightseeing], Quads)),
	
	% 5. Ms. Gross isn't the woman who is going camping. One woman is going antiquing on her vacation.
	
	\+ member([gross, _, _, camping], Quads),
	\+ member([knight, _, _, camping], Quads),
	\+ member([mcevoy, _, _, camping], Quads),
	\+ member([knight, _, _, antiquing], Quads),
	\+ member([mcevoy, _, _, antiquing], Quads),
	
	tell(appleton, AppletonSubject, AppletonState, AppletonActivity),
    tell(gross, GrossSubject, GrossState, GrossActivity),
    tell(knight, KnightSubject, KnightState, KnightActivity),
    tell(mcevoy, McevoySubject, McevoyState, McevoyActivity),
	tell(parnell, ParnellSubject, ParnellState, ParnellActivity).
	
	% Succeeds if all elements of the argument list are bound and different.
	% Fails if any elements are unbound or equal to some other element.
	all_different([H | T]) :- member(H, T), !, fail.
	all_different([_ | T]) :- all_different(T).
	all_different([_]).
	
	tell(Name, Subject, State, Activity) :-
		write(Name), write(' teaches '), write(Subject),
		write(' and is going to '), write(State),
		write(' to go '), write(Activity), write('.'), nl.