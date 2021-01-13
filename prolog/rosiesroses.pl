customer(hugh).
customer(ida).
customer(jeremy).
customer(leroy).
customer(stella).

rose(cottage_beauty).
rose(golden_sunset).
rose(mtn_bloom).
rose(pink_paradise).
rose(sweet_dreams).

event(anniversary_party).
event(charity_auction).
event(retirement_banquet).
event(senior_prom).
event(wedding).

item(balloons).
item(candles).
item(chocolates).
item(place_cards).
item(streamers).

solve :-
	% chooses the same rose for everyone, then repeatedly backtracks until all_different
	% predicate is satisfied. Then it does the same for events and items.

	rose(HughRose), rose(IdaRose), rose(JeremyRose), rose(LeroyRose), rose(StellaRose),
	all_different([HughRose, IdaRose, JeremyRose, LeroyRose, StellaRose]),

	event(HughEvent), event(IdaEvent), event(JeremyEvent), event(LeroyEvent), event(StellaEvent),
	all_different([HughEvent, IdaEvent, JeremyEvent, LeroyEvent, StellaEvent]),

	item(HughItem), item(IdaItem), item(JeremyItem), item(LeroyItem), item(StellaItem),
	all_different([HughItem, IdaItem, JeremyItem, LeroyItem, StellaItem]),

	% each list is a quadruple [customer, rose, event, item]
	% specific only customer and leave the rest to be solved

	Quads = [ [hugh, HughRose, HughEvent, HughItem],
				[ida, IdaRose, IdaEvent, IdaItem],
				[jeremy, JeremyRose, JeremyEvent, JeremyItem],
				[leroy, LeroyRose, LeroyEvent, LeroyItem],
				[stella, StellaRose, StellaEvent, StellaItem] ],

	% 1. Jeremy made a purchase for the senior prom. Stella didnt choose flowers for a wedding,
	% picked the Cottage Beauty.
	member([jeremy, _, senior_prom, _], Quads),
	\+ member([stella, _, wedding, _], Quads),
	member([stella, cottage_beauty, _, _], Quads),

	% 2. Hugh selected the Pink Paradise and didnt choose flowers for the charity auction or the wedding.
	member([hugh, pink_paradise, _, _], Quads),
	\+ member([hugh, _, charity_auction, _], Quads),
	\+ member([hugh, _, wedding, _], Quads),

	% 3. The customer who picked roses for an anniversary party also bought streamers.
	% The one shopping for a wedding chose the balloons.

	member([_, _, anniversary_party, streamers], Quads),
	member([_, _, wedding, balloons], Quads),

	% 4. The customer who bought the Sweet Dreams also bought chocolates. Jeremy didnt pick the Mtn Bloom.
	member([_, sweet_dreams, _, chocolates], Quads),
	\+ member([jeremy, mtn_bloom, _, _], Quads),

	% 5. Leroy was shopping for the retirement banquet. The customer in charge of decorating the senior prom
	% also bought the candles.
	member([leroy, _, retirement_banquet, _], Quads),
	member([_, _, senior_prom, candles], Quads),

	tell(hugh, HughRose, HughEvent, HughItem),
    tell(ida, IdaRose, IdaEvent, IdaItem),
    tell(jeremy, JeremyRose, JeremyEvent, JeremyItem),
    tell(leroy, LeroyRose, LeroyEvent, LeroyItem),
	tell(stella, StellaRose, StellaEvent, StellaItem).

	% Succeeds if all elements of the argument list are bound and different.
	% Fails if any elements are unbound or equal to some other element.
	all_different([H | T]) :- member(H, T), !, fail.
	all_different([_ | T]) :- all_different(T).
	all_different([_]).

	tell(Name, Rose, Event, Item) :-
		write(Name), write(' bought the '), write(Rose),
		write(' to go to a(n) '), write(Event),
		write(' with '), write(Item), write('.'), nl.
