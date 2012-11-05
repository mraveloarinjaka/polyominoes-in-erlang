-module(polyominos).
-export([adjacents/1, translate/1, generate/1, retrieveCanonicalForm/1]).

rotate({X,Y}, Theta) -> 
   {round(X*math:cos(Theta) - Y*math:sin(Theta)), round(X*math:sin(Theta) + Y*math:cos(Theta))}.

rotate90(Polyomino) ->
   translate(lists:sort(lists:map(fun (X) -> rotate(X, math:pi()/2) end, Polyomino))).
rotate180(Polyomino) ->
   translate(lists:sort(lists:map(fun (X) -> rotate(X, math:pi()) end, Polyomino))).
rotate270(Polyomino) ->
   translate(lists:sort(lists:map(fun (X) -> rotate(X, 3*math:pi()/2) end, Polyomino))).

mirror(Polyomino) ->
   translate(lists:sort(lists:map(fun ({X,Y}) -> {-X, Y} end, Polyomino))).

retrieveCanonicalForm(Polyomino) ->
   RetrieveAllRotations = fun (X) -> [X, rotate90(X), rotate180(X), rotate270(X)] end,
   hd(lists:sort(RetrieveAllRotations(Polyomino)++RetrieveAllRotations(mirror(Polyomino)))).

adjacentsToOneElement({X,Y}) -> [{X-1, Y}, {X, Y-1}, {X, Y+1}, {X+1, Y}].

adjacentsInternal([], _, AdjacentsSoFar) -> AdjacentsSoFar;

adjacentsInternal([Current|Remainings], Polyomino, AdjacentsSoFar) ->
   PotentialAdjacents = adjacentsToOneElement(Current)--Polyomino,
   AdjacentsToAdd = PotentialAdjacents--AdjacentsSoFar,
   adjacentsInternal(Remainings, Polyomino, AdjacentsToAdd++AdjacentsSoFar).

adjacents(Polyomino) -> adjacentsInternal(Polyomino, Polyomino, []).

translate([Head|Tail]) -> 
   {MinX, MinY} = lists:foldl(fun ({X, Y}, {AccX, AccY}) -> {min(X, AccX), min(Y, AccY)} end, Head, Tail),
   [{X-MinX, Y-MinY} || {X, Y} <- [Head|Tail]].

generateFromOnePolyonimo(Polyomino) -> 
   PotentialPolyominos = lists:map(fun (X) -> lists:sort(translate(X)) end, [[Adjacent]++Polyomino || Adjacent <- adjacents(Polyomino)]),
   lists:usort([retrieveCanonicalForm(PotentialPolyomino) || PotentialPolyomino <- PotentialPolyominos]).

generateInternal(0, GeneratedSoFar) -> GeneratedSoFar;
generateInternal(N, GeneratedSoFar) -> 
   generateInternal(N-1, lists:usort(lists:flatmap(fun (X) -> generateFromOnePolyonimo(X) end, GeneratedSoFar))). 

generate(1) -> [[{0,0}]];
generate(N) when N>0 -> generateInternal(N-1, generate(1)).

