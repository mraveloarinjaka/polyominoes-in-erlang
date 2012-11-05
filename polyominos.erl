-module(polyominos).
-export([adjacents/1, translate/1, generate/1]).

adjacentsToOneElement({X,Y}) -> [{X-1, Y}, {X, Y-1}, {X, Y+1}, {X+1, Y}].

adjacentsInternal([], _, AdjacentsSoFar) -> AdjacentsSoFar;

adjacentsInternal([Current|Remainings], Polyomino, AdjacentsSoFar) ->
   PotentialAdjacents = adjacentsToOneElement(Current)--Polyomino,
   AdjacentsToAdd = PotentialAdjacents--AdjacentsSoFar,
   adjacentsInternal(Remainings, Polyomino, AdjacentsSoFar++AdjacentsToAdd).

adjacents(Polyomino) -> adjacentsInternal(Polyomino, Polyomino, []).

translate([Head|Tail]) -> 
   {MinX, MinY} = lists:foldl(fun ({X, Y}, {AccX, AccY}) -> {min(X, AccX), min(Y, AccY)} end, Head, Tail),
   [{X-MinX, Y-MinY} || {X, Y} <- [Head|Tail]].

generateFromOnePolyonimo(Polyomino) -> 
   lists:map(fun (X) -> lists:sort(translate(X)) end, [Polyomino++[Adjacent] || Adjacent <- adjacents(Polyomino)]).

generateInternal(0, GeneratedSoFar) -> GeneratedSoFar;
generateInternal(N, GeneratedSoFar) -> 
   generateInternal(N-1, lists:flatmap(fun (X) -> generateFromOnePolyonimo(X) end, GeneratedSoFar)). 

generate(1) -> [[{0,0}]];
generate(N) when N>0 -> lists:usort(generateInternal(N-1, generate(1))).

