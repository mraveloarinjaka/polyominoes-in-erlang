-module(polyominos).
-export([generate/1]).

-import(plists).

-define(CHUNK, 1000).
-define(PCONTEXT, {processes, 4}).

translate(Polyomino) -> 
   {MinX, MinY} = {lists:min([X || {X,_} <- Polyomino]), lists:min([Y || {_,Y} <- Polyomino])},
   [{X-MinX, Y-MinY} || {X, Y} <- Polyomino].

%rotate({X,Y}, Theta) -> 
%   {round(X*math:cos(Theta) - Y*math:sin(Theta)), round(X*math:sin(Theta) + Y*math:cos(Theta))}.

rotate90(Polyomino) ->
   translate(lists:sort(lists:map(fun ({X,Y}) -> {-Y,X} end, Polyomino))).
rotate180(Polyomino) ->
   translate(lists:sort(lists:map(fun ({X,Y}) -> {-X,-Y} end, Polyomino))).
rotate270(Polyomino) ->
   translate(lists:sort(lists:map(fun ({X,Y}) -> {Y,-X} end, Polyomino))).

mirror(Polyomino) ->
   translate(lists:sort(lists:map(fun ({X,Y}) -> {-X, Y} end, Polyomino))).

retrieveCanonicalForm(Polyomino) ->
   RetrieveAllRotations = fun (X) -> 
      Mirror = mirror(X),
      [X, rotate90(X), rotate180(X), rotate270(X), Mirror, rotate90(Mirror), rotate180(Mirror), rotate270(Mirror)] end,
   hd(lists:usort(RetrieveAllRotations(Polyomino))).

adjacentsToOneElement({X,Y}) -> [{X-1, Y}, {X, Y-1}, {X, Y+1}, {X+1, Y}].

adjacents(Polyomino) -> 
   lists:usort(lists:append([adjacentsToOneElement(X) || X <- Polyomino]))--Polyomino.

generateFromOnePolyonimo(Polyomino) -> 
   GrownPolyominos = [[Adjacent]++Polyomino || Adjacent <- adjacents(Polyomino)],
   lists:usort(lists:map(fun (X) -> retrieveCanonicalForm(translate(lists:sort(X))) end, GrownPolyominos)).

%processByChunk([], Result) -> Result;
%processByChunk(ChunkToProcess, Result) ->
%   Chunk = lists:sublist(ChunkToProcess, ?CHUNK),
%   Remaining = if ?CHUNK < length(ChunkToProcess) -> lists:nthtail(?CHUNK+1, ChunkToProcess);
%      true -> []
%   end,
%   GrownPolyominos = plists:map(fun (X) -> generateFromOnePolyonimo(X) end, Chunk, ?PCONTEXT),
%   processByChunk(Remaining, plists:usort(GrownPolyominos++Result)).

generate(0, GeneratedSoFar) -> GeneratedSoFar;
generate(N, GeneratedSoFar) when N>0 -> 
   GrownPolyominos = lists:append(plists:map(fun (X) -> generateFromOnePolyonimo(X) end, GeneratedSoFar, [?CHUNK, ?PCONTEXT])),
   Result = plists:usort(fun (A, B) -> A =< B end, GrownPolyominos, [?CHUNK, ?PCONTEXT]),
   generate(N-1, Result). 

generate(1) -> [[{0,0}]];
generate(N) when N>1 -> generate(N-1, generate(1)).

