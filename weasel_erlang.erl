-module(weasel_erlang).
%% export all, so that we can play with the functions in the REPL.
%% otherwise, we would just -export([evolution/0]).
-compile(export_all).
-author("Bjorn Roberg").

-define(TARGET_WEASEL, "methinks it is a weasel").
-define(GENE_POOL, "abcdefghijklmnopqrstuvwxyz ").
-define(MUT_CHANCE, 0.04).
-define(LITTER_SIZE, 100).

main() ->
    evolution(),
    erlang:halt(0).


%% if called with no arguments, just fetch a random gene.
random_gene()  ->
    [lists:nth(rand:uniform(length(?GENE_POOL)), ?GENE_POOL)].
%% if called with an existing gene, use the mutation change to see
%% whether to fetch a new gene, or keep the gene.
random_gene(G) ->
    case rand:uniform() =< ?MUT_CHANCE of
        true  -> random_gene();
        false -> G
    end.

%% if called without arguments, then all genes will be completely random.
random_genes() ->
    L1 = lists:seq(1, length(?TARGET_WEASEL)),
    %% L2 is a List of "Lists" (i.e. strings)
    L2 = lists:map(fun (_N) -> random_gene() end, L1),
    %% so we need to flatten it to simply a "List"
    lists:flatten(L2).
%% if called with an argument, suppose it is a "Weasel", and
%% for each of the genes in that weasel, maybe mutate it.
random_genes(Parent) ->
    L1 = lists:map(fun (G) -> random_gene(G) end, Parent),
    %% same here.
    lists:flatten(L1).

%% create a litter of weasels that are children of Weasel
procreate(Weasel) ->
    Seq = lists:seq(1, ?LITTER_SIZE),
    lists:map(fun (_N) -> random_genes(Weasel) end, Seq).

%% compute the rating (compared to TARGET_WEASEL) of the Weasel
rating_of(Weasel) ->
    Key = weasel_rating,
    %% this is not a pure function, but uses only variables that
    %% it closes over.
    ComputeFn = fun () ->
                    %% zip the current Weasel with the target weasel
                    %% which results in a list of tuples where each tuple
                    %% is a pair that, when both elements are the same,
                    %% results in one point, else no point.
                    Zipped = lists:zip(Weasel, ?TARGET_WEASEL),
                    Fn = fun (GeneTuple, Acc) ->
                                case GeneTuple of
                                    {G,G} -> Acc + 1;
                                    {_,_} -> Acc
                                end
                        end,
                    _Rating = lists:foldl(Fn, 0, Zipped)
                end,
    %% memoize, because this is an idempotent function.
    %% see http://stackoverflow.com/a/3315713
    %% and
    %% see http://www.erlang.org/course/advanced.html#dict
    %% for more info.
    %% this breaks referential transparency, but it's the
    %% cleanest way of storing the results.
    case erlang:get({Key, Weasel}) of
        %% cache hit
        Rating when is_integer(Rating) -> Rating;

        %% miss
        undefined ->
            Rating = ComputeFn(),
            erlang:put({Key, Weasel}, Rating),
            Rating
    end.


fittest_of(Litter) ->
    %% sort the Litter by individual rating
    L = lists:sort(fun (W1, W2) ->
                           rating_of(W1) >= rating_of(W2)
                   end, Litter),
    %% the head weasel is the Fittest
    [Fittest|_] = L,
    Fittest.


evolution() ->
    io:format("~s  :: ~s :: ~s ~n",
             ["G#", "R#", "Genes"]),
    io:format("~s :: ~s :: ~s ~n",
             ["===", "==", "====="]),

    %% initialise a fresh Weasel
    FirstWeasel = random_genes(),
    %% create a process to initiate the evolution
    spawn_generation(FirstWeasel),
    %% initate the actor for the above process.
    step_generation(0).


spawn_generation(Parent) ->
    Pid = self(),
    %% send the results of the generation to the actor
    spawn(fun() -> Pid ! generation(Parent) end).


generation(Parent) ->
    Litter = procreate(Parent),
    NewParent = fittest_of(Litter),
    {NewParent, rating_of(NewParent)}.


step_generation(Generation) ->
    Goal = rating_of(?TARGET_WEASEL),
    %% wait for the Parent of this Generation to create a Litter
    %% and compute the Fittest.
    receive
        {FittestWeasel, Rating} when Rating =:= Goal ->
            %% Rating is at Goal, so we're done. Don't spawn more processes.
            io:format("~3..0B :: ~2..0B :: ~s ~n",
                    [Generation, Rating, FittestWeasel]);
        {FittestWeasel, Rating} ->
            %% The Weasel did not quite cut it, so let it go on.
            io:format("~3..0B :: ~2..0B :: ~s ~n",
                      [Generation, Rating, FittestWeasel]),
            spawn_generation(FittestWeasel),
            step_generation(Generation + 1)
        after
        3000 ->
            exit('Timeout')
    end.
