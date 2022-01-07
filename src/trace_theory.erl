- module(trace_theory).

-export([perform_computation/3]).

-ignore_xref([perform_computation/3]).

-export_type([word/0, dependent/0, production/0, alphabet/0]).

-type production() :: char().
-type production_definition() :: {char(), list(), list()}.
-type word() :: [production()].
-type dependent() :: [{production(), production()}].
-type independent() :: [{production(), production()}].
-type alphabet() :: [string()].

%------------------------------------------------%
%                      API                       %
%------------------------------------------------%

%Main Function that runs all the code, it computes Dependent Productions set,
%Independent Productions set, Graph, and Foata Normal Form.
-spec perform_computation(Alphabet :: alphabet(),
                          Productions :: [production_definition()],
                          Word :: word()) -> 
                              ok.
perform_computation(Alphabet, Productions, Word) ->
    Dependent = determine_dependent(Productions),
    Independent = determine_independent(Productions),
    print_dependent_independent(Dependent, Independent),
    FNF1 = foata_normal_form:get_foata_normal_form(Word, Dependent, Alphabet),
    print_FNF(FNF1, "WRONG "),
    {Graph, Order} = graph_creation:create_graph(Word, Dependent),
    print_graph(Graph, Order),
    FNF2 = get_foata_normal_form_from_graph(Graph),
    print_FNF(FNF2, ""),
    ok.

%------------------------------------------------%
%               PRIVATE FUNCTIONS                %
%------------------------------------------------%

%Function that checks if every possible pair of productions is dependent
-spec determine_dependent(Productions :: [production_definition()]) -> dependent().
determine_dependent(Productions) ->
    [ {Name1, Name2} || {Name1, _, _} = X <- Productions, {Name2, _, _} = Y <- Productions, is_dependent(X, Y)].

%Function that checks if the specific pair of productions is dependent
-spec is_dependent(Prod1 :: production_definition(), Prod2 :: production_definition())  -> boolean().
is_dependent({_, Prod1Left, Prod1Right}, {_, Prod2Left, Prod2Right}) ->
    case Prod1Left of
        Prod2Left -> true;
        _ -> 
            case lists:nth(find_non_zero(Prod1Left), Prod2Right) of
                A when A /= 0 -> true;
                _ ->
                    case lists:nth(find_non_zero(Prod2Left), Prod1Right) of
                       A when A /= 0 -> true;
                       _ -> false
                    end
            end
    end.

%Function that checks if every possible pair of productions is independent
-spec determine_independent(Productions :: [production_definition()]) -> independent().
determine_independent(Productions) ->
    [ {Name1, Name2} || {Name1, _, _} = X <- Productions, {Name2, _, _} = Y <- Productions, is_independent(X, Y)].

%Function that checks if the specific pair of productions is independent
-spec is_independent(Prod1 :: production_definition(), Prod2 :: production_definition())  -> boolean().
is_independent(Prod1, Prod2) ->
    case is_dependent(Prod1, Prod2) of
        true -> false;
        false -> true
    end.

%function that seeks first non-zero value in an array
-spec find_non_zero([integer()]) -> integer().
find_non_zero(Table) ->
     find_non_zero(Table, 1).

%function that seeks first non-zero value in an array
-spec find_non_zero([integer()], integer()) -> integer().
find_non_zero([], _) ->
    -1;
find_non_zero([Head | Tail], Pos) ->
    case Head of
        A when A /= 0 -> Pos;
        _ -> find_non_zero(Tail, Pos + 1)
    end.
    
%Function computes foata normal form based on graph
-spec get_foata_normal_form_from_graph(Graph :: graph_creation:graph_without_n_parents()) ->
     foata_normal_form:foata_normal_form().
get_foata_normal_form_from_graph(Graph) ->
    foata_normal_form:create_from_graph(graph_creation:insert_parent_data(Graph)).

%-------------------------------------------------%
%               PRINTING FUNCTIONS                %
%-------------------------------------------------%

%Function printing dependent and independent sets
-spec print_dependent_independent(Dependent :: dependent(), Independent :: independent()) -> ok.
print_dependent_independent(Dependent, Independent) ->
    print_table(Dependent, "D"),
    print_table(Independent, "I").

%Function prints table of dependent and independent productions
-spec print_table(Table :: dependent() | independent(), Letter :: string()) -> ok.
print_table(Table, Letter) ->
    io:format(Letter ++ " = {"),
    lists:foreach(fun({Prod1, Prod2}) ->
        io:format("(" ++ Prod1 ++ "," ++ Prod2 ++ ")") 
    end, Table),
    io:format("}\n"),
    ok.

%Function prints graph
-spec print_graph(Graph::graph_creation:graph_without_n_parents(), Order::graph_creation:order()) -> ok.
print_graph(Graph, Order) -> % Setting vertex numbers
    {VertexNumber, _, NumberVertex} = lists:foldl(fun({Letter, _} = Element, {Map1, Counter, Map2}) ->
        {maps:put(Element, Counter, Map1), Counter + 1, maps:put(Counter, Letter, Map2)}
    end, {#{}, 1, #{}}, Order),
    % Printing all edges in a graph
    lists:foreach(fun({Key, Connections}) ->
        lists:foreach(fun(DesignetedConnection) -> 
            io:fwrite("~.10B", [maps:get(Key, VertexNumber)]),
            io:format(" -> "),
            io:fwrite("~.10B\n", [maps:get(DesignetedConnection, VertexNumber)])
        end, Connections)
    end, maps:to_list(Graph)),
    % Printing all vertices in a graph
    lists:foreach(fun({Number, Letter}) ->
        io:fwrite("~.10B", [Number]),
        io:format("[label=" ++ Letter ++ "]\n")
    end, maps:to_list(NumberVertex)),
    ok.

%Function printing FNF
-spec print_FNF(List :: foata_normal_form:foata_normal_form(), String :: string()) -> ok.
print_FNF(List, String) ->
    io:format("FNF " ++ String ++ "= "),
    lists:foreach(fun([Head|Tail]) ->
        io:format("[" ++ Head),
        case Tail of
            [] -> io:format("]");
            Tab -> lists:foreach(fun(Element) ->
                       io:format("," ++ Element)
                   end, Tab),
                   io:format("]")
        end
    end, List),
    io:format("\n"),
    ok.
