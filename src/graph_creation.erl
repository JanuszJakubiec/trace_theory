-module(graph_creation).

-export([create_graph/2, insert_parent_data/1]).

-type vertex() :: {nonempty_string(), integer()}.
-type order() :: [vertex()].
-type connections() :: [vertex()].
-type graph_without_n_parents() :: #{vertex() => connections() | list()}.
-type vertex_data_parents() :: {connections(), integer()} | connections() | list().
-type graph_with_n_parents() :: #{vertex() => vertex_data_parents()}. 

%------------------------------------------------%
%                      API                       %
%------------------------------------------------%

%Function creates graph of dependency from Word using dependencies written in Dependent.
-spec create_graph(Word :: trace_theory:word(), Dependent :: trace_theory:dependent()) ->
    {graph_without_n_parents(), order()}.
create_graph(Word, Dependent) ->
    {AdjacencyMap, Order} = create_vertices(Word),
    {minimalize_graph(add_edges(AdjacencyMap, Order, Dependent), Order), Order}.

%Function adds extra information about number of parent to every vertex.
-spec insert_parent_data(Graph :: graph_with_n_parents()) -> graph_with_n_parents().
insert_parent_data(Graph) ->
    List = maps:to_list(Graph),
    lists:foldl( fun({_, Connections}, GraphMap) ->
        lists:foldl(fun insert_to_map/2, GraphMap, Connections)
    end, convert_graph(Graph, List), List).

%-------------------------------------------------%
%       insert_parent_data helper functions       %
%-------------------------------------------------%

%Function adds 0 as inital value of n_parents to every vertex
-spec convert_graph(graph_without_n_parents(), [{vertex(), connections()}]) -> graph_with_n_parents().
convert_graph(Graph, List) ->
    lists:foldl(fun({Vertex, Connections}, GraphMap) ->
        maps:put(Vertex, {Connections, 0}, GraphMap)
    end, Graph, List).

%Function adds 1 to all neighbors' parent's counter.
-spec insert_to_map(Key :: vertex(), Graph :: graph_with_n_parents()) -> graph_with_n_parents().
insert_to_map(Key, Graph) ->
    {Connections, Parents} = maps:get(Key, Graph),
    maps:put(Key, {Connections, Parents+1}, Graph).

%-------------------------------------------------%
%          create_graph helper functions          %
%-------------------------------------------------%

%Function adds vertices to graph based on Word.
-spec create_vertices(Word :: trace_theory:word()) -> {graph_without_n_parents(), order()}.
create_vertices(Word) ->
    create_vertices_iterate(Word, {#{}, []}, #{}).

%Function iterates trough the Word adding for every production new vertex.
-spec create_vertices_iterate(trace_theory:word(), {graph_without_n_parents(), order()}, #{char() => integer()}) ->
    {graph_without_n_parents(), order()}.
create_vertices_iterate([], Vertices, _) ->
    Vertices;
create_vertices_iterate([Head | Tail], {Vertices, Order}, LettersCounter) ->
    case maps:is_key(Head, LettersCounter) of
        true -> 
            A = maps:get(Head, LettersCounter),
            create_vertices_iterate(Tail, {maps:put({Head, A}, [], Vertices), Order ++ [{Head, A}]}, maps:put(Head, A+1, LettersCounter));
        false ->
            create_vertices_iterate(Tail, {maps:put({Head, 0}, [], Vertices), Order ++ [{Head, 0}]}, maps:put(Head, 1, LettersCounter))
    end.

%Function adds edges to a graph wich represent relation of dependency between productions
-spec add_edges(graph_without_n_parents(), order(), trace_theory:dependent()) -> graph_without_n_parents().
add_edges(Graph, [], _) ->
    Graph;
add_edges(Graph, [Head | Tail], Dependent) ->
    add_edges(add_connections(Graph, Head, Tail, Dependent), Tail, Dependent).

%For every vertex, function add its connections to Graph
-spec add_connections(graph_without_n_parents(), vertex(), order(), trace_theory:dependent()) ->
    graph_without_n_parents().
add_connections(Graph, _, [], _) ->
     Graph;
add_connections(Graph, {ElementIdentifier, _} = Element, [{HeadIdentifier, _} = Head | Tail], Dependent) ->
    case lists:filter(fun(X) ->
            case X of
                {ElementIdentifier, HeadIdentifier} -> true;
                _ -> false
            end
        end, Dependent) of 
            [{ElementIdentifier, HeadIdentifier}] -> 
                Tab = maps:get(Element, Graph),
                add_connections(maps:put(Element, Tab ++ [Head], Graph), Element, Tail, Dependent);
            [] -> 
                add_connections(Graph, Element, Tail, Dependent)
    end.

%Function minimizes graph, deleting any unnecessary vertex.
-spec minimalize_graph(graph_without_n_parents(), order()) -> graph_without_n_parents().
minimalize_graph(GraphMap, []) ->
    GraphMap;
minimalize_graph(GraphMap, [Head | Tail]) ->
    minimalize_graph(delete_extra_edges(GraphMap, Head, maps:get(Head, GraphMap)), Tail).

%For every vertex in a graph, function sets out new connections table, only with necessary connections.
-spec delete_extra_edges(graph_without_n_parents(), vertex(), connections()) -> graph_without_n_parents().
delete_extra_edges(GraphMap, _, []) ->
    GraphMap;
delete_extra_edges(GraphMap, Head, Connections) ->
    maps:put(Head, lists:filter(fun(Element) ->
        check_path(GraphMap, Connections, Head, Element, Head)
    end, Connections), GraphMap).

%Function checks if there is no alternative path to the neighbor.
%If there is not it returns true, if there is it returns false.
-spec check_path(graph_without_n_parents(), connections(), vertex(), vertex(), vertex()) -> boolean().
check_path(_, _, _, Element, Element) ->
    false;
check_path(_, [], _, _, _) ->
    true;
check_path(GraphMap, Connections, Head, Element, Head) ->
    check_results(lists:map(fun(X) -> 
        case X of
            Element -> true;
            _ -> check_path(GraphMap, maps:get(X, GraphMap), Head, Element, X)
        end
    end, Connections));
check_path(GraphMap, Connections, Head, Element, _) ->
    check_results(lists:map(fun(X) -> check_path(GraphMap, maps:get(X, GraphMap), Head, Element, X)
    end, Connections)).

%Function used to determine if there is false in a table, in that case, the function returns false.
-spec check_results([boolean()]) -> boolean().
check_results([]) ->
    true;
check_results([false | _]) ->
    false;
check_results([_|Tail]) ->
    check_results(Tail).
