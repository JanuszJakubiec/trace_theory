-module(foata_normal_form).

-export([create_from_graph/1, get_foata_normal_form/3]).
-export_type([foata_normal_form/0]).

-type foata_class() :: [trace_theory:production()].
-type foata_normal_form() :: [foata_class()].
-type stack_element() :: dependent | char().
-type stack() :: #{char() => stack_element()}.

%------------------------------------------------%
%                      API                       %
%------------------------------------------------%

%Function that is called to compute foata normal form using Dickert's graph
-spec create_from_graph(Graph :: graph_creation:graph_with_n_parents()) -> foata_normal_form().
create_from_graph(Graph) ->
    create_from_graph(Graph, []).

%Function computes foata normal form based on Word, Dependent and Alphabet
-spec get_foata_normal_form(Word :: trace_theory:word(),
                        Dependent :: trace_theory:dependent(),
                        Alphabet :: trace_theory:alphabet()) ->
                            foata_normal_form().
get_foata_normal_form(Word, Dependent, Alphabet) ->
    Stack = fill_stack(create_stack(Alphabet), Word, Dependent),
    get_foata_normal_form_from_stack(Stack).

%-------------------------------------------------%
%       create_from_graph helper functions        %
%-------------------------------------------------%

%Recursive function that is performed while there is something in Graph
%First, it specifies which vertex should be deleted and inserted to FNF.
%Than it reduces n_parent in every neighbor of soon-be deleted vertex'es.
%Finally it deletes vertexes from graph and adds them to FNF table.
-spec create_from_graph(Graph :: graph_creation:graph_with_n_parents(),
                        FNF :: foata_normal_form()) ->
                            foata_normal_form().
create_from_graph(Graph, FNF) when map_size(Graph) == 0 ->
    FNF;
create_from_graph(Graph, FNF) ->
    GraphList = maps:to_list(Graph),
    VertexToBeDeleted = lists:filter(fun(Element) ->
        case Element of
            {_,{_, 0}} -> true;
            _ -> false
        end
    end, GraphList),
    % VertexToBeDeleted -> list of vertexes that soon will be deleted.
    NewGraph = lists:foldl(fun({Key, _}, GraphMap) ->
        {Connections, _} = maps:get(Key, GraphMap),
        NewGraph = lists:foldl(fun reduce_parents/2, GraphMap, Connections),
        % Connections -> list of neighbors of vertex that is processed in this iteration.
        maps:remove(Key, NewGraph)
        % Vertex from VertexToBeDeleted after being used to decrease n_parent in its neighbors is deleted.
    end, Graph, VertexToBeDeleted),
    create_from_graph(NewGraph, FNF ++ [transform_deleted_vertex_table(VertexToBeDeleted)]).

%Function that is reducing n_parents
-spec reduce_parents(Connections :: graph_creation:connections(),
                     Graph :: graph_creation:graph_with_n_parents()) -> 
                         graph_creation:graph_with_n_parents().
reduce_parents(Connection, Graph) ->
    {NConnections, Parents} = maps:get(Connection, Graph),
    maps:put(Connection, {NConnections, Parents - 1}, Graph).

%Function that returns list of productions. It takes them from table VertexToBeDeleted.
-spec transform_deleted_vertex_table(List :: graph_creation:connections()) -> foata_class().
transform_deleted_vertex_table(List) ->
    lists:map(fun( {{Production, _}, _} ) ->
         Production 
    end, List).

%--------------------------------------------------%
%    create_foata_normal_form helper functions     %
%--------------------------------------------------%

%Function creates stack, its a map. The keys are letters from alphabet and values are empty lists.
-spec create_stack(Alphabet :: trace_theory:alphabet()) -> #{char() => list()}.
create_stack(Alphabet) ->
    lists:foldl(fun(Letter, Map) ->
        maps:put(Letter, [], Map)
    end, #{}, Alphabet).

%Function reads the word backwards and writes to a stack.
-spec fill_stack(Stack :: stack(),
                 Word :: trace_theory:word(),
                 Dependent :: trace_theory:dependent()) ->
                      stack().
fill_stack(Stack, Word, Dependent) ->
    lists:foldr(fun(Letter, NewStack) ->
        add_to_stack(NewStack, Letter, Dependent)
    end, Stack, Word).

%Function adds to a stack data accordingly to a read letter.
-spec add_to_stack(Stack :: stack(), Letter :: char(), Dependent :: trace_theory:dependent()) -> stack().
add_to_stack(Stack, Letter, Dependent) ->
    LetterList = maps:get(Letter, Stack),
    StackWithLetter = maps:put(Letter, LetterList ++ [Letter], Stack),
    lists:foldl(fun(DependentLetter, NewStack) ->
        List = maps:get(DependentLetter, NewStack),
        maps:put(DependentLetter, List ++ [dependent], NewStack)
    end, StackWithLetter, determine_letters(Letter, Dependent)).

%Function returns a list of letters that are dependant with a Letter.
-spec determine_letters(Letter :: char(), Dependent :: trace_theory:dependent()) -> [char()].
determine_letters(Letter, Dependent) ->
    lists:foldl(fun(Element, List) ->
        case Element of
            {Letter, Letter} -> List;
            {Letter, NewLetter} -> List ++ [NewLetter];
            _ -> List
        end
    end, [], Dependent).

%Function runs recursively while any stack is not empty. It reads the data level-by-level
-spec get_foata_normal_form_from_stack({Stack :: stack(), FNF :: foata_normal_form()} | stack()) ->
                                          foata_normal_form() | {stack() | foata_normal_form()}.
get_foata_normal_form_from_stack({Stack, FNF}) when map_size(Stack) == 0 ->
    remove_empty_classes(FNF);
get_foata_normal_form_from_stack({Stack, FNF}) ->
    get_foata_normal_form_from_stack(lists:foldl(fun(Letter, {NewStack, NewFNF}) ->
        List = maps:get(Letter, NewStack),
        case List of
            [] -> {maps:remove(Letter, NewStack), NewFNF};
            [dependent | Tail] -> {maps:put(Letter, Tail, NewStack), NewFNF};
            [Letter | Tail] -> [CurrentClass | ClassesBefore] = NewFNF,
                             {maps:put(Letter, Tail, NewStack), [[Letter | CurrentClass] | ClassesBefore]}
        end
    end, {Stack, [[] | FNF]}, maps:keys(Stack)));
get_foata_normal_form_from_stack(Stack) ->
    get_foata_normal_form_from_stack({Stack, []}).

%Function removes empty classes witch will appear when whole level is filled with atom "dependent"
-spec remove_empty_classes(FNF :: foata_normal_form()) -> foata_normal_form().
remove_empty_classes(FNF) ->
    lists:filter(fun(Element) ->
        case Element of
            [] -> false;
            _ -> true
        end
    end, FNF).
