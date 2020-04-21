:- op(3, yfx, p_op).
:- op(2, yfx, b_op).
:- op(1, xf,  h_op).

%! operator_char(?Operator, ?Char)
% True if Char is the character representing Operator
operator_char(pp, '+').
operator_char(bb, '.').
operator_char(hh, '*').

%! operator_char(?Operator, ?Priority)
% True if Operator has priority Priority
operator_priority(none, 4).
operator_priority(pp, 3).
operator_priority(bb, 2).
operator_priority(hh, 1).

%! split_head(?List, ?Head, ?Tail)
% True if Head is the head of List and Tail is the tail of List
split_head([Head | Tail], Head, Tail).

%! split_head(?Tail, ?Head, ?List)
% Same as split_head/3, but with different order of arguments.
% For use with meta-predicates, e.g. prepeding the same head to each list in a list of lists.
% True if Head is the head of List and Tail is the tail of List.
prepend_head(H, T, L) :- split_head(L, H, T).

%! for_any(+Predicate, +List)
% True if there exists an element E of List for which Predicate(E) is true.
for_any(Predicate, [H | _]) :-
	call(Predicate, H), !.
for_any(Predicate, [_ | T]) :-
	for_any(Predicate, T).

%! member_reversed(?List, ?Member)
% True if Member is a member of List
member_reversed(List, Member) :-
	member(Member, List).

%! maplist_with_delims(+DelimFunction, +Transform, +List)
% True if DelimFunction is true and for each member M of List Transform(M) is true.
% Calls DelimFunction after each successful call of Transform except after the call to the last element of List.
% True if List is empty.
maplist_with_delims(_, _, []).
maplist_with_delims(_, Transform, [H]) :-
	call(Transform, H), !.
maplist_with_delims(DelimFunction, Transform, [H | T]) :-
	call(Transform, H),
	call(DelimFunction),
	maplist_with_delims(DelimFunction, Transform, T).

%! maplist_or_call_with_delims(+DelimFunction, +Transform, +Input)
% If Input is a list, same as maplist_with_delims/3.
% Else true if Transform(Input) and DelimFunction are true (in that order of execution).
maplist_or_call_with_delims(DelimFunction, Transform, List) :-
	is_list(List),
	!,
	maplist_with_delims(DelimFunction, Transform, List).
maplist_or_call_with_delims(DelimFunction, Transform, Term) :-
	call(Transform, Term),
	call(DelimFunction).

%! maplist_or_transform(+Transform, +X, -Transformed)
% If X is a list, same as maplist/3.
% Else true if Transform(X, Transformed) is true.
maplist_or_transform(Transform, List, TransformedList) :-
	is_list(List),
	!,
	maplist(Transform, List, TransformedList).
maplist_or_transform(Transform, Term, TransformedTerm) :-
	call(Transform, Term, TransformedTerm).

%! mapfoldl(+Function, +Input, +ListInput, -Output, -ListOutput)
% Performs left fold on ListInput with Input as initial value and Output as final value, but Function must be quaternary,
% with the fourth argument of each call stored in ListOutput.
mapfoldl(_, Output, [], Output, []) :- !.
mapfoldl(Function, Input, [ListInputH | ListInputT], Output, [ListOutputH | ListOutputT]) :-
	call(Function, Input, ListInputH, IMOutput, ListOutputH),
	mapfoldl(Function, IMOutput, ListInputT, Output, ListOutputT).

ignorer(Value, _, Value).

%! constant_list(+Controls, +Value, -ConstantList)
% True if ConstantList is a list with each member equal to Value and with the same length as list Controls.
constant_list(Controls, Value, ConstantList) :-
	maplist(ignorer(Value), Controls, ConstantList).

%! constant_list(+RowControls, +ColumnControls, +Value, -Matrix)
% True if Matrix is a list of lists of the same length as RowControls
% with each member being a constant list with members equal to Value and length same as ColumnControls.
constant_matrix(RowControls, ColumnControls, Value, Matrix) :-
	constant_list(ColumnControls, Value, ConstantRow),
	constant_list(RowControls, ConstantRow, Matrix).

%! prepend_or_append(?X, ?List, ?Result)
% If X is list, same as append/3.
% Else same as prepend_head/3.
prepend_or_append(List, OtherList, Appended) :-
	is_list(List),
	!,
	append(List, OtherList, Appended).
prepend_or_append(NonList, OtherList, [NonList | OtherList]).

%! flatten_matrix(+List, -Flat)
% True if List is a list and Flat is list of members of List "unpacked".
% If a member is not a list, it is left unchanged.
% Else the members of the member list are pasted into Flat.
% Similar to append/2 but List may contain non-lists.
flatten_matrix([], []) :- !.
flatten_matrix([H | T], Flat) :-
	flatten_matrix(T, FlatT),
	prepend_or_append(H, FlatT, Flat).

%! flatten_matrix(+Transform, +List, -Flat)
% True if Flat is flattened (with flatten_matrix/2) output of maplist/3 with Transform and List.
maplist_flatten(Transform, ListIn, FlatListOut) :-
	maplist(Transform, ListIn, ListOut),
	flatten_matrix(ListOut, FlatListOut).

%! union_lists(+Lists, -Union)
% True if Union is a list of unique values from flattened (with flatten_matrix/2) Lists.
union_lists(Lists, Union) :-
	flatten_matrix(Lists, List),
	sort(List, Union).

%! delete_all(?X, +ListInput, -ListOutput)
% True if ListOutput is a list containing all members of ListInput which are not unifiable with X.
% Preserves order.
delete_all(X, ListInput, ListOutput) :-
	exclude(=(X), ListInput, ListOutput).

%! split_by_control(+Controls, +List, +Control, -Pre, -Element, -Post)
% True if append(Pre, [Element | Post], List) is true and Element has the same index in List as Control has in Controls.
% Element must me a member of List and Control must be a member of Controls.
split_by_control([Control | _], [Element | ElementsT], Control, [], Element, ElementsT) :- !.
split_by_control([_ | ControlT], [ElementsH | ElementsT], Control, [ElementsH | Pre], Element, Post) :-
	split_by_control(ControlT, ElementsT, Control, Pre, Element, Post).

%! get_element(+Controls, +List, +Control, -Element)
% True if Element has the same index in List as Control has in Controls.
% Element must me a member of List and Control must be a member of Controls.
get_element(Controls, List, Control, Element) :-
	split_by_control(Controls, List, Control, _, Element, _).

%! get_element(+Matrix, +RowControls, +ColumnControls, +RowControl, +ColumnControl, -Element)
% True if Element is in a row with the same index as RowControl has in RowControls
% and in a column with the same index as ColumnControl has in ColumnControls.
% Element, RowControl and ColumnControl must be members of List, RowControls and ColumnControls, respectively.
get_element(Matrix, RowControls, ColumnControls, RowControl, ColumnControl, Element) :-
	get_element(RowControls, Matrix, RowControl, Row),
	get_element(ColumnControls, Row, ColumnControl, Element).

%! get_element(+Default, +Controls, +List, +Control, -Element)
% If Element is a member of List, same as get_element(Controls, List, Control, Element).
% Else Element is unified with Default.
get_element_default(_, Controls, List, Control, Element) :-
	get_element(Controls, List, Control, Element), !.
get_element_default(Default, _, _, _, Default).

%! transform_element(+Transform, +Controls, +List, +Control, -ListResult)
% True if ListResult has the same members as List,
% except the member at the same index as Control is in Controls is transformed by Transform.
% Control must be a member of Control.
transform_element(Transform, Controls, List, Control, ListResult) :-
	split_by_control(Controls, List, Control, Pre, Element, Post),
	call(Transform, Element, NewElement),
	append(Pre, [NewElement | Post], ListResult).

%! set_element(+Controls, +List, +Control, +NewElement, -ListResult)
% True if ListResult has the same members as List,
% except the member at the same index as Control is in Controls is swapped by NewElement.
% Control must be a member of Control.
set_element(Controls, List, Control, NewElement, ListResult) :-
	transform_element(ignorer(NewElement), Controls, List, Control, ListResult).

%! transform_element(+Transform, +Matrix, +RowControls, +ColumnControls, +RowControl, +ColumnControl, -NewMatrix)
% Similar to transform_element/5 but transform an element of Matrix specified by RowControl and ColumnControl.
% See get_element/6 for explanation of RowControl and ColumnControl.
transform_element(Transform, Matrix, RowControls, ColumnControls, RowControl, ColumnControl, NewMatrix) :-
	split_by_control(RowControls, Matrix, RowControl, Pre, Row, Post),
	transform_element(Transform, ColumnControls, Row, ColumnControl, NewRow),
	append(Pre, [NewRow | Post], NewMatrix).

%! set_element(+Matrix, +RowControls, +ColumnControls, +RowControl, +ColumnControl, +NewElement, -NewMatrix)
% Similar to transform_element/7 but swaps the element for NewElement.
set_element(Matrix, RowControls, ColumnControls, RowControl, ColumnControl, NewElement, NewMatrix) :-
	transform_element(ignorer(NewElement), Matrix, RowControls, ColumnControls, RowControl, ColumnControl, NewMatrix).

%! delete_first(+X, +List, -ModifiedList)
% True if ModifiedList is same as List but with the first member unifiable with X removed.
% ModifiedList is same as List if there is no such member.
delete_first(X, List, ModifiedList) :-
	split_by_control(List, List, X, Pre, _, Post),
	append(Pre, Post, ModifiedList), !.
delete_first(_, List, List).

wrapper(hh, X, hh(X)).
wrapper(bb, X, bb(X)).
wrapper(pp, X, pp(X)).

wrapper_for_make_helper(XX, Y, XXY) :- wrapper(XX, Y, XXY), !.
wrapper_for_make_helper(_, X, X).

%! join_regex(+Operation, +List, -Wrapped)
% Joins regexes from List into a regex with operation Operation.
% Unpacks the regexes with the same operation as Operation.
% E.g. make_helper(pp, [pp([a,b,c]), d], pp([a,b,c,d])).
join_regex(bb, [], ~) :- !.
join_regex(pp, [], @) :- !.
join_regex(_ , [X], X) :- !.
join_regex(XX, List, Wrapped) :-
	maplist(wrapper_for_make_helper(XX), UnwrappedList, List),	
	flatten_matrix(UnwrappedList, Flat),
	wrapper(XX, Flat, Wrapped).

%! simplify_rule(+Regex, -SimplifiedRegex)
% Specifies rules for simplification.
% For complete simplification of a regex, use simplify/2
simplify_rule(hh(~), ~).
simplify_rule(hh(@), @).
simplify_rule(hh(hh(A)), hh(A)).
simplify_rule(hh(pp(List)), hh(PP)) :-
	delete_all(~, List, ListWithoutL),
	join_regex(pp, ListWithoutL, PP).
simplify_rule(pp([X]), X) :-
	!.
simplify_rule(pp(List), PP) :-
	delete_all(@, List, ListWithoutO),
	sort(ListWithoutO, SetWithoutO),
	join_regex(pp, SetWithoutO, PP).
simplify_rule(bb([pp([~, X]), hh(X)]), hh(X)) :-
	!.
simplify_rule(bb([X]), X) :-
	!.
simplify_rule(bb(List), @) :-
	member(@, List),
	!.
simplify_rule(bb(List), BB) :-
	delete_all(~, List, ListWithoutL),
	join_regex(bb, ListWithoutL, BB).

%! simplify(+Regex, -SimplifiedRegex)
% Simplifies a regex.
simplify(Wrapped, S) :-
	wrapper(XX, Unwrapped, Wrapped),
	maplist_or_transform(simplify, Unwrapped, UnwrappedMapped),
	wrapper(XX, UnwrappedMapped, WrappedMapped),
	simplify_rule(WrappedMapped, S), !.
simplify(A, A).

equal_helper(X, X, X).

%! equal(+List, +Value)
% True if all members of List are equal to Value.
equal([Head | T], Value) :-
	foldl(equal_helper, T, Head, Value).

get_states(dfa(States, _, _, _, _), States).
get_states(lnfa(States, _, _, _, _, _), States).
get_states(llnfa(States, _, _, _, _, _), States).

get_alphabet(lnfa(_, Alphabet, _, _, _, _), Alphabet).
get_alphabet(llnfa(_, Alphabet, _, _, _, _), Alphabet).

get_transition_function(lnfa(_, _, TransitionFunction, _, _, _), TransitionFunction).
get_transition_function(llnfa(_, _, TransitionFunction, _, _, _), TransitionFunction).

get_lambda_transitions(lnfa(_, _, _, LambdaTransitions, _, _), LambdaTransitions).

get_lambda_closures(llnfa(_, _, _, LambdaClosures, _, _), LambdaClosures).
get_lambda_closure(LLNFA, State, LambdaClosure) :-
	get_states(LLNFA, States),
	get_lambda_closures(LLNFA, LambdaClosures),
	get_element(States, LambdaClosures, State, LambdaClosure).

get_start_state(dfa(_, _, _, StartState, _), StartState).
get_start_states(lnfa(_, _, _, _, StartStates, _), StartStates).
get_start_states(llnfa(_, _, _, _, StartStates, _), StartStates).

get_accept_states(dfa(_, _, _, _, AcceptStates), AcceptStates).
get_accept_states(lnfa(_, _, _, _, _, AcceptStates), AcceptStates).
get_accept_states(llnfa(_, _, _, _, _, AcceptStates), AcceptStates).

%! do_a_step(+DFA, ?Symbol, ?FromState, ?ToState)
% True if there is an edge from FromState to ToState with symbol Symbol on DFA.
do_a_step(_, ~, FromState, FromState).
do_a_step(dfa(States, Alphabet, TransitionFunction, _, _), Symbol, FromState, ToState) :-
	get_element(TransitionFunction, States, Alphabet, FromState, Symbol, ToState).

%! process(+DFA, +Word, -ToState)
% True if Word gets DFA from it's start state to ToState.
process(DFA, Word, ToState) :-
	get_start_state(DFA, StartState),
	foldl(do_a_step(DFA), Word, StartState, ToState).

%! process(+DFA, +Word)
% True if DFA accepts Word.
process(DFA, Word) :-
	process(DFA, Word, ToState),
	get_accept_states(DFA, AcceptStates),
	member(ToState, AcceptStates).

%! make_regex(+DFA, +AllowedStates, +FromState, +ToState, -Regex)
% True if Regex is a regex of all words that get DFA from FromState to ToState by only using states from AllowedStates.
make_regex(DFA, [], FromState, ToState, Regex) :-
	!,
	findall(S, do_a_step(DFA, S, FromState, ToState), L),
	join_regex(pp, L, PP),
	simplify(PP, Regex).
make_regex(DFA, [FromState | StateTail], FromState, FromState, Regex) :-
	!,
	make_regex(DFA, StateTail, FromState, FromState, A),
	join_regex(bb, [A, hh(A)], BB),
	simplify(BB, Regex).
make_regex(DFA, [FromState | StateTail], FromState, ToState, Regex) :-
	!,
	make_regex(DFA, StateTail, FromState, ToState, A),
	make_regex(DFA, StateTail, FromState, FromState, B),
	join_regex(bb, [hh(B), A], BB),
	simplify(BB, Regex).
make_regex(DFA, [ToState | StateTail], FromState, ToState, Regex) :-
	!,
	make_regex(DFA, StateTail, FromState, ToState, A),
	make_regex(DFA, StateTail, ToState, ToState, B),
	join_regex(bb, [A, hh(B)], BB),
	simplify(BB, Regex).
make_regex(DFA, [StateHead | StateTail], FromState, ToState, Regex) :-
	make_regex(DFA, StateTail, FromState, ToState, A),
	make_regex(DFA, StateTail, FromState, StateHead, B),
	make_regex(DFA, StateTail, StateHead, StateHead, C),
	make_regex(DFA, StateTail, StateHead, ToState, D),
	join_regex(bb, [B, hh(C), D], BB),
	join_regex(pp, [A, BB], PP),
	simplify(PP, Regex).

%! make_regex(+DFA, +ToState, -Regex)
% True if Regex is a regex of all words that get DFA from it's start state to ToState.
make_regex(DFA, ToState, Regex) :-
	get_states(DFA, States),
	get_start_state(DFA, StartState),
	delete_first(StartState, States, StatesWithoutStart),
	make_regex(DFA, [StartState | StatesWithoutStart], StartState, ToState, Regex).

%! make_regex(+DFA, -Regex)
% True if L(Regex) = L(DFA).
make_regex(DFA, Regex) :-
	get_accept_states(DFA, AcceptStates),
	maplist(make_regex(DFA), AcceptStates, Regexes),
	join_regex(pp, Regexes, PP),
	simplify(PP, Regex).

%! set_edge(+Edge, +InputDFA, -OutputDFA)
% True if OutputDFA is same as InputDFA, except with one edge swapped with Edge.
set_edge(
		e(FromState, Symbol, ToState),
		dfa(States, Alphabet, TransitionFunction, StartState, AcceptStates),
		dfa(States, Alphabet, NewTransitionFunction, StartState, AcceptStates)) :-
	set_element(TransitionFunction, States, Alphabet, FromState, Symbol, ToState, NewTransitionFunction).

%! add_if_not_member(+Element, +InputList, -OutputList)
% If Element is a member of InputList, OutputList is same as InputList.
% Else prepend Element to InputList.
add_if_not_member(Element, List, [Element | List]) :-
	\+ member(Element, List), !.
add_if_not_member(_, List, List).

unwrap_edge(e(FromState, Symbol, ToState), [FromState, ToState], Symbol).

%! get_all_mentioned_states_and_symbols(+Edges, -States, -Symbols)
% True if States is a list of all unique states from Edges and Symbols is a list of all unique symbols from Edges.
get_all_mentioned_states_and_symbols(Edges, States, Symbols) :-
	maplist(unwrap_edge, Edges, StatePairs, Symbols),
	flatten_matrix(StatePairs, States).

%! make_edgeless_dfa(+States, +Alphabet, +StartState, +AcceptStates, -DFA) 
% True if DFA has states States, alphabet Alphabet, start state StartState, accept states AcceptStates and from all states all symbols go to failState.
% failState must be a member of States.
make_edgeless_dfa(States, Alphabet, StartState, AcceptStates, dfa(States, Alphabet, TransitionFunction, StartState, AcceptStates)) :-
	constant_matrix(States, Alphabet, failState, TransitionFunction).

%! edfa_to_dfa(+EDFA, -DFA)
% Construct DFA from EDFA so each specified edge is preserved, failState is added if needed and all missing edges point to failState.
edfa_to_dfa(edfa(Edges, StartState, AcceptStates), DFA) :-
	get_all_mentioned_states_and_symbols(Edges, StatesList, SymbolsList),
	sort(StatesList, StatesWithoutFail),
	sort(SymbolsList, Alphabet),
	add_if_not_member(failState, StatesWithoutFail, States),
	member(StartState, States),
	maplist(member_reversed(States), AcceptStates),
	make_edgeless_dfa(States, Alphabet, StartState, AcceptStates, EdgelessDFA),
	foldl(set_edge, Edges, EdgelessDFA, DFA).

%! parentheses_from_operators(+OuterOperator, +InnerOperator, +Parenthesis)
% Prints Parenthesis if OuterOperator has lower priority than InnerOperator.
parentheses_from_operators(OuterOperator, InnerOperator, Parenthesis) :-
	operator_priority(OuterOperator, OOP),
	operator_priority(InnerOperator, IOP),
	OOP < IOP, !,
	write(Parenthesis).
parentheses_from_operators(_, _, _).

%! print_regex(+OuterOP, +Regex)
% Prints Regex.
% Encloses it in parentheses if the priority of OuterOP is lower than the priority of the outermost operation of Regex.
print_regex(OuterOP, WrappedRegex) :-
	wrapper(InnerOP, UnwrappedRegex, WrappedRegex),
	!,
	parentheses_from_operators(OuterOP, InnerOP, '('),
	operator_char(InnerOP, Char),
	maplist_or_call_with_delims(write(Char), print_regex(InnerOP), UnwrappedRegex),
	parentheses_from_operators(OuterOP, InnerOP, ')'), !.
print_regex(_, X) :-
	write(X).

%! print_regex(+OuterOP, +Regex)
% Prints Regex.
print_regex(Regex) :-
	print_regex(none, Regex).

%%%%%%%%%%%%%%%%%
dfa_to_regex :-
	writeln("enter a list of edges in format e(FromState, Symbol, ToState):"),
	read(Edges),
	writeln("enter the name of the start state:"),
	read(StartState),
	writeln("enter a list of accept states:"),
	read(AcceptStates),
	edfa_to_dfa(edfa(Edges, StartState, AcceptStates), DFA),
	make_regex(DFA, Regex),
	write("regex: "),
	print_regex(Regex).
%%%%%%%%%%%%%%%%%

set_start_states(lnfa(S, A, T, L, _, Ac), StartStates, lnfa(S, A, T, L, StartStates, Ac)).
set_accept_states(lnfa(S, A, T, L, St, _), AcceptStates, lnfa(S, A, T, L, St, AcceptStates)).

%! lambda_closure_dfs(+States, +Transitions, +Stack, +SeenStates, -Closure)
% True if Closure is the set of all visible states from all states in Stack, while ignoring SeenStates.
lambda_closure_dfs(_, _, [], Seen, Seen) :- !.
lambda_closure_dfs(States, Transitions, [StackH | StackT], SeenStates, Closure) :-
	member(StackH, SeenStates),
	!,
	lambda_closure_dfs(States, Transitions, StackT, SeenStates, Closure).
lambda_closure_dfs(States, Transitions, [StackH | StackT], SeenStates, Closure) :-
	get_element(States, Transitions, StackH, Transition),
	append(Transition, StackT, NewStack),
	lambda_closure_dfs(States, Transitions, NewStack, [StackH | SeenStates], Closure).

%! lambda_closure(+LNFA, +State, -Closure)
% True if Closure is the lambda closure of State in LNFA.
lambda_closure(LNFA, State, Closure) :-
	get_states(LNFA, States),
	get_lambda_transitions(LNFA, LambdaTransitions),
	lambda_closure_dfs(States, LambdaTransitions, [State], [], Closure).

%! lambda_closure_set(+LLNFA, +StatesList, -Closure)
% True if Closure is the lambda closure of StatesList in LLNFA.
lambda_closure_set(LLNFA, StatesList, Closure) :-
	get_lambda_closures(LLNFA, LambdaClosures),
	get_states(LLNFA, States),
	maplist(get_element(States, LambdaClosures), StatesList, ClosuresList),
	union_lists(ClosuresList, Closure).

%! add_edge(+Edge, +InputLNFA, -OutputLNFA)
% True if OutputLNFA is the same as InputLNFA with added edge Edge.
add_edge(
		e(FromState, ~, ToState),
		lnfa(States, Alphabet, TransitionFunction, LambdaTransitions, StartStates, AcceptStates),
		lnfa(States, Alphabet, TransitionFunction, NewLambdaTransitions, StartStates, AcceptStates)) :-
	!,
	transform_element(prepend_head(ToState), States, LambdaTransitions, FromState, NewLambdaTransitions).
add_edge(
		e(FromState, Symbol, ToState),
		lnfa(States, Alphabet, TransitionFunction, LambdaTransitions, StartStates, AcceptStates),
		lnfa(States, Alphabet, NewTransitionFunction, LambdaTransitions, StartStates, AcceptStates)) :-
	transform_element(prepend_head(ToState), TransitionFunction, States, Alphabet, FromState, Symbol, NewTransitionFunction).

%! make_edgeless_lnfa(+States, +Alphabet, +StartStates, +AcceptStates, -LNFA)
% True if LNFA has states States, alphabet Alphabet, start states StartStates, accept states AcceptStates and no edges.
make_edgeless_lnfa(States, Alphabet, StartStates, AcceptStates, lnfa(States, Alphabet, TransitionFunction, LambdaTransitions, StartStates, AcceptStates)) :-
	constant_list(States, [], LambdaTransitions),
	constant_list(Alphabet, [], EmptyRow),
	constant_list(States, EmptyRow, TransitionFunction).

%! elnfa_to_lnfa(+ELNFA, -LNFA)
% Converts ELNFA to equivalent LNFA.
elnfa_to_lnfa(elnfa(Edges, StartStates, AcceptStates), LNFA) :-
	get_all_mentioned_states_and_symbols(Edges, StatesL, SymbolsWithLL),
	sort(StatesL, States),
	sort(SymbolsWithLL, AlphabetWithL),
	delete_first(~, AlphabetWithL, Alphabet),
	make_edgeless_lnfa(States, Alphabet, StartStates, AcceptStates, EmptyLNFA),
	foldl(add_edge, Edges, EmptyLNFA, LNFA).

%! elnfa_to_lnfa(+ELNFA, -LNFA)
% Converts LNFA to equivalent LLNFA.
lnfa_to_llnfa(
		LNFA,
		llnfa(States, Alphabet, TransitionFunction, LambdaClosures, StartStates, AcceptStates)) :-
	get_states(LNFA, States),
	get_alphabet(LNFA, Alphabet),
	get_transition_function(LNFA, TransitionFunction),
	get_start_states(LNFA, StartStates),
	get_accept_states(LNFA, AcceptStates),
	maplist(lambda_closure(LNFA), States, LambdaClosures).

%! do_a_step_l(+LLNFA, +Symbol, +FromState, -ToStates)
% Gets the set of reachable states from FromState with Symbol in LLNFA.
% Ignores lambda edges.
do_a_step_l(llnfa(States, Alphabet, TransitionFunction, _, _, _), Symbol, FromState, ToStates) :-
	get_element(TransitionFunction, States, Alphabet, FromState, Symbol, ToStates).

%! process_l_one_state_step(+LLNFA, +Symbol, +FromStates, -ToStates)
% True if ToStates is a set of states Symbol gets the LLNFA to from the set FromStates.
% Uses lambda edges.
% Helper for process_l_one_state/4
process_l_one_state_step(LLNFA, Symbol, FromStates, ToStates) :-
	maplist(do_a_step_l(LLNFA, Symbol), FromStates, ToStatesLists),
	union_lists(ToStatesLists, ToStatesUnion),
	lambda_closure_set(LLNFA, ToStatesUnion, ToStates).

%! dprocess_l_one_state(+LLNFA, +Word, +FromState, -ToStates)
% True if ToStates is a set of states Word gets LLNFA to from FromState.
process_l_one_state(LLNFA, Word, FromState, ToStates) :-
	get_lambda_closure(LLNFA, FromState, FromStateClosure),
	foldl(process_l_one_state_step(LLNFA), Word, FromStateClosure, ToStates).

%! dprocess_l_one_state(+LLNFA, +Word, +FromState, -ToStates)
% True if LLNFA accepts Word.
process_l(LLNFA, Word) :-
	get_start_states(LLNFA, StartStates),
	maplist(process_l_one_state(LLNFA, Word), StartStates, ToStatesList),
	get_accept_states(LLNFA, AcceptStates),
	for_any(for_any(member_reversed(AcceptStates)), ToStatesList).

%! add_state(+InputLNFA, +State, -OutputLNFA)
% True if OutputLNFA is InputLNFA with added state State.
add_state(
		lnfa(States, Alphabet, TransitionFunction, LambdaTransitions, StartStates, AcceptStates),
		State,
		lnfa([State | States], Alphabet, [EmptyRow | TransitionFunction], [[] | LambdaTransitions], StartStates, AcceptStates)) :-
	constant_list(Alphabet, [], EmptyRow).

%! rename_list(+OldValues, +NewValues, +OldList, -NewList)
% Renames members of OldList according to bijection defined by OldValues and NewValue.
rename_list(OldValues, NewValues, OldList, NewList) :-
	maplist(get_element(OldValues, NewValues), OldList, NewList).
%! rename_list(+OldValues, +NewValues, +OldList, -NewList)
% Renames members of members of OldList according to bijection defined by OldValues and NewValue.
rename_matrix(OldValues, NewValues, OldMatrix, NewMatrix) :-
	maplist(rename_list(OldValues, NewValues), OldMatrix, NewMatrix).
%! rename_list(+OldValues, +NewValues, +OldList, -NewList)
% Renames members of members of members of OldList according to bijection defined by OldValues and NewValue.
rename_supermatrix(OldValues, NewValues, OldSuperMatrix, NewSuperMatrix) :-
	maplist(rename_matrix(OldValues, NewValues), OldSuperMatrix, NewSuperMatrix).

%! rename_list(+InputLNFA, +NewStates, -OutputLNFA)
% Renames states of InputLNFA to NewStates.
rename_states(
		lnfa(States, Alphabet, TransitionFunction, LambdaTransitions, StartStates, AcceptStates),
		NewStates,
		lnfa(NewStates, Alphabet, NewTransitionFunction, NewLambdaTransitions, NewStartStates, NewAcceptStates)) :-
	rename_supermatrix(States, NewStates, TransitionFunction, NewTransitionFunction),
	rename_matrix(States, NewStates, LambdaTransitions, NewLambdaTransitions),
	rename_list(States, NewStates, StartStates, NewStartStates),
	rename_list(States, NewStates, AcceptStates, NewAcceptStates).

filler(Difference, StartValue, _, EndValue, StartValue) :-
	EndValue is StartValue + Difference.

%! rename_list(+StartValue, +InputLNFA, -EndValue, -OutputLNFA)
% True if OutputLNFA is InputLNFA with states renamed to numbers starting with StartValue and EndValue is smallest unused number greater than StartValue.
rename_states(StartValue, InputLNFA, EndValue, OutputLNFA) :-
	get_states(InputLNFA, States),
	mapfoldl(filler(1), StartValue, States, EndValue, RenamedStates),
	rename_states(InputLNFA, RenamedStates, OutputLNFA).

reorder_list_default(Default, OldOrder, NewOrder, ListInput, ListOutput) :-
	maplist(get_element_default(Default, OldOrder, ListInput), NewOrder, ListOutput).

%! expand_alphabet(+NewAlphabet, +InputLNFA, -OutputLNFA)
% True if OutputLNFA is InputLNFA with alphabet NewAlphabet.
% All edges remain the same if the symbol is not removed by the operation.
expand_alphabet(
		NewAlphabet,
		lnfa(States, Alphabet, TransitionFunction, LambdaTransitions, StartStates, AcceptStates),
		lnfa(States, NewAlphabet, NewTransitionFunction, LambdaTransitions, StartStates, AcceptStates)) :-
	maplist(reorder_list_default([], Alphabet, NewAlphabet), TransitionFunction, NewTransitionFunction).

%! merge_lnfa(+LNFAList, -LNFA)
% True if LNFA is the disjunct union of LNFAList.
% States are renamed so none is lost in the union.
% Alphabet of LNFA is the union of alphabets of LNFAList.
% Edges are preserved.
% StartStates and AcceptStates are unions of their respective sets.
merge_lnfa(LNFAList, lnfa(MergedStates, MergedAlphabet, MergedTransitionFunction, MergedLambdaTransitions, MergedStartStates, MergedAcceptStates)) :-
	mapfoldl(rename_states, 0, LNFAList, _, RenamedLNFAList),

	maplist_flatten(get_states, RenamedLNFAList, MergedStates),

	maplist(get_alphabet, RenamedLNFAList, AlphabetsList),
	union_lists(AlphabetsList, MergedAlphabet),

	maplist(expand_alphabet(MergedAlphabet), RenamedLNFAList, ExpanedLNFAList),
	maplist_flatten(get_transition_function, ExpanedLNFAList, MergedTransitionFunction),

	maplist_flatten(get_lambda_transitions, RenamedLNFAList, MergedLambdaTransitions),
	maplist_flatten(get_start_states, RenamedLNFAList, MergedStartStates),
	maplist_flatten(get_accept_states, RenamedLNFAList, MergedAcceptStates).
	
make_lambda_edge_from_to(FromState, ToState, e(FromState, ~, ToState)).
make_lambda_edge_to_from(ToState, FromState, e(FromState, ~, ToState)).

%! add_lamba_edge_to_each(+LNFA, +FromState, +ToStates, -LNFAResult)
% True if LFNAResult is LNFA with added lambda edge from FromState to each state from ToStates.
add_lambda_edge_to_each(LNFA, FromState, ToStates, LNFAResult) :-
	maplist(make_lambda_edge_from_to(FromState), ToStates, Edges),
	foldl(add_edge, Edges, LNFA, LNFAResult).

%! add_lamba_edge_from_each(+LNFA, +FromStates, +ToState, -LNFAResult)
% True if LFNAResult is LNFA with added lambda edge to ToState from each state from FromStates.
add_lambda_edge_from_each(LNFA, FromStates, ToState, LNFAResult) :-
	maplist(make_lambda_edge_to_from(ToState), FromStates, Edges),
	foldl(add_edge, Edges, LNFA, LNFAResult).

%! union_lnfa(+LNFAList, -UnionLNFA)
% True if L(UnionLNFA) is the union of languages of each lnfa from LNFAList.
union_lnfa(LNFAList, UnionLNFA) :-
	merge_lnfa(LNFAList, MergedLNFA1),
	add_state(MergedLNFA1, end, MergedLNFA2),
	add_state(MergedLNFA2, start, MergedLNFA3),
	get_accept_states(MergedLNFA3, AcceptStates),
	add_lambda_edge_from_each(MergedLNFA3, AcceptStates, end, MergedLNFA4),
	get_start_states(MergedLNFA4, StartStates),
	add_lambda_edge_to_each(MergedLNFA4, start, StartStates, MergedLNFA5),
	set_accept_states(MergedLNFA5, [end], MergedLNFA6),
	set_start_states(MergedLNFA6, [start], UnionLNFA).

%! lambda_connect_pairs(+LNFA, +FromStates, +ToStates, -LNFAResult)
% True if LNFAResult is LNFA with correspoding pairs from FromStates and ToStates connected with a lambda edge.
lambda_connect_pairs(LNFA, FromStates, ToStates, LNFAResult) :-
	maplist(make_lambda_edge_from_to, FromStates, ToStates, Edges),
	foldl(add_edge, Edges, LNFA, LNFAResult).

%! concat_lnfa(+LNFAList, -ConcatLNFA)
% True if L(ConcatLNFA) is created by concatenation of languages of automata from LNFAList (in the order in which the appear in the list).
concat_lnfa(LNFAList, ConcatLNFA) :-
	merge_lnfa(LNFAList, MergedLNFA1),
	get_start_states(MergedLNFA1, [StartState | StartStatesT]),
	get_accept_states(MergedLNFA1, AcceptStates),
	append(AcceptStatesH, [AcceptState], AcceptStates),
	!,
	lambda_connect_pairs(MergedLNFA1, AcceptStatesH, StartStatesT, MergedLNFA2),
	set_start_states(MergedLNFA2, [StartState], MergedLNFA3),
	set_accept_states(MergedLNFA3, [AcceptState], ConcatLNFA).

%! iterate_lnfa(+LNFA, -IteratedLNFA)
% True if L(LNFA)* = L(IteratedLNFA).
iterate_lnfa(LNFA, IteratedLNFA) :-
	rename_states(0, LNFA, _, RenamedLNFA1),
	get_start_states(RenamedLNFA1, [StartStateOld]),
	get_accept_states(RenamedLNFA1, [AcceptStateOld]),
	add_state(RenamedLNFA1, end, RenamedLNFA2),
	add_state(RenamedLNFA2, start, RenamedLNFA3),
	add_edge(e(AcceptStateOld, ~, StartStateOld), RenamedLNFA3, RenamedLNFA4),
	add_edge(e(start, ~, StartStateOld), RenamedLNFA4, RenamedLNFA5),
	add_edge(e(AcceptStateOld, ~, end), RenamedLNFA5, RenamedLNFA6),
	add_edge(e(start, ~, end), RenamedLNFA6, RenamedLNFA7),
	set_start_states(RenamedLNFA7, [start], RenamedLNFA8),
	set_accept_states(RenamedLNFA8, [end], IteratedLNFA).

regex_to_lnfa_dispatch(hh, LNFAIn, LNFA) :-
	iterate_lnfa(LNFAIn, LNFA).
regex_to_lnfa_dispatch(pp, LNFAIn, LNFA) :-
	union_lnfa(LNFAIn, LNFA).
regex_to_lnfa_dispatch(bb, LNFAIn, LNFA) :-
	concat_lnfa(LNFAIn, LNFA).

%! regex_to_lnfa(+Regex, -LNFA)
% True if L(LNFA) = L(Regex).
regex_to_lnfa(Regex, LNFA) :-
	wrapper(XX, UnwrappedRegex, Regex),
	!,
	maplist_or_transform(regex_to_lnfa, UnwrappedRegex, LNFAs),
	regex_to_lnfa_dispatch(XX, LNFAs, LNFA).
regex_to_lnfa(@, lnfa([0,1], [], [[],[]], [[],[]], [0], [1])) :- !.
regex_to_lnfa(~, lnfa([0,1], [], [[],[]], [[1], []], [0], [1])) :- !.
regex_to_lnfa(Symbol, lnfa([0,1], [Symbol], [[[1]],[[]]], [[],[]], [0], [1])).

operand_to_list_regex_helper(pp, A p_op B, [BRegex | AList]) :-
	operand_to_list_regex(B, BRegex),
	operand_to_list_regex_helper(pp, A, AList), !.
operand_to_list_regex_helper(bb, A b_op B, [BRegex | AList]) :-
	operand_to_list_regex(B, BRegex),
	operand_to_list_regex_helper(bb, A, AList), !.
operand_to_list_regex_helper(_, A, [X]) :-
	operand_to_list_regex(A, X).

%! operand_to_list_regex(+OperatorsRegex, -ListFormRegex)
% Transform regex with operators to a list form regex.
operand_to_list_regex(A p_op B, pp(Y)) :-
	operand_to_list_regex_helper(pp, A p_op B, X),
	reverse(X, Y), !.
operand_to_list_regex(A b_op B, bb(Y)) :-
	operand_to_list_regex_helper(bb, A b_op B, X),
	reverse(X, Y), !.
operand_to_list_regex(A h_op, hh(X)) :-
	operand_to_list_regex(A, X), !.
operand_to_list_regex(A, A).

replace_all_helper(X, Y, X, Y).
replace_all_helper(_, _, Z, Z).

%! replace_all(+What, +With, +InputList, -OutputList)
% True if OutputList is InputList with all occurences of What replaced with With.
replace_all(What, With, ListInput, ListOutput) :-
	maplist(replace_all_helper(What, With), ListInput, ListOutput).

%! read_regex(-Regex)
% Reads regex from input
read_regex(Regex) :-
	read(RegexString),
	string_chars(RegexString, RegexChars1),
	replace_all('+', [' ', 'p', '_', 'o', 'p', ' '], RegexChars1, RegexChars2),
	replace_all('.', [' ', 'b', '_', 'o', 'p', ' '], RegexChars2, RegexChars3),
	replace_all('*', [' ', 'h', '_', 'o', 'p'], RegexChars3, RegexChars4),
	flatten_matrix(RegexChars4, RegexChars5),
	string_chars(OperandRegexString, RegexChars5),
	term_string(OperandRegex, OperandRegexString),
	operand_to_list_regex(OperandRegex, Regex).

make_edge(FromState, Symbol, ToState, e(FromState, Symbol, ToState)).
list_to_edges(FromState, Symbol, ToStates, Edges) :-
	maplist(make_edge(FromState, Symbol), ToStates, Edges).
row_to_edges(Alphabet, FromState, Row, EdgesList) :-
	maplist(list_to_edges(FromState), Alphabet, Row, EdgesList).
matrix_to_edges(States, Alphabet, TF, EdgesListList) :-
	maplist(row_to_edges(Alphabet), States, TF, EdgesListList).

%! regular_edges(-LNFA, +Edges)
% True if Edges is a list of all non-lambda edges in LNFA
regular_edges(LNFA, Edges) :-
	get_states(LNFA, States),
	get_alphabet(LNFA, Alphabet),
	get_transition_function(LNFA, TransitionFunction),
	matrix_to_edges(States, Alphabet, TransitionFunction, EdgesListList),
	flatten_matrix(EdgesListList, EdgesList),
	flatten_matrix(EdgesList, Edges).

list_to_lambda_edges(FromState, ToStates, Edges) :-
	list_to_edges(FromState, ~, ToStates, Edges).
row_to_lambda_edges(States, Row, EdgesList) :-
	maplist(list_to_lambda_edges, States, Row, EdgesList).

%! lambda_edges(-LNFA, +Edges)
% True if Edges is a list of all lambda edges in LNFA
lambda_edges(LNFA, Edges) :-
	get_states(LNFA, States),
	get_lambda_transitions(LNFA, LambdaTransitions),
	row_to_lambda_edges(States, LambdaTransitions, EdgesList),
	flatten_matrix(EdgesList, Edges).

%! write_edge(+Edge)
% Writes Edge to output.
write_edge(e(FromState, Symbol, ToState)) :-
	write(FromState),
	write(" -"),
	write(Symbol),
	write("-> "),
	write(ToState).

%%%%%%%%%%%%%%%%%
regex_to_lnfa :-
	writeln("enter a regex in quotes:"),
	read_regex(Regex),
	regex_to_lnfa(Regex, LNFA),
	write("States are: "),
	get_states(LNFA, States),
	maplist_with_delims(write(", "), write, States),
	writeln(""),
	write("Alphabet is: "),
	get_alphabet(LNFA, Alphabet),
	maplist_with_delims(write(", "), write, Alphabet),
	writeln(""),
	write("Regular edges are: "),
	regular_edges(LNFA, Edges),
	maplist_with_delims(write(", "), write_edge, Edges),
	writeln(""),
	write("Lambda edges are: "),
	lambda_edges(LNFA, LambdaEdges),
	maplist_with_delims(write(", "), write_edge, LambdaEdges),
	writeln(""),
	write("Start states are: "),
	get_start_states(LNFA, StartStates),
	maplist_with_delims(write(", "), write, StartStates),
	writeln(""),
	write("Accept states are: "),
	get_accept_states(LNFA, AcceptStates),
	maplist_with_delims(write(", "), write, AcceptStates).
%%%%%%%%%%%%%%%%%


% © 2020 Peter Fačko
