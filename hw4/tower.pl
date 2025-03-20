%Check if the grid is the right size
valid_col_size(R, N) :- length(R, N).

%Check if each row in the grid is the right size
valid_row_size([], _).
valid_row_size([H|T], N) :- length(H, N), valid_row_size(T, N).

%Check if the grid is a valid size
valid_size(G, N) :- valid_col_size(G, N), valid_row_size(G, N).

%Reverse all rows
reverse_all(G, ReversedG) :- maplist(reverse, G, ReversedG).

%Split Matrix into a Column and the rest of the Rows
grab_head([H|_], H).
grab_tail([_|T], T).

split_col([],[],[]).
split_col([H|T],C,R) :- 
    grab_head(H, Head),
    grab_tail(H, Tail),
    split_col(T, OldC, OldR),
    append([Head], OldC, C),
    append([Tail], OldR, R).

%If for every row in transposed T, it is equal to the corresponding column of T, then it is the transpose
transpose([], []).
transpose([[]|_], []).
transpose(T, [C|TransposedT]) :-
    split_col(T, C, R),
    transpose(R, TransposedT).

%Counts how many are visible from one list
count_visible(L, Total) :- 
    count(L, Count, 0),
    Total #= Count.

count([], 0, _).
count([H|T], Count, Max) :-
    H #> Max, 
    count(T, OldCount, H),
    Count #= OldCount + 1.
count([H|T], Count, Max) :-
    H #=< Max,
    count(T, Count, Max).

%Creates list of all visible
count_all_visible([],[]).
count_all_visible([GridH|GridT], [VisibleH|VisibleT]) :-
    count_visible(GridH, VisibleH),
    count_all_visible(GridT, VisibleT).


%domain checker
domain_valid_numbers([], _).
domain_valid_numbers([H|T], N) :- fd_domain(H, 1, N), domain_valid_numbers(T, N).

domain_all_unique(G) :- maplist(fd_all_different, G).

%Check if:
%   row, transpose, reverse, and reverse transpose has 
%   valid size
%   is a permutation of the list described by 1-N
%   has correct counts


%Checks all rows forward and backward
verify_rows_domain(_, [], [], []).
verify_rows_domain(N, [H|T], [FH|FT], [BH|BT]) :-
    length(H, N),
    reverse(H, RH),
    count(H, FH, 0),
    count(RH, BH, 0),
    verify_rows_forward_back(N, T, FT, BT).

ntower(0, [], counts([],[],[],[])).
ntower(N, T, counts(Top, Bottom, Left, Right)) :-
    valid_size(T, N),
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N),
    !,
    domain_valid_numbers(T, N),
    domain_all_unique(T),
    
    transpose(T, TransposedT),
    domain_all_unique(TransposedT),

    maplist(fd_labeling, T),

    verify_rows_domain(N, T, Left, Right),
    verify_rows_domain(N, TransposedT, Top, Bottom).


%
%Plain Specific Implementations
%

%Validate all rows 

%Check if they are made up of unique numbers
plain_row_unique(L) :- sort(L, SortedL), length(L, Len1), length(SortedL, Len2), Len1 == Len2.

%Check if they are within the domain values
plain_row_numbers(N, L) :- maplist(between(1, N), L).

%Counts how many are visible from one list
plain_count([], Accumulator, Count, _) :- Count is Accumulator.

plain_count([H|T], Accumulator, Count, Max) :-
    H > Max, 
    NewAccumulator is Accumulator + 1,
    plain_count(T, NewAccumulator, Count, H).

plain_count([H|T], Accumulator, Count, Max) :-
    H =< Max,
    plain_count(T, Accumulator, Count, Max).

%Checks all rows forward and backward
verify_rows_forward_back(_, [], [], []).
verify_rows_forward_back(N, [H|T], [FH|FT], [BH|BT]) :-
    length(H, N),
    maplist(between(1,N), H),
    plain_row_unique(H),
    reverse(H, RH),
    plain_count(H, 0, FH, 0),
    plain_count(RH, 0, BH, 0),
    verify_rows_forward_back(N, T, FT, BT).
    

plain_ntower(N, T, counts(Top, Bottom, Left, Right)) :- 
	valid_size(T, N),
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N),
    !,
    verify_rows_forward_back(N, T, Left, Right),
    transpose(T, TransposedT),
    verify_rows_forward_back(N, TransposedT, Top, Bottom).


%Speedup tests

test_ntower(Time) :- 
    statistics(cpu_time, [StartTime|_]),
    ntower(5,
        T,
        counts(
            [3,4,4,2,1],[2,2,1,3,2],[3,2,1,2,3],[1,2,3,4,2])),
    statistics(cpu_time, [FinishTime | _]),
    Time is FinishTime-StartTime.

test_plain_ntower(Time) :- 
    statistics(cpu_time, [StartTime|_]),
    plain_ntower(5,
        T,
        counts(
            [3,4,4,2,1],[2,2,1,3,2],[3,2,1,2,3],[1,2,3,4,2])),
    statistics(cpu_time, [FinishTime | _]),
    Time is FinishTime-StartTime.

speedup(TimeRatio) :-
    test_ntower(NtowerTime),
    test_plain_ntower(PlainNtowerTime),
    TimeRatio is PlainNtowerTime/NtowerTime.

%Ambiguous Towers

ambiguous(N, C, T1, T2) :-
    ntower(N, T1, C),
    ntower(N, T2, C),
    \+ T1 == T2.