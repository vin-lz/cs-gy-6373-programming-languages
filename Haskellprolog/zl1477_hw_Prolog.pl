/* Q1. Write a program that finds that the maximum of a list of numbers. Assume there are no duplicates in the list. */
max_in_list([X], X).
max_in_list([X, Y | R], Max) :-
    max_in_list([Y | R], MaxRest),
    max(X, MaxRest, Max).
max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- X < Y.


/* Q2. Write a program that succeeds if the intersection of two given list parameters is empty. */
empty_intersection([], X).
empty_intersection([H | T], X) :-
    not_a_member(H, X),
    empty_intersection(T, X).
not_a_member(Y, []).
not_a_member(Y, [H | T]) :-
    not_a_member(Y, T),
    Y \== H, !.


/* Q3. Write a program that returns a list containing the union of the elements of two given lists. Again, assume there are no duplicates in each list (although the same element might be in both lists). */
union_list([], X, X).
union_list([H | T], Y, Z) :-
    member_of(H, Y),
    union_list(T, Y, Z), !.
union_list([H | T], Y, [H | Z]) :-
    union_list(T, Y, Z).
member_of(X, [X | T]).
member_of(X, [H | T]) :-
    member_of(X, T).


/* Q4. Write a program that returns the final element of a list. */
final_element([X], X).
final_element([H | T], X) :-
    final_element(T, X), !.


/* Q5. Write a funtion that takes two lists of integers and returns a list containing tuples with corresponding elements from both the lists. */
product([], _, []).
product([H1 | T1], L2, R) :-
    put(H1, L2, R1),
    product(T1, L2, R2),
    append(R1, R2, R).
put(X, [], []).
put(X, [H | T], [[X, H] | R]) :-
	put(X, T, R).
