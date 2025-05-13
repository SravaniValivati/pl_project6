% Entry point: the entire token list must be consumed exactly.
parse(Tokens) :-
    parse_lines(Tokens, []).

%% Lines → Line ; Lines | Line
parse_lines([], []) :- !.  % No more tokens left, valid end
parse_lines(In, Rest) :-
    parse_line(In, R1),
    ( R1 = [';'|R2]
    -> parse_lines(R2, Rest)
    ;  Rest = R1
    ).

%% Line → Num , Line | Num
parse_line(In, Rest) :-
    parse_num(In, R1),
    (   R1 = [','|R2]
    ->  parse_line(R2, Rest)
    ;   Rest = R1
    ).

%% Num → Digit | Digit Num
parse_num([D|Rest], Out) :-
    digit(D),
    parse_num_tail(Rest, Out).

parse_num_tail([D|Rest], Out) :-
    digit(D),
    parse_num_tail(Rest, Out).
parse_num_tail(Rest, Rest).

%% Digit → '0' | '1' | ... | '9'
digit(D) :-
    member(D, ['0','1','2','3','4','5','6','7','8','9']).

