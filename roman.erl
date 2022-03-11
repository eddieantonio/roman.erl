%% roman -- Roman Numerals in Erlang
%%
%% @doc Allows you to add and subtract Roman numerals in pure Erlang, with
%% Roman numeral "literals":
%%
%% ```
%% roman:add(ii, ii) =:= iv.
%% roman:subtract(v, iii) =:= ii.
%% '''
%%
%% Finally! A way to do calculations on movie copyright years!
%%
%% ```
%% roman:subtract(mmi, mcmlxviii) =:= xxxiii.
%% '''
%%
%% At no point does any of this code attempt to convert anything into an
%% integer. In fact, apart from Erlang's function_notation/0, there are no
%% digits in the source code whatsoever. All arithmetic is done
%% "symbolically", and without using your computer's integer arithmetic
%% hardware. Give those adders a break!
%%
%% Other fun facts:
%%  - This code has no concept of zero.
%%  - Since this code only uses "standardized" Roman numerals, the maximum
%%    value is ⅯⅯⅯⅭⅯⅩⅭⅠⅩ (three thousand, nine hundred ninety nine).
%%
%% This gives a total range of Ⅰ to ⅯⅯⅯⅭⅯⅩⅭⅠⅩ, inclusive.
%%
%% Also, I used Google Translate to name as many variables as I could in
%% Latin, because... quidnī?
%% (I don't remember enough from tenth grade Latin to know whether any of the
%% translated names are GOOD, so they are most likely BAD).

-module(roman).

-export([add/2, subtract/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ====================================================================
%% Public API
%% ====================================================================

%% @doc Adds two Roman numerals together.
add(A, B) ->
    Summa = add_internus(parse(A), parse(B)),
    scribe(Summa).

%% @doc Subtracts the second Roman numeral from the first.
subtract(A, B) ->
    Differentia = sub_internus(parse(A), parse(B)),
    scribe(Differentia).

%% ====================================================================
%% Internal Functions
%% ====================================================================

%% The internal representation of roman numerals is a list of decimal places.
%% Decimal places are ordered in INCREASING magnitude. That is, the lowest
%% magnitude item is first, and the highest magnitude item is last.
%%
%% This is **technically** not a place value system, since if a decimal place
%% does not participate in a number, it is absent from the list, rather than
%% having a value of zero. It is also **technically** annoying.
%%
%% Examples of valid internal representations:
%%
%%   i      ≡ [{unus, i}]
%%   x      ≡ [{decem, x}]
%%   c      ≡ [{centum, c}]
%%   m      ≡ [{mille, m}]
%%
%% Values are in increasing magnitude, and intervening decimal places are
%% absent:
%%
%%   mdcvii ≡ [{unus, vii}, {decem, dc}, {mille, m}]
%%
%% Notice the lack of a {decem, _} term!
-type numerum() ::[valorem(), ...].
%% A decimal place can be ones, tens, hundreds, thousands:
-type valorem() :: unus() | decem() | centum() | mille().
%% And these enumerate all possible value within that decimal place:
-type unus() :: {unus, i | ii | iii | iv | v | vi | vii | viii | ix}.
-type decem() :: {decem, x | xx | xxx | xl | l | lx | lxx | lxxx | xc}.
-type centum() :: {centum, c | cc | ccc | cd | d | dc | dcc | dccc | cm}.
%% note bene: in "standardized" Roman numerals, there's no way to count from
%% four thousand and beyond:
-type mille() :: {mille, m | mm | mmm }.

%% @doc Write a Roman numeral as an atom.
-spec scribe(numerum()) -> atom().
scribe(Numerum) ->
    DecreasingInMagntiude = lists:reverse(Numerum),
    ListOfLists = [atom_to_list(I) || {_, I} <- DecreasingInMagntiude],
    list_to_atom(lists:concat(ListOfLists)).

%% ====================================================================

%% The way adding works is by decomposing the addition into
%% a bunch of adding ones.
%%
%% Say you want the sum x + v (ten plus five). We know:
%%  - how to increment a number by one
%%  - how to decrement a number by one
%%
%% Why, that's the same as the sum of one more than first and one less than
%% the second:
%%
%%      x + v =:= (x + i) + (v - i)
%%      x + v =:= xi + iv
%%
%% Continue this process; at each step, the first number gets one bigger, and
%% the second number gets one smaller:
%%
%%      x + v =:= (x + i) + (v - i)
%%      x + v =:= xi + iv
%%                xi + iv =:= (xi + i) + (iv - i)
%%                xi + iv =:= xii + iii
%%                            xii + iii =:= (xii + i) + (iii - i)
%%                            xii + iii =:= xiii + ii
%%
%% Eventually, the second number becomes one. Since we know how to add one to
%% a number, we return the end result
%%
%%      x + v =:= xiv + i
%%      x + v =:= xv
%%
%% @see increment/1
%% @see decrement/1
-spec add_internus(numerum(), numerum()) -> numerum().
add_internus(A, [{unus, i}]) ->
    increment(A);
add_internus(A, B) ->
    ASucc = increment(A),
    BPred = decrement(B),
    add_internus(ASucc, BPred).

%% Subtracting works similarly to adding.
%% In this case, we are taking away from both numbers by the same amount.
%%
%% We know how to take away one from a number. So we repeat this process:
%%
%%      x - v =:= (x - i) - (v - i)
%%      x - v =:= ix - iv
%%
%%                ix - iv =:= (ix - i) - (iv - i)
%%                ix - iv =:= viii - iii
%%
%%                            viii - iii =:= (viii - i) - (iii - i)
%%                            viii - iii =:= vii - ii
%%
%% Eventually, the second number becomes one. Since we know how to subtract
%% one from a number, we return the result:
%%
%% x - v =:= vi - i
%% x - v =:= v
%%
%% @see add_internus/2
%% @see decrement/1
-spec sub_internus(numerum(), numerum()) -> numerum().
sub_internus(A, [{unus, i}]) ->
    decrement(A);
sub_internus(A, B) ->
    sub_internus(decrement(A), decrement(B)).

%% ====================================================================

%% @doc add one to a number.
-spec increment(numerum()) -> numerum().
increment(Numerum=[{unus, _}|_]) ->
    %% Incrementing when there is a ones place in a number is straightforward:
    incrementum_generalis(Numerum);
increment(Etc) ->
    %% However, when the lowest decimal place is tens or larger, adding one
    %% means adding the term {unus, i} to entire number.
    [primus(unus)|Etc].

%% Increments any decimal place.
incrementum_generalis([{Magnitudo, I}|Etc]) ->
    case succssorem(I) of
        %% We must carry over into the next decimal place:
        carry -> propagate_carry(Magnitudo, Etc);
        %% No carry required; we keep the same decimal place:
        II -> [{Magnitudo, II}|Etc]
    end.

%% When handling carry, we need to know, what decimal initiated the carry.
propagate_carry(ExMagnitudo, []) ->
    %% In the case that there is a carry, and there is not a decimal place
    %% high enough to absorb it, that means that we need to spontaneously
    %% create the immediate NEXT decimal place into existence:
    [primus(diende(ExMagnitudo))];
propagate_carry(ExMagnitudo, Numerum=[{Magnitudo, _}|_]) ->
    %% If there is something bigger, we need to check whether it's...
    case MagnitudoProximo = diende(ExMagnitudo) of
        %% ...the immediately next decimal place:
        %% e.g., xix + i ≡ [{unus, ix}, {decem, x}] + i
        %%               ≡ [            {decem, xx}]
        Magnitudo -> incrementum_generalis(Numerum);
        %% ...or an even larger decimal place, so we need to vivify the
        %% decimal place in between:
        %% e.g., cix + i ≡ [{unus, ix},             {centum, c}]
        %%               ≡ [            {decem, x}, {centum, c}]
        _ -> [primus(MagnitudoProximo)|Numerum]
    end.

%% The next decimal place:
diende(unus) -> decem;
diende(decem) -> centum;
diende(centum) -> mille;
diende(mille) -> error(nimis_magna).

%% The "first" of a decimal place.
primus(unus) -> {unus, i};
primus(decem) -> {decem, x};
primus(centum) -> {centum, c};
primus(mille) -> {mille, m}.

%% ====================================================================

%% @doc take one away from a number.
-spec decrement(numerum()) -> numerum().
decrement([{Magnitudo, II}|Etc]) ->
    ante(Magnitudo) ++ case praedecessor(II) of
                           % If we have to borrow, this term disappears from
                           % existence:
                           borrow -> Etc;
                           I -> [{Magnitudo, I}|Etc]
                       end.

%% What number is immediately before a decimal place?
ante(mille) -> ante(centum) ++ [{centum, cm}];
ante(centum) -> ante(decem) ++ [{decem, xc}];
ante(decem) -> [{unus, ix}];
ante(unus) -> [].


%% @doc What numeral comes next for THIS decimal place?
% Special cases:
succssorem(m) -> mm;
succssorem(mm) -> mmm;
succssorem(mmm) -> error(nimis_magna);
% General case:
succssorem(Numerum) ->
    Partes = split_atom(Numerum),
    Simbola = simbola(magnitudo(Partes)),
    case succssorem(Simbola, Partes) of
        carry -> carry;
        PartesNovem -> fuse_atom(PartesNovem)
    end.

%% The successor of any numeral looks the same, regardless of its decimal
%% place. This table generalizes adding one to any numeral, with your
%% choice of {I, V, X} := {i, v, x} | {x, l, c} | {c, d, m}.
%%
%% @see simbola/1
succssorem({I, _, _}, [I])          -> [I, I];
succssorem({I, _, _}, [I, I])       -> [I, I, I];
succssorem({I, V, _}, [I, I, I])    -> [I, V];
succssorem({I, V, _}, [I, V])       -> [V];
succssorem({I, V, _}, [V])          -> [V, I];
succssorem({I, V, _}, [V, I])       -> [V, I, I];
succssorem({I, V, _}, [V, I, I])    -> [V, I, I, I];
succssorem({I, V, X}, [V, I, I, I]) -> [I, X];
succssorem({I, _, X}, [I, X])       -> carry.

%% @doc What numeral comes immediately before for THIS decimal place?
% Special cases:
praedecessor(m)     -> borrow;
praedecessor(mm)    -> m;
praedecessor(mmm)   -> mm;
% General case
praedecessor(Numerum) ->
    Partes = split_atom(Numerum),
    Simbola = simbola(magnitudo(Partes)),
    case praedecessor(Simbola, Partes) of
        borrow -> borrow;
        PartesNovem -> fuse_atom(PartesNovem)
    end.

%% The predecessor of any numeral looks the same, regardless of its decimal
%% place. This table generalizes taking one away from any numeral, with your
%% choice of {I, V, X} := {i, v, x} | {x, l, c} | {c, d, m}.
%%
%% @see simbola/1
praedecessor({I, _, _}, [I])          -> borrow;
praedecessor({I, _, _}, [I, I])       -> [I];
praedecessor({I, _, _}, [I, I, I])    -> [I, I];
praedecessor({I, V, _}, [I, V])       -> [I, I, I];
praedecessor({I, V, _}, [V])          -> [I, V];
praedecessor({I, V, _}, [V, I])       -> [V];
praedecessor({I, V, _}, [V, I, I])    -> [V, I];
praedecessor({I, V, _}, [V, I, I, I]) -> [V, I, I];
praedecessor({I, V, X}, [I, X])       -> [V, I, I, I].

%% @doc What is the decimal place of a term?
magnitudo([i|_]) -> unus;
magnitudo([v|_]) -> unus;
magnitudo([x|_]) -> decem;
magnitudo([l|_]) -> decem;
magnitudo([c|_]) -> centum;
magnitudo([d|_]) -> centum;
magnitudo([m|_]) -> mille.

%% @doc What letters are used to represent this decimal place?
simbola(unus) -> {i, v, x};
simbola(decem) -> {x, l, c};
simbola(centum) -> {c, d, m};
% Note: in practice, this is never used:
simbola(mille) -> {m}.

%% ====================================================================

%% @doc Parses a "Roman numeral literal" (really, an atom) into the internal
%% representation.
-spec parse(atom()) -> numerum().
parse(Term) ->
    AtomList = split_atom(Term),
    {Mille, EtceteraI} = parse_mille(AtomList),
    {Centum, EtceteraII} = parse_generalis(centum, {c, d, m}, EtceteraI),
    {Decem, EtceteraIII} = parse_generalis(decem, {x, l, c}, EtceteraII),
    {Unus, []} = parse_generalis(unus, {i, v, x}, EtceteraIII),
    Unus ++ Decem ++ Centum ++ Mille.

%% @doc Returns a parsed numeral, if any, and a list of leftover characters.
parse_generalis(Nomen, {I, V, X}, AtomList) ->
    {Parse, Etc} = parse_generalis_initium({I, V, X}, AtomList, []),
    Term = case Parse of
        % Did not parse anything (everything is leftover):
        [] -> [];
        % Parsed something! The accepted string is in reverse:
        Reversed -> [{Nomen, fuse_atom(lists:reverse(Reversed))}]
    end,
    {Term, Etc}.

%% @doc Implements a "generic" finite state machine (FSM) that accepts Roman
%% numerals at a given order of magnitude. The FSM is parameterized by the
%% symbols used for its ones (I), its fives (V), and its tens (X).
%%
%% Note: accepted atoms are returned in REVERESED order!
%%
%% See the README for a diagram of this state machine.
parse_generalis_initium({I, V, X}, [I|Etc], Acc) -> i({I, V, X}, Etc, [I|Acc]);
parse_generalis_initium({I, V, X}, [V|Etc], Acc) -> v({I, V, X}, Etc, [V|Acc]);
parse_generalis_initium({_, _, _}, Etc, []) -> {[], Etc}.
i({I, V, X}, [I|Etc], Acc) -> ii({I, V, X}, Etc, [I|Acc]);
i({I, V, X}, [V|Etc], Acc) -> iv({I, V, X}, Etc, [V|Acc]);
i({I, V, X}, [X|Etc], Acc) -> ix({I, V, X}, Etc, [X|Acc]);
i({_, _, _}, Etc, Acc) -> {Acc, Etc}.
ii({I, V, X}, [I|Etc], Acc) -> iii({I, V, X}, Etc, [I|Acc]);
ii({_, _, _}, Etc, Acc) -> {Acc, Etc}.
iii({_, _, _}, Etc, Acc) -> {Acc, Etc}.
iv({_, _, _}, Etc, Acc) -> {Acc, Etc}.
v({I, V, X}, [I|Etc], Acc) -> vi({I, V, X}, Etc, [I|Acc]);
v({_, _, _}, Etc, Acc) -> {Acc, Etc}.
vi({I, V, X}, [I|Etc], Acc) -> ii({I, V, X}, Etc, [I|Acc]);
vi({_, _, _}, Etc, Acc) -> {Acc, Etc}.
ix({_, _, _}, Etc, Acc) -> {Acc, Etc}.

%% Parse thousands. Special cased, because the Romans could not count beyond
%% MMMCMXCIX.
parse_mille([m,m,m|Etc]) -> {[{mille, mmm}], Etc};
parse_mille([m,m|Etc]) -> {[{mille, mm}], Etc};
parse_mille([m|Etc]) -> {[{mille, m}], Etc};
parse_mille(Etc) -> {[], Etc}.

%% ====================================================================

%% @doc Returns a list of single character atoms taken from the given atom.
-spec split_atom(atom()) -> [atom()].
split_atom(Atom) ->
    [list_to_atom([Char]) || Char <- atom_to_list(Atom)].

%% @doc Concatenates a list of atoms into a single atom.
-spec fuse_atom(['i' | 'v' | 'x' | 'l' | 'c' | 'd' | 'm', ...]) -> atom().
fuse_atom(AtomList) ->
    ListOfLists = [atom_to_list(Char) || Char <- AtomList],
    list_to_atom([Char || [Char] <- ListOfLists]).

%% ====================================================================
%% Unit Tests
%% ====================================================================

-ifdef(EUNIT).
% largest value:
maximum() -> mmmcmxcix.
% before the largest value:
ante_maximum() -> mmmcmxcviii.
-endif.

-ifdef(EUNIT).
add_test() ->
    [?assertEqual(ii, add(i, i)),
     ?assertEqual(iv, add(ii, ii)),
     ?assertEqual(mmxx, add(i, mmxix)),
     ?assertEqual(cxi, add(lxix, xlii)),
     ?assertEqual(cdlxxxix, add(lxix, cdxx)),
     ?assertEqual(lxix, add(lxviii, i)), % commendatus
     ?assertEqual(maximum(), add(ante_maximum(), i))].
-endif.

-ifdef(EUNIT).
sub_test() ->
    [?assertEqual(i, subtract(ii, i)),
     ?assertEqual(ii, subtract(iv, ii)),
     ?assertEqual(xxvii, subtract(lxix, xlii)),
     ?assertEqual(xxx, subtract(mmxxii, mcmxcii)),
     ?assertEqual(ante_maximum(), subtract(maximum(), i)),
     ?assertEqual(i, subtract(maximum(), ante_maximum()))].
-endif.

-ifdef(EUNIT).
decrement_test() ->
    [?assertEqual(parse(mmccxix), decrement(parse(mmccxx))),
     ?assertEqual(parse(mmcxcix), decrement(parse(mmcc)))].
-endif.

-ifdef(EUNIT).
parse_unum_test() ->
    [?assertEqual([{unus, i}], parse(i)),
     ?assertEqual([{unus, ii}], parse(ii)),
     ?assertEqual([{unus, iii}], parse(iii)),
     ?assertEqual([{unus, iv}], parse(iv)),
     ?assertEqual([{unus, v}], parse(v)),
     ?assertEqual([{unus, vi}], parse(vi)),
     ?assertEqual([{unus, vii}], parse(vii)),
     ?assertEqual([{unus, viii}], parse(viii)),
     ?assertEqual([{unus, ix}], parse(ix))].
-endif.

-ifdef(EUNIT).
parse_mixed_test() ->
     [?assertEqual([{unus, ix}, {decem, lx}], parse(lxix)),
      ?assertEqual([{decem, xx}, {centum, cd}], parse(cdxx))].
-endif.

-ifdef(EUNIT).
parse_invalid_test() ->
    [?assertError(_, parse(viv)),
     ?assertError(_, parse(vix))].
-endif.

-ifdef(EUNIT).
split_test() ->
    [?assertEqual([c, x, i], split_atom(cxi))].
-endif.

-ifdef(EUNIT).
join_test() ->
    [?assertEqual(cxi, fuse_atom([c, x, i]))].
-endif.
