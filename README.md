# roman.erl

A [Roman numeral][roman] calculator written in Erlang.

# Features

 - Adds **Roman numeral literals** to Erlang!
 - **Never converts numerals into integers** — be assured that
   your calculations remain in Roman numerals throughout the entire
   process!
 - Absolutely **no concept of zero**.
 - Supports calculations in the range of **Ⅰ** to **ⅯⅯⅯⅭⅯⅩⅭⅨ**, inclusive.

[roman]: https://en.wikipedia.org/wiki/Roman_numerals

## Usage

Right now, you have to compile it yourself, but you can pop into the
`erl` console and do the following:

```erl
> 1> c(roman).
{ok,roman}
> 2
2> roman:add(ii, ii).
iv
3> roman:add(cdxx, lxix).
cdlxxxix
4> roman:subtract(mmi, mcmlxviii).
xxxiii
```

# Why?

I overheard somebody saying that creating a calculator without
converting Roman numeral into integers would be painful. They were
right.

I chose to implement it in Erlang because I saw the opportunity to fake
literals into the language without having to modify the language's
parser. Also, pattern matching rules.

# How it works

It [parses](#parser) atoms as Roman numerals into an [internal
representation](#internal-representation). Calculations are done by
incrementing or decrementing numbers one-by-one.

# Parser

The parser is a "generic" finite state machine. Since the same logic
that can be used to parse units (Ⅰ) can be reused to parse tens (Ⅹ) and
hundreds (Ⅽ), the finite state machine can be parameterized for t unit
(Ⅰ), half (Ⅴ), and next (Ⅹ) letter.

![Generic finite state acceptor for Roman
numerals](https://g.gravizo.com/source/fsm?https%3A%2F%2Fraw.githubusercontent.com%2Feddieantonio%2Froman.erl%2Fmain%2FREADME.md)

<details>
<summary></summary>
fsm
 digraph G {
    rankdir = "LR";
    node [shape = doublecircle];
    start -> i [label="i"];
    start -> v [label="v"];
    i -> iv [label = "v"];
    i -> ii [label = "i"];
    i -> ix [label = "x"];
    ii -> iii  [label = "i"];
    v -> vi  [label = "i"];
    vi -> ii  [label = "i"];
    start [shape=point];
    ii [label = "ii or vii"];
    iii [label = "iii or viii"];
  }
fsm
</details>

## Internal Representation

Numbers are represented internally as a non-empty list of decimal
terms.

Each term in the list is a tuple of `{Magnitude, Value}`, where
`Magnitude` can be one of `unus` (units), `decem` (tens), `centum`
(hundreds), and—to a limited extent—`mille` (thousands). `Value` is the
literal Roman numeral for that decimal place.

Terms are always ordered from least significant to most significant
term. There is no zero value, so terms that would ordinarily be zero in
place-value notation are omitted from the list.

Examples:

 - Ⅳ = `[{unus, iv}]`
 - ⅩⅩⅦ = `[{unus, vii}, {decem, xx}]`
 - ⅮⅬⅤ = `[{unus, v}, {decem, l}, {centum, d}]`
 - ⅯⅯⅩⅡ = `[{unus, ii}, {decem, xx}, {mille, ii}]`

# License

MIT License.
