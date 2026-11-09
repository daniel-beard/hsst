# hsst

hsst is a tiny, statically-typed, pipeline-oriented scripting language for transforming text on stdin.
The standard mode is run as a filter over stdin or pasteboard contents `String -> a` (or `[Char] -> a` when only a list-generic prim fits)

- Single expression: `words |> map(uppercase) |> take(2) |> unwords`
- `|>` is left-to-right composition
- Function application `f(a, b)` with automatic currying, e.g.
  - `hsst 'words("a b")'` is equivalent to `echo "a b" | hsst words`
- Lambdas 
  - `"ab" & map(\x -> upcaseChar(x))` or `"ab" & map(upcaseChar)`
- Hindley-Milner type inference
- Let bindings
  - Let-polymorphism - the same let bound function can be used on multiple types. 
    E.g. `echo "a b" | hsst 'let id = \x -> x in id |> words |> id'` (`id` is applied to `String` and `[String]`)
  - All types are monomorphised during type inference.
- Pairs
  - bifunctor-like operations
  - `("ab", 1) & first(uppercase)`
- ad-hoc polymorphism
  - E.g. `uppercase` can be applied to a `String` or a `Char`, `reverse` on a `String` is a grapheme cluster aware reverse, separate to `reverse` on a `List`

Under the hood the pipeline is `parse → resolve to de Bruijn indices → infer → elaborate into a GADT core → evaluate` 

## Standard Library / Primitives

⚠️ This is under-documented, and will remain so until the core language design has settled down a bit. It's all very experimental.

`hsst -h` shows a primitive's signatures (with a description and examples, where it's documented), or every primitive that mentions a type:

```
> hsst -h swap
swap :: (a, b) -> (b, a)

Swaps the order of the two values in a pair

>>> swap(("1", 2))
(2, "1")

> hsst -h uppercase
uppercase :: String -> String
uppercase :: Char -> Char

> hsst -h Regex
match :: Regex -> String -> String
matches :: Regex -> String -> Bool
```

Primitives are documented `doc` quasi quoter.
Examples are compile time checked, and the test suite runs every example against its expected output.

```haskell
prim "swap" (Scheme [a, b] $ tyPair a b ::-> tyPair b a) implSwap
  `documented` [doc|
    Swaps the order of the two values in a pair

    >>> swap(("1", 2))
    (2, "1")
  |],
```

A non-complete list of types (see the full list with `hsst -t`):
- `Int`, `Bool`, `Char`, `List a`, `String`, `Regex`, `Pair`

## Pairs

Pairs only: `(a, b)`, although they can be nested: `(a, (b, c))`

- Literals: `(1, "a")`. Passing to functions: `f((a, b))` or `(a, b) & f`
- Lambdas can destructure pairs: `\(w, n) -> n` or nested: `\(a, (b, c)) -> plus(a, c)`
- Program output of a pair is tab separated (one per line for a list of them), so it's easy to use with cut/awk/whatever.
- Output using `tee` shows as `("hello", 5)`
- See the help output `hsst -h Pair`

```
> echo "hello hsst" | hsst 'words |> map(fanout(id, length))'
hello	5
hsst	4

(this is an intentionally contrived example)
> echo "hi there" | hsst 'words |> map(\w -> (w, length(w))) |> map(\(w, n) -> n)'
2
5

> hsst 'zip(words("a b"), words("x y")) & map(swap)'
x	a
y	b
```

## Usage examples

```
echo "hello world" | hsst "words |> map(uppercase) |> take(1)"
["HELLO"]
```

```
> echo "hello world" | hsst "let id = \x -> x in id |> words |> map(uppercase) |> id"
["HELLO","WORLD"]
```

## Errors

Rust-c style errors that point to the exact source span that caused the issue:

```
  error: unbound variable: mp
   --> <arg>:1:10
    |
  1 | words |> mp(uppercase)
    |          ^^ not found in scope
```

```
> hsst "'c' |> upcaseChar"
error: expected a function, but got Char; |> composes functions -- use & to apply a value to a function
 --> <arg>:1:5
  |
1 | 'c' |> upcaseChar
  |     ^^ did you mean & ?
```

## Building

- Requirements
  - `brew install icu4c@78`
  - `ln -s /opt/homebrew/opt/icu4c@78/lib/pkgconfig/*.pc /opt/homebrew/lib/pkgconfig/` (so that pkg-config finds the library, it's not linked by default on macOS)

