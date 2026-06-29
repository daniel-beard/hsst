# hsst

hsst is a tiny, statically-typed, pipeline-oriented scripting language for transforming text on stdin.
The standard mode is run as a filter over stdin or pasteboard contents `String -> a`

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

Under the hood the pipeline is `parse → resolve to de Bruijn indices → infer → elaborate into a GADT core → evaluate` 

## Standard Library / Primitives

⚠️ This is under-documented, and will remain so until the core language design has settled down a bit. It's all very experimental.

A non-complete list of primitives:
- `Int`, `Bool`, `Char`, `List a`, `String` (actually `[Char]`), `Regex`
- map/filter/take/reverse

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

