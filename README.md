# hsst

hsst is a tiny, statically-typed, pipeline-oriented scripting language for transforming text on stdin. 
You write a single expression that gets fed standard input — e.g. `words |> map(uppercase) |> take(2) |> unwords` — using left-to-right composition (|>), function application (f(a, b)), lambdas (\x -> ...), and let bindings. 
It ships with a standard library of primitives for strings, lists, ints, and bools (words/lines, uppercase/base64, map/filter/take/reverse,
  plus/minus, etc.). 

It's fully type-safe: it runs full Hindley-Milner type inference so you never write type annotations, and it supports let-polymorphism (the same let-bound function can be used at multiple types). 

Under the hood the pipeline is `parse → resolve to de Bruijn indices → infer → elaborate into a strongly-typed GADT core → evaluate`, where the elaboration step makes the interpreter total by construction — only well-typed programs are even representable, so there are no runtime type errors. If your program has type String -> a it's applied to stdin, otherwise it's just evaluated and printed.

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

## TODOs

- [ ] Many more tests
- [ ] More Prims
- [ ] Eventual rank-2 polymorphism (maybe adhoc polymorphism as well?)
