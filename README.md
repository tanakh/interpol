# interpol
String interpolation macros for rust

This crate uses [proc_macro_hack](https://crates.io/crates/proc-macro-hack), so it works with stable version of rustc.

# Usage

Macros provided by this crate correspond to std's formatting macros (`format`,`print`, `println`, `eprint`, `eprintln`, ...).

You can use these macros just same as std's ones.

```rust
use interpol::println;

let a = 3;
let b = 4;
println!("{a} times {b} is {a * b}."); // => "3 times 4 is 12."
```

It is also able to specify formatting option:

```rust
let pi = 3.14;
println!("{pi:.10}"); // => "3.1400000000"
println!("{123:x}");  // => "7b"

let v = vec![1, 2, 3];
println!("{v:?}");     // => "[1, 2, 3]"
```

Correctly handles unicode characters:

```rust
let world = "世界";
println!("ハロー {world}"); // => "ハロー 世界"
```

It is able to write expressions:

```rust
println!("PI = {std::f64::consts::PI:.10}");
    // => "PI = 3.1415926536"

println!("PI = { 1.0_f64.atan() * 4.0 :.10}");
    // => "PI = 3.1415926536"
```

You can also write nested block expressions:

```rust
println!("{ { let t = 123; t * t } }"); // => "15129"

println!("{{ let t = 123; t * t }}");
    // "{{" and "}}" is escaped so the result is:
    // => "{ let t = 123; t * t }"

```
