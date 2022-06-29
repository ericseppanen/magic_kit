## magic_kit: a container for shared singleton objects

__This crate is a work in progress.__

`magic_kit` can:
- autogenerate structs ("kits") that hold a bunch of singleton objects
- autogenerate adapters from one kit to another
- provide traits to allow on-demand lazy initialization of objects
- allow lazy-initialized kits and always-initialized kits to interoperate

A very basic example, using `magic_kit` to create always-initialized kits:
```rust

struct A {}
struct B {}
struct C {}

#[magic_kit(A, B, C)]
struct ABCKit {}

#[magic_kit(C, B)]
struct CBKit {}

let kit = ABCKit { /* details omitted */ };
let kit2 = CBKit::from(&kit);
```

The magic_kit attribute macro will automatically generate all the required code:
```rust
struct ABCKit {
    a: Arc<A>,
    b: Arc<B>,
    c: Arc<C>,
}

// These declare compatibility with any subset kit
impl KitGet<A> for ABCKit { /* details omitted */ }
impl KitGet<B> for ABCKit { /* details omitted */ }
impl KitGet<C> for ABCKit { /* details omitted */ }

#[magic_kit(C, B)]
struct CBKit {
    c: Arc<C>,
    b: Arc<B>,
}

// This will end up calling Arc::clone() on each member of the donor struct.
impl<K> From<&K> for CBKit
where
    K: KitGet<C> + KitGet<B>,
{
    fn from(kit: &K) -> Self {
        /* details omitted */
    }
}
```
