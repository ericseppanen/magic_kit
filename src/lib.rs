use std::sync::Arc;

pub trait KitGet<T> {
    fn kitget(&self) -> Arc<T>;
}

pub trait Initializer<T> {
    fn create(kit: &T) -> Self;
}

pub use magic_kit_macros::{lazy_kit, magic_kit};
