use magic_kit::{lazy_kit, magic_kit, Initializer, KitGet};
use std::sync::Arc;

#[allow(dead_code)]
#[test]
fn test1() {
    pub mod alpha {
        pub struct A {}
        impl A {
            pub fn hello(&self) {
                println!("hello from A");
            }
        }
    }

    struct B {
        s: String,
    }
    pub struct C {}
    struct D {}

    #[lazy_kit(alpha::A, B)]
    struct LazyKitABCD {
        pub c: Arc<C>,
        d: D,
    }

    impl Initializer<LazyKitABCD> for alpha::A {
        fn create(_kit: &LazyKitABCD) -> Self {
            println!("A::create");
            alpha::A {}
        }
    }

    impl Initializer<LazyKitABCD> for B {
        fn create(kit: &LazyKitABCD) -> Self {
            println!("B::create");
            kit.a.hello();
            B {
                s: "Bee string".to_string(),
            }
        }
    }

    #[magic_kit(alpha::A, B)]
    struct CompKitAB {}

    let kit = LazyKitABCD::new(Arc::new(C {}), D {});

    // This won't do automatic create on deref.
    if false {
        kit.a.hello();
    }
    // But either of these will:
    kit.get::<alpha::A>().hello();
    KitGet::<alpha::A>::kitget(&kit).hello();

    let kit2 = CompKitAB::from(&kit);

    #[magic_kit(B)]
    struct CompKitB {}

    let kit3 = CompKitB::from(&kit2);
    let kit4: CompKitB = (&kit).into();
    assert_eq!(kit4.get::<B>().s, "Bee string");
    assert_eq!(&kit4.b.s, "Bee string");
    assert_eq!(KitGet::<B>::kitget(&kit3).s, "Bee string");

    // This one requires LazyKitABCD to implement KitGet<C>.
    #[magic_kit(C, B)]
    struct CompKitCB {}

    let _kit5 = CompKitCB::from(&kit);
}
