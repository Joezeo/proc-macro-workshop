// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

use std::marker::PhantomData;

use derive_builder::Builder;
use derive_debug::CustomDebug;

#[derive(Builder)]
#[allow(dead_code)]
pub struct Command {
    executable: String,
    #[builder(each = "arg")]
    args: Vec<String>,
    current_dir: Option<String>,
}

pub trait Trait {
    type Type;
}

#[derive(CustomDebug)]
pub struct Struct<T, U, W: Trait> {
    val: String,
    #[debug = "0b{:08b}"]
    num: u8,
    u: U,
    phantomdata: PhantomData<T>,
    tit: PhantomData<W::Type>,
}

fn main() {
    let _ = Command::builder().executable("executable".into()).build().unwrap();
}
