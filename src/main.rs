use std::rc::Rc;

use anyhow::Result;
use rand::prelude::ThreadRng;

use crate::dice::{Die, FateDie, SimpleDie};

mod dice;

fn main() -> Result<()> {
    println!("Hello, world!");

    let mut dice: Vec<Rc<dyn Die>> = vec![];
    dice.push(SimpleDie::new(6)?);
    dice.push(SimpleDie::new(20)?);
    dice.push(FateDie::new());

    let mut rng = ThreadRng::default();
    for die in &dice {
        let roll = die.clone().roll(&mut rng);
        println!("{} roll: {}", die, roll.face);
    }

    Ok(())
}
