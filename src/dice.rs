use std::{
    borrow::Cow,
    fmt::{self, Debug, Display},
    rc::Rc,
};

use rand::{Rng, RngCore};

use crate::errors::DieError;

pub type Value = i16;

#[derive(Debug)]
pub enum Face {
    Numeric(Value),
    NamedNumeric(Cow<'static, str>, Value),
}

impl Face {
    pub fn value(&self) -> Option<Value> {
        match self {
            Face::Numeric(value) => Some(*value),
            Face::NamedNumeric(_, value) => Some(*value),
        }
    }
}

impl Display for Face {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Face::Numeric(value) => write!(f, "{}", value),
            Face::NamedNumeric(name, _) => write!(f, "{}", name),
        }
    }
}

pub trait Die: Display + Debug {
    fn roll(self: Rc<Self>, rng: &mut dyn RngCore) -> Rc<Roll>;
}

#[derive(Debug)]
pub struct Roll {
    pub die: Rc<dyn Die>,
    pub face: Face,
    pub discarded: bool,
}

impl Roll {
    pub fn new(die: Rc<dyn Die>, face: Face) -> Roll {
        Roll {
            die,
            face,
            discarded: false,
        }
    }
}

#[derive(Debug)]
pub struct SimpleDie {
    faces: Value,
}

impl SimpleDie {
    pub fn new(faces: Value) -> Result<Rc<SimpleDie>, DieError> {
        if faces > 0 {
            Ok(Rc::new(SimpleDie { faces }))
        } else {
            Err(DieError::InvalidFaceCount { faces })
        }
    }
}

impl Display for SimpleDie {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "d{}", self.faces)
    }
}

impl Die for SimpleDie {
    fn roll(self: Rc<Self>, rng: &mut dyn RngCore) -> Rc<Roll> {
        let face = Face::Numeric(rng.gen_range(1..=self.faces));
        Rc::new(Roll::new(self.clone(), face))
    }
}

#[derive(Debug)]
pub struct FateDie {}

impl FateDie {
    pub fn new() -> Rc<FateDie> {
        Rc::new(FateDie {})
    }
}

impl Display for FateDie {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "dF")
    }
}

impl Die for FateDie {
    fn roll(self: Rc<Self>, rng: &mut dyn RngCore) -> Rc<Roll> {
        let value = rng.gen_range(-1..=1);
        let label = if value < 0 {
            "-"
        } else if value > 0 {
            "+"
        } else {
            "0"
        };
        let face = Face::NamedNumeric(Cow::Borrowed(label), value);
        Rc::new(Roll::new(self.clone(), face))
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use rand::rngs::mock::StepRng;

    use super::*;

    prop_compose! {
        /// A deterministic random number generator, with a seed chosen by
        /// `proptest`.
        fn rng()(seed in any::<u64>()) -> StepRng {
            StepRng::new(seed, 17)
        }

    }

    /// Generate a simple die in one of the usual sizes.
    fn simple_die() -> impl Strategy<Value = Rc<SimpleDie>> {
        prop_oneof![
            Just(SimpleDie::new(2).unwrap()),
            Just(SimpleDie::new(4).unwrap()),
            Just(SimpleDie::new(6).unwrap()),
            Just(SimpleDie::new(8).unwrap()),
            Just(SimpleDie::new(10).unwrap()),
            Just(SimpleDie::new(12).unwrap()),
            Just(SimpleDie::new(20).unwrap()),
            (1..60i16).prop_map(|faces| SimpleDie::new(faces).unwrap()),
        ]
    }

    /// Generate die of a common sort.
    fn die() -> impl Strategy<Value = Rc<dyn Die>> {
        prop_oneof![
            simple_die().prop_map(|d| d as Rc<dyn Die>),
            Just(FateDie::new() as Rc<dyn Die>),
        ]
    }

    proptest! {
        #[test]
        fn dice_and_rolls_have_names(mut rng in rng(), die in die()) {
            assert!(die.to_string().len() > 0);
            let roll = die.clone().roll(&mut rng);
            assert!(roll.face.to_string().len() > 0);
        }

        #[test]
        fn simple_die_always_in_range(mut rng in rng(), die in simple_die()) {
            let roll = die.clone().roll(&mut rng);
            let value = roll.face.value().unwrap();
            assert!(1 <= value && value <= die.faces);
            assert_eq!(roll.face.to_string(), value.to_string())
        }

        #[test]
        fn fate_die_has_value_in_expected_range(mut rng in rng()) {
            let die = FateDie::new();
            let roll = die.clone().roll(&mut rng);
            let value = roll.face.value().unwrap();
            let face_name = roll.face.to_string();
            match value {
                -1 => assert_eq!(face_name, "-"),
                0 => assert_eq!(face_name, "0"),
                1 => assert_eq!(face_name, "+"),
                _ => panic!("unexpected dF roll: {:?}", roll),
            }
        }
    }
}
