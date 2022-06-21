//! Rollable dice.

use std::{
    borrow::Cow,
    cmp::Ordering,
    fmt::{self, Debug, Display},
    rc::Rc,
};

use rand::{Rng, RngCore};

use crate::{
    errors::ProgramError,
    pretty::{ColorSpec, PrettyFormat, WriteColor},
};

/// A value that can appear on the faces of dice.
pub type Value = i16;

/// A die face. May have either a number, or some other symbol (but still with a
/// numeric value).
#[derive(Debug, Eq, PartialEq)]
pub enum Face {
    /// A simple numeric face.
    Numeric(Value),
    /// A face with some other symbol, but still with a numeric value.
    NamedNumeric(Cow<'static, str>, Value),
}

impl Face {
    /// Get the value of this face.
    pub fn value(&self) -> Value {
        match self {
            Face::Numeric(value) => *value,
            Face::NamedNumeric(_, value) => *value,
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

/// Interface shared by all dice.
pub trait RollDie {
    /// Roll this die once.
    fn roll_die(self: &Rc<Self>, rng: &mut dyn RngCore) -> Rc<Roll>;
}

/// Any type of dice that we support.
#[derive(Debug, Eq, PartialEq)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
#[non_exhaustive]
pub enum Die {
    /// Dice with numbered faces.
    Simple(Rc<SimpleDie>),
    /// Dice with "-", "0" and "+" faces.
    Fate(Rc<FateDie>),
}

impl fmt::Display for Die {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Die::Simple(die) => <SimpleDie as fmt::Display>::fmt(die, f),
            Die::Fate(die) => <FateDie as fmt::Display>::fmt(die, f),
        }
    }
}

impl RollDie for Die {
    fn roll_die(self: &Rc<Self>, rng: &mut dyn RngCore) -> Rc<Roll> {
        match &**self {
            Die::Simple(die) => die.roll_die(rng),
            Die::Fate(die) => die.roll_die(rng),
        }
    }
}

/// A single roll of a [`Die`].
#[derive(Debug, Eq, PartialEq)]
pub struct Roll {
    /// The die that was rolled.
    pub die: Rc<Die>,
    /// The face showing.
    pub face: Face,
    /// Whether we have discard this die from the final total (such as when
    /// rolling with advantage or disadvantage, or rolling "4d6 drop lowest").
    pub discarded: bool,
}

impl Roll {
    /// Create a new [`Roll`]. `face` must appear on `die`.
    pub fn new(die: Rc<Die>, face: Face) -> Roll {
        Roll {
            die,
            face,
            discarded: false,
        }
    }
}

impl PrettyFormat for Roll {
    fn pretty_format(&self, writer: &mut dyn WriteColor) -> Result<(), std::io::Error> {
        if self.discarded {
            writer.set_color(ColorSpec::new().set_dimmed(self.discarded))?;
        }
        write!(writer, "{}", self.face)?;
        writer.reset()
    }
}

/// A standard numeric die.
#[derive(Debug, Eq, PartialEq)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct SimpleDie {
    /// The number of faces on this die.
    #[cfg_attr(test, proptest(strategy = "1..60i16"))]
    faces: Value,
}

impl SimpleDie {
    /// Create a new die with the specified number of faces.
    pub fn new(faces: Value) -> Result<Rc<SimpleDie>, ProgramError> {
        if faces > 0 {
            Ok(Rc::new(SimpleDie { faces }))
        } else {
            Err(ProgramError::InvalidFaceCount { faces })
        }
    }
}

impl Display for SimpleDie {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "d{}", self.faces)
    }
}

impl RollDie for SimpleDie {
    fn roll_die(self: &Rc<Self>, rng: &mut dyn RngCore) -> Rc<Roll> {
        let face = Face::Numeric(rng.gen_range(1..=self.faces));
        Rc::new(Roll::new(Rc::new(Die::Simple(self.clone())), face))
    }
}

/// A specialized "Fate" die, with "-", "0" and "+" faces.
#[derive(Debug, Eq, PartialEq)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct FateDie {}

impl FateDie {
    /// Create a new Fate die.
    pub fn new() -> Rc<FateDie> {
        Rc::new(FateDie {})
    }
}

impl Display for FateDie {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "dF")
    }
}

impl RollDie for FateDie {
    fn roll_die(self: &Rc<Self>, rng: &mut dyn RngCore) -> Rc<Roll> {
        let value = rng.gen_range(-1..=1);
        let label = match value.cmp(&0) {
            Ordering::Less => "-",
            Ordering::Equal => "0",
            Ordering::Greater => "+",
        };
        let face = Face::NamedNumeric(Cow::Borrowed(label), value);
        Rc::new(Roll::new(Rc::new(Die::Fate(self.clone())), face))
    }
}

#[cfg(test)]
pub mod tests {
    use proptest::prelude::*;
    use rand::SeedableRng;
    use rand_chacha::ChaCha8Rng;

    use super::*;

    prop_compose! {
        /// A deterministic random number generator, with a seed chosen by
        /// `proptest`.
        pub fn rng()(seed in any::<u64>()) -> ChaCha8Rng {
            ChaCha8Rng::seed_from_u64(seed)
        }

    }

    proptest! {
        #[test]
        fn dice_and_rolls_have_names(mut rng in rng(), die in any::<Rc<Die>>()) {
            assert!(!die.to_string().is_empty());
            let roll = die.roll_die(&mut rng);
            assert!(!roll.face.to_string().is_empty());
        }

        #[test]
        fn simple_die_always_in_range(mut rng in rng(), die in any::<Rc<SimpleDie>>()) {
            let roll = die.roll_die(&mut rng);
            let value = roll.face.value();
            assert!(1 <= value && value <= die.faces);
            assert_eq!(roll.face.to_string(), value.to_string())
        }

        #[test]
        fn fate_die_has_value_in_expected_range(mut rng in rng()) {
            let die = FateDie::new();
            let roll = die.roll_die(&mut rng);
            let value = roll.face.value();
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
