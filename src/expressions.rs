//! Abstract syntax trees

use std::{fmt, io, rc::Rc};

use rand::RngCore;

use crate::{
    dice::{Die, Roll, RollDie, Value},
    errors::{MathError, ProgramError},
    pretty::{PrettyFormat, WriteColor},
};

/// Interface to a type that contains one or more [`Die`] values that can be
/// rolled.
pub trait RollAll {
    /// The type returned by rolling.
    type Output;

    /// Recursively roll all [`Die`] instances in
    fn roll_all(self: Rc<Self>, rng: &mut dyn RngCore) -> Self::Output;
}

/// Calculate the overall value for a rolled expression.
pub trait Evaluate {
    /// The output produced by this type.
    type Output;

    /// Evaluate and return output or error.
    fn evaluate(&self) -> Result<Self::Output, ProgramError>;
}

/// Binary operations.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub enum Binop {
    /// Addition.
    Add,
    /// Subtraction.
    Sub,
}

impl Binop {
    /// Apply this binary operator to two values.
    pub fn apply(self, v1: Value, v2: Value) -> Result<Value, MathError> {
        match self {
            Binop::Add => v1
                .checked_add(v2)
                .ok_or(MathError::Overflow { op: self, v1, v2 }),
            Binop::Sub => v1
                .checked_sub(v2)
                .ok_or(MathError::Overflow { op: self, v1, v2 }),
        }
    }
}

impl fmt::Display for Binop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Binop::Add => write!(f, "+"),
            Binop::Sub => write!(f, "-"),
        }
    }
}

/// Expressions, including constants and math.
///
/// These typically appear in two forms:
///
/// - [`Expr<DiceExpr>`]: Parsed source code.
/// - [`Expr<RollsExpr>`]: Paresed source code with die rolls attached.
#[derive(Debug, Eq, PartialEq)]
pub enum Expr<D: fmt::Debug + Eq + 'static> {
    /// An expression involving dice. The contents vary depending on whether the
    /// dice have been rolled.
    Dice(Rc<D>),
    /// A simple constant numeric value.
    Constant(Value),
    /// A binary operator.
    Binop(Binop, Rc<Self>, Rc<Self>),
}

impl<D: fmt::Debug + Eq + 'static> Expr<D> {
    /// Is this a binop? Used for inserting parens when printing.
    fn is_binop(&self) -> bool {
        matches!(self, Expr::Binop(_, _, _))
    }
}

impl RollAll for Expr<DiceExpr> {
    type Output = Expr<RollsExpr>;

    fn roll_all(self: Rc<Self>, rng: &mut dyn RngCore) -> Self::Output {
        match &*self {
            Expr::Dice(dice_expr) => Expr::Dice(Rc::new(dice_expr.to_owned().roll_all(rng))),
            Expr::Constant(n) => Expr::Constant(*n),
            Expr::Binop(op, e1, e2) => {
                let r1 = e1.to_owned().roll_all(rng);
                let r2 = e2.to_owned().roll_all(rng);
                Expr::Binop(*op, Rc::new(r1), Rc::new(r2))
            }
        }
    }
}

impl Evaluate for Expr<RollsExpr> {
    type Output = Value;

    fn evaluate(&self) -> Result<Self::Output, ProgramError> {
        match self {
            Expr::Dice(rolls) => rolls.evaluate(),
            Expr::Constant(n) => Ok(*n),
            Expr::Binop(op, e1, e2) => {
                let v1 = e1.evaluate()?;
                let v2 = e2.evaluate()?;
                op.apply(v1, v2)
                    .map_err(|source| ProgramError::Math { source })
            }
        }
    }
}

impl<D: fmt::Debug + Eq + PrettyFormat + 'static> PrettyFormat for Expr<D> {
    fn pretty_format(&self, writer: &mut dyn WriteColor) -> Result<(), io::Error> {
        match self {
            Expr::Dice(d) => d.pretty_format(writer),
            Expr::Constant(n) => write!(writer, "{n}"),
            // Handle parentheses insertion. This code is expected to evolve
            // rapidly if we add more operators.
            Expr::Binop(op, e1, e2) if e2.is_binop() => {
                e1.pretty_format(writer)?;
                write!(writer, " {} (", op)?;
                e2.pretty_format(writer)?;
                write!(writer, ")")
            }
            Expr::Binop(op, e1, e2) => {
                e1.pretty_format(writer)?;
                write!(writer, " {} ", op)?;
                e2.pretty_format(writer)
            }
        }
    }
}

/// Expressions that include only dice, and operations performed on sets of
/// rolls.
#[derive(Debug, Eq, PartialEq)]
pub enum DiceExpr {
    /// XdY expressions.
    Dice {
        /// The number of dice to roll.
        count: u64,
        /// The type of dice to roll.
        die: Rc<Die>,
    },
}

impl RollAll for DiceExpr {
    type Output = RollsExpr;

    fn roll_all(self: Rc<Self>, rng: &mut dyn RngCore) -> Self::Output {
        match &*self {
            DiceExpr::Dice { count, die } => {
                let mut rolls = vec![];
                for _ in 0..*count {
                    rolls.push(die.roll_die(rng));
                }
                RollsExpr::Rolls { expr: self, rolls }
            }
        }
    }
}

impl PrettyFormat for DiceExpr {
    fn pretty_format(&self, writer: &mut dyn WriteColor) -> Result<(), io::Error> {
        match self {
            DiceExpr::Dice { count, die } => {
                if *count != 1 {
                    write!(writer, "{count}")?;
                }
                write!(writer, "{die}")
            }
        }
    }
}

/// A [`DiceExpr`] plus the [`Roll`]s it produced.
#[derive(Debug, Eq, PartialEq)]
pub enum RollsExpr {
    /// A number of rolls of similar dice.
    Rolls {
        /// The [`DiceExpr`] that generated rolls.
        expr: Rc<DiceExpr>,
        /// The resulting [`Roll`]s.
        rolls: Vec<Rc<Roll>>,
    },
}

impl Evaluate for RollsExpr {
    type Output = Value;

    fn evaluate(&self) -> Result<Self::Output, ProgramError> {
        match self {
            RollsExpr::Rolls { rolls, .. } => {
                let mut sum: Value = 0;
                for roll in rolls {
                    let value = roll.face.value();
                    sum = sum.checked_add(value).ok_or(ProgramError::Math {
                        source: MathError::Overflow {
                            op: Binop::Add,
                            v1: sum,
                            v2: value,
                        },
                    })?;
                }
                Ok(sum)
            }
        }
    }
}

impl PrettyFormat for RollsExpr {
    fn pretty_format(&self, writer: &mut dyn WriteColor) -> Result<(), io::Error> {
        match self {
            RollsExpr::Rolls { expr, rolls } => {
                expr.pretty_format(writer)?;
                write!(writer, " (")?;
                for (idx, roll) in rolls.iter().enumerate() {
                    if idx != 0 {
                        write!(writer, " ")?;
                    }
                    roll.pretty_format(writer)?;
                }
                write!(writer, ")")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;
    use crate::dice::tests::rng;

    impl<D: fmt::Debug + Eq + Arbitrary> Arbitrary for Expr<D> {
        type Parameters = ();

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            let leaf = prop_oneof![
                (-20..=20i16).prop_map(Self::Constant),
                any::<D>().prop_map(|d| Self::Dice(Rc::new(d))),
            ];
            leaf.prop_recursive(4, 32, 10, |inner| {
                prop_oneof![(any::<Binop>(), inner.clone(), inner)
                    .prop_map(|(binop, e1, e2)| Self::Binop(binop, Rc::new(e1), Rc::new(e2))),]
            })
            .boxed()
        }

        type Strategy = BoxedStrategy<Self>;
    }

    impl Arbitrary for DiceExpr {
        type Parameters = ();

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            ((0..6u64), any::<Rc<Die>>())
                .prop_map(|(count, die)| DiceExpr::Dice { count, die })
                .boxed()
        }

        type Strategy = BoxedStrategy<Self>;
    }

    impl Arbitrary for RollsExpr {
        type Parameters = ();

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            (any::<Rc<DiceExpr>>(), rng())
                .prop_map(|(expr, mut rng)| expr.roll_all(&mut rng))
                .boxed()
        }

        type Strategy = BoxedStrategy<Self>;
    }

    proptest! {
        #[test]
        fn expressions_can_be_evaluated(mut rng in rng(), expr in any::<Rc<Expr<DiceExpr>>>()) {
            let output = expr.roll_all(&mut rng);
            let _ = output.evaluate();
        }
    }
}
