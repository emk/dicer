//! Abstract syntax trees

use std::{fmt, io, rc::Rc};

use rand::RngCore;

use crate::{
    annotations::Annotations,
    dice::{Die, Roll, RollDie},
    errors::{MathError, ProgramError},
    pretty::{PrettyFormat, WriteColor},
    spans::Span,
    values::{Number, Values},
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
    pub fn apply(self, n1: Number, n2: Number) -> Result<Number, MathError> {
        match self {
            Binop::Add => n1
                .checked_add(n2)
                .ok_or(MathError::Overflow { op: self, n1, n2 }),
            Binop::Sub => n1
                .checked_sub(n2)
                .ok_or(MathError::Overflow { op: self, n1, n2 }),
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
#[derive(Debug, PartialEq)]
pub enum Expr<D: fmt::Debug + Eq + 'static> {
    /// An expression involving dice. The contents vary depending on whether the
    /// dice have been rolled.
    Dice(Span, Rc<D>),
    /// A simple constant numeric value.
    Constant(Span, Number),
    /// Annotations used to label values.
    Annotations(Span, Rc<Self>, Annotations),
    /// A binary operator.
    Binop(Span, Binop, Rc<Self>, Rc<Self>),
}

impl<D: fmt::Debug + Eq + 'static> Expr<D> {
    /// The source code span for this expression.
    pub fn span(&self) -> &Span {
        match self {
            Expr::Dice(span, _) => span,
            Expr::Constant(span, _) => span,
            Expr::Annotations(span, _, _) => span,
            Expr::Binop(span, _, _, _) => span,
        }
    }

    /// The span containing this expression and `other`.
    ///
    /// The two spans must be in same file. This is intended for use by the
    /// parser.
    pub(crate) fn combined_span_for_parser(&self, other: &Expr<D>) -> Span {
        let (s1, s2) = (self.span(), other.span());
        debug_assert_eq!(s1.file_id, s2.file_id);
        debug_assert!(s1.range.end <= s2.range.start);
        Span {
            file_id: s1.file_id,
            range: s1.range.start..s2.range.end,
        }
    }
}

impl<D: fmt::Debug + Eq + 'static> Expr<D> {
    /// Is this a binop? Used for inserting parens when printing.
    fn is_binop(&self) -> bool {
        matches!(self, Expr::Binop(_, _, _, _))
    }
}

impl RollAll for Expr<DiceExpr> {
    type Output = Expr<RollsExpr>;

    fn roll_all(self: Rc<Self>, rng: &mut dyn RngCore) -> Self::Output {
        match &*self {
            Expr::Dice(span, dice_expr) => {
                Expr::Dice(span.to_owned(), Rc::new(dice_expr.to_owned().roll_all(rng)))
            }
            Expr::Constant(span, n) => Expr::Constant(span.to_owned(), *n),
            Expr::Annotations(span, e, annotations) => {
                let r = e.to_owned().roll_all(rng);
                Expr::Annotations(span.clone(), Rc::new(r), annotations.clone())
            }
            Expr::Binop(span, op, e1, e2) => {
                let r1 = e1.to_owned().roll_all(rng);
                let r2 = e2.to_owned().roll_all(rng);
                Expr::Binop(span.to_owned(), *op, Rc::new(r1), Rc::new(r2))
            }
        }
    }
}

impl Evaluate for Expr<RollsExpr> {
    type Output = Values;

    fn evaluate(&self) -> Result<Self::Output, ProgramError> {
        match self {
            Expr::Dice(_, rolls) => Ok(Values::from(rolls.evaluate()?)),
            Expr::Constant(_, n) => Ok(Values::from(*n)),
            // TODO: Actually tag values.
            Expr::Annotations(_, e, annotations) => Ok(e.evaluate()?.annotate(annotations)),
            Expr::Binop(span, op, e1, e2) => {
                let v1 = e1.evaluate()?;
                let v2 = e2.evaluate()?;
                v1.pointwise(&v2, |n1, n2| op.apply(n1, n2))
                    .map_err(|source| ProgramError::Math {
                        span: span.to_owned(),
                        source,
                    })
            }
        }
    }
}

impl<D: fmt::Debug + Eq + PrettyFormat + 'static> PrettyFormat for Expr<D> {
    fn pretty_format(&self, writer: &mut dyn WriteColor) -> Result<(), io::Error> {
        match self {
            Expr::Dice(_, d) => d.pretty_format(writer),
            Expr::Constant(_, n) => write!(writer, "{n}"),
            Expr::Annotations(_, e, annotations) if e.is_binop() => {
                write!(writer, "(")?;
                e.pretty_format(writer)?;
                write!(writer, ") {}", annotations)
            }
            Expr::Annotations(_, e, annotations) => {
                e.pretty_format(writer)?;
                write!(writer, " {}", annotations)
            }
            // Handle parentheses insertion. This code is expected to evolve
            // rapidly if we add more operators.
            Expr::Binop(_, op, e1, e2) if e2.is_binop() => {
                e1.pretty_format(writer)?;
                write!(writer, " {} (", op)?;
                e2.pretty_format(writer)?;
                write!(writer, ")")
            }
            Expr::Binop(_, op, e1, e2) => {
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
        /// The source location of this [`DiceExpr`].
        span: Span,
        /// The number of dice to roll.
        count: u64,
        /// The type of dice to roll.
        die: Rc<Die>,
    },
}

impl DiceExpr {
    /// The source location of this [`DiceExpr`].
    pub fn span(&self) -> &Span {
        match self {
            DiceExpr::Dice { span, .. } => span,
        }
    }
}

impl RollAll for DiceExpr {
    type Output = RollsExpr;

    fn roll_all(self: Rc<Self>, rng: &mut dyn RngCore) -> Self::Output {
        match &*self {
            DiceExpr::Dice { count, die, .. } => {
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
            DiceExpr::Dice { count, die, .. } => {
                write!(writer, "{count}{die}")
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
    type Output = Number;

    fn evaluate(&self) -> Result<Self::Output, ProgramError> {
        match self {
            RollsExpr::Rolls { expr, rolls, .. } => {
                let mut sum: Number = 0;
                for roll in rolls {
                    let value = roll.face.value();
                    sum = sum.checked_add(value).ok_or(ProgramError::Math {
                        span: expr.span().to_owned(),
                        source: MathError::Overflow {
                            op: Binop::Add,
                            n1: sum,
                            n2: value,
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
                (any::<Span>(), -20..=20i16).prop_map(|(s, c)| Self::Constant(s, c)),
                (any::<Span>(), any::<D>()).prop_map(|(s, d)| Self::Dice(s, Rc::new(d))),
            ];
            leaf.prop_recursive(4, 32, 10, |inner| {
                prop_oneof![
                    (any::<Span>(), any::<Binop>(), inner.clone(), inner.clone()).prop_map(
                        |(span, binop, e1, e2)| Self::Binop(span, binop, Rc::new(e1), Rc::new(e2))
                    ),
                    (any::<Span>(), inner, any::<Annotations>()).prop_map(
                        |(span, e, annotations)| Self::Annotations(span, Rc::new(e), annotations)
                    ),
                ]
            })
            .boxed()
        }

        type Strategy = BoxedStrategy<Self>;
    }

    impl Arbitrary for DiceExpr {
        type Parameters = ();

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            (any::<Span>(), (0..6u64), any::<Rc<Die>>())
                .prop_map(|(span, count, die)| DiceExpr::Dice { span, count, die })
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
