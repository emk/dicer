//! Values returned by calculations.

use std::{collections::BTreeMap, fmt};

use crate::{annotations::Annotations, errors::MathError};

/// A number than can be rolled on a die, or created by doing math
/// on constants and die faces.
pub type Number = i16;

/// A set of values, including both annotated and unnannotated values.
pub struct Values {
    /// Annotated values. The `None` annotation represents a bare value with no
    /// annotations.
    values: BTreeMap<Option<Annotations>, Number>,
}

impl Values {
    /// Add `annotations` to each number in `Values`, merging annotations as
    /// necessary.
    pub fn annotate(&self, annotations: &Annotations) -> Values {
        let mut values = BTreeMap::new();
        for (existing_annotations, value) in &self.values {
            let new_annotations = match existing_annotations {
                None => annotations.clone(),
                Some(existing_annotations) => existing_annotations.union(annotations),
            };
            values.insert(Some(new_annotations), *value);
        }
        Values { values }
    }

    /// Apply `f` to `self` and `other` in a pointwise fashion. When a given annotation exists only in one set of values
    pub fn pointwise<F>(&self, other: &Values, f: F) -> Result<Values, MathError>
    where
        F: Fn(Number, Number) -> Result<Number, MathError>,
    {
        let mut i1 = self.values.iter().peekable();
        let mut i2 = other.values.iter().peekable();
        let mut result = BTreeMap::new();
        loop {
            match (i1.peek(), i2.peek()) {
                // We've reached the end of both iterators. We're done.
                (None, None) => break,
                // We've reached the end of `i2`.
                (Some(&(k1, v1)), None) => {
                    result.insert(k1.to_owned(), f(*v1, 0)?);
                    i1.next();
                }
                // `k1 < k2`.
                (Some(&(k1, v1)), Some(&(k2, _))) if k1 < k2 => {
                    result.insert(k1.to_owned(), f(*v1, 0)?);
                    i1.next();
                }
                // We've reached the end of `i1`.
                (None, Some(&(k2, v2))) => {
                    result.insert(k2.to_owned(), f(0, *v2)?);
                    i2.next();
                }
                // `k2 < k1`.
                (Some(&(k1, _)), Some(&(k2, v2))) if k2 < k1 => {
                    result.insert(k2.to_owned(), f(0, *v2)?);
                    i2.next();
                }
                // `k2 == k1`.
                (Some(&(k1, v1)), Some(&(k2, v2))) => {
                    debug_assert!(k1 == k2);
                    result.insert(k1.to_owned(), f(*v1, *v2)?);
                    i1.next();
                    i2.next();
                }
            }
        }
        Ok(Values { values: result })
    }
}

impl From<Number> for Values {
    fn from(n: Number) -> Self {
        let mut values = BTreeMap::new();
        values.insert(None, n);
        Self { values }
    }
}

impl fmt::Display for Values {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (idx, (annotations, n)) in self.values.iter().enumerate() {
            if idx > 0 {
                write!(f, " + ")?;
            }
            write!(f, "{}", n)?;
            if let Some(annotations) = annotations {
                write!(f, " {}", annotations)?;
            }
        }
        Ok(())
    }
}
