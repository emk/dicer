//! Annotations can be used to label values, sort of like units.

use std::{collections::BTreeSet, fmt};

use once_cell::sync::OnceCell;
use regex::Regex;

/// A label on a value, such as "[piercing]" or "[mark]".
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct Annotation(#[cfg_attr(test, proptest(strategy = "\"[_a-z][_a-z0-9]*\""))] String);

impl Annotation {
    /// Create a new annotation. `s` must be a lowercase ASCII C identifier.
    pub fn new(s: &str) -> Self {
        #[allow(clippy::missing_docs_in_private_items)]
        static RE: OnceCell<Regex> = OnceCell::new();
        let re =
            RE.get_or_init(|| Regex::new("^[_a-z][_a-z0-9]*$").expect("invalid regex in source"));
        assert!(re.is_match(s));
        Self(s.to_owned())
    }
}

impl fmt::Display for Annotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// A set of labels on a value, such as "[mark, piercing]".
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct Annotations(
    #[cfg_attr(test, proptest(filter = "|s| !BTreeSet::is_empty(s)"))] BTreeSet<Annotation>,
);

impl Annotations {
    /// Combine two sets of annotations.
    pub fn union(&self, other: &Annotations) -> Annotations {
        Annotations(self.0.union(&other.0).cloned().collect())
    }
}

impl fmt::Display for Annotations {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for (idx, annotation) in self.0.iter().enumerate() {
            if idx != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", annotation)?;
        }
        write!(f, "]")
    }
}

impl FromIterator<Annotation> for Annotations {
    fn from_iter<T: IntoIterator<Item = Annotation>>(iter: T) -> Self {
        Self(BTreeSet::from_iter(iter))
    }
}
