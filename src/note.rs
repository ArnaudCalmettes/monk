//! # Note manipulation module
//!
//! In `monk`, a music note is the association of a base name (C, D, ..., B)
//! and a number of alterations (sharp, flat, double-sharp).
//!
//! For convenience, all notes up to one sharp or one flat are defined as clonable constants:
//!
//! * `NOTE_C`: C
//! * `NOTE_F_SHARP`: F♯
//! * `NOTE_B_FLAT`: B♭
//! * ...
//!
//! ```
//! use monk::note::*;
//!
//! let c_natural: Note = NOTE_C;
//! let f_sharp: Note = NOTE_F_SHARP;
//! let b_flat: Note = NOTE_B_FLAT;
//!
//! let g_double_sharp = NOTE_G_SHARP.augment(); // Add one more sharp
//!
//! assert_eq!(format!("{}", c_natural), String::from("C"));
//! assert_eq!(format!("{}", f_sharp), String::from("F#"));
//! assert_eq!(format!("{}", b_flat), String::from("Bb"));
//! assert_eq!(format!("{}", g_double_sharp), String::from("G##"));
//! ```
//!
//! ## Chromatic representation and enharmony
//!
//! It is often easier to think of notes as degrees of the chromatic scale. For this reason,
//! we can convert `Note`s from and to integer values (`i8`) representing chromatic degrees.
//!
//! In this representation:
//!
//! * `0 (mod 12)` is C,
//! * `1 (mod 12)` is C♯,
//! * ...
//! * `10 (mod 12)` is B♭,
//! * `11 (mod 12)` is B
//!
//! ```
//! # use monk::note::*;
//! assert_eq!(NOTE_C.degree(), 0);
//! assert_eq!(NOTE_C_SHARP.degree(), 1);
//! assert_eq!(NOTE_D.degree(), 2);
//! assert_eq!(NOTE_B.degree(), 11);
//!
//! assert_eq!(Note::from_degree(0, true), NOTE_C);
//! assert_eq!(Note::from_degree(1, true), NOTE_C_SHARP);
//! assert_eq!(Note::from_degree(2, true), NOTE_D);
//!
//! // Represent degree 2 as a diminished D instead of an augmented C
//! assert_eq!(Note::from_degree(1, false), NOTE_D_FLAT);
//!
//! // Any i8 value is accepted.
//! assert_eq!(Note::from_degree(-34, true), NOTE_D);
//! ```
//!
//! The second parameter to `Note::from_degree` is used to choose whether chromatic notes should be
//! considered as augmented (`true`) or diminished (`false`) neighbour notes.
//!
//! In order to stay theoretically sound, `monk` distinguishes note equality from enharmony.
//! For instance, C♯ and D♭ are distinct notes, but they are enharmonic because they correspond to the same
//! degree on the chromatic scale:
//!
//! ```
//! # use monk::note::*;
//! assert!(NOTE_C_SHARP != NOTE_D_FLAT);              // C# and Db are not the same note
//! assert!(NOTE_C_SHARP.is_enharmonic(NOTE_D_FLAT));  // But they are enharmonic
//! ```
use std::fmt;

/// Note "roots" correspond to the notes of the C major scale (the white keys of a piano keyboard).

#[derive(PartialEq, Debug, Clone)]
pub enum NoteRoot {
    C,
    D,
    E,
    F,
    G,
    A,
    B,
}

impl NoteRoot {
    /// Get the chromatic degree associated to this NoteRoot value.
    ///
    /// ```
    /// # use monk::note::NoteRoot;
    /// assert_eq!(NoteRoot::C.degree(), 0);
    /// assert_eq!(NoteRoot::B.degree(), 11);
    /// ```
    pub fn degree(&self) -> i8 {
        match self {
            NoteRoot::C => 0,
            NoteRoot::D => 2,
            NoteRoot::E => 4,
            NoteRoot::F => 5,
            NoteRoot::G => 7,
            NoteRoot::A => 9,
            NoteRoot::B => 11,
        }
    }

    fn str(&self) -> &'static str {
        match self {
            NoteRoot::C => "C",
            NoteRoot::D => "D",
            NoteRoot::E => "E",
            NoteRoot::F => "F",
            NoteRoot::G => "G",
            NoteRoot::A => "A",
            NoteRoot::B => "B",
        }
    }
}

impl fmt::Display for NoteRoot {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.str())
    }
}

/// Wrap a chromatic degree to get its canonical value in [0; 11].
///
/// ```
/// # use monk::note::wrap_degree;
/// assert_eq!(wrap_degree(3), 3);
/// assert_eq!(wrap_degree(-5), 7);
/// assert_eq!(wrap_degree(26), 2);
/// ```
pub fn wrap_degree(n: i8) -> i8 {
    let res = n % 12;
    if res < 0 {
        res + 12
    } else {
        res
    }
}

/// Common representation for a music note.
///

#[derive(Debug, Clone, PartialEq)]
pub struct Note {
    base: NoteRoot,
    alt: i8,
}

const NATURAL: i8 = 0;
const SHARP: i8 = 1;
const FLAT: i8 = -1;

pub const NOTE_C: Note = Note {
    base: NoteRoot::C,
    alt: NATURAL,
};

pub const NOTE_C_SHARP: Note = Note {
    base: NoteRoot::C,
    alt: SHARP,
};

pub const NOTE_C_FLAT: Note = Note {
    base: NoteRoot::C,
    alt: FLAT,
};

pub const NOTE_D: Note = Note {
    base: NoteRoot::D,
    alt: NATURAL,
};

pub const NOTE_D_SHARP: Note = Note {
    base: NoteRoot::D,
    alt: SHARP,
};

pub const NOTE_D_FLAT: Note = Note {
    base: NoteRoot::D,
    alt: FLAT,
};

pub const NOTE_E: Note = Note {
    base: NoteRoot::E,
    alt: NATURAL,
};

pub const NOTE_E_SHARP: Note = Note {
    base: NoteRoot::E,
    alt: SHARP,
};

pub const NOTE_E_FLAT: Note = Note {
    base: NoteRoot::E,
    alt: FLAT,
};

pub const NOTE_F: Note = Note {
    base: NoteRoot::F,
    alt: NATURAL,
};

pub const NOTE_F_SHARP: Note = Note {
    base: NoteRoot::F,
    alt: SHARP,
};

pub const NOTE_F_FLAT: Note = Note {
    base: NoteRoot::F,
    alt: FLAT,
};

pub const NOTE_G: Note = Note {
    base: NoteRoot::G,
    alt: NATURAL,
};

pub const NOTE_G_SHARP: Note = Note {
    base: NoteRoot::G,
    alt: SHARP,
};

pub const NOTE_G_FLAT: Note = Note {
    base: NoteRoot::G,
    alt: FLAT,
};

pub const NOTE_A: Note = Note {
    base: NoteRoot::A,
    alt: NATURAL,
};

pub const NOTE_A_SHARP: Note = Note {
    base: NoteRoot::A,
    alt: SHARP,
};

pub const NOTE_A_FLAT: Note = Note {
    base: NoteRoot::A,
    alt: FLAT,
};

pub const NOTE_B: Note = Note {
    base: NoteRoot::B,
    alt: NATURAL,
};

pub const NOTE_B_SHARP: Note = Note {
    base: NoteRoot::B,
    alt: SHARP,
};

pub const NOTE_B_FLAT: Note = Note {
    base: NoteRoot::B,
    alt: FLAT,
};

const NOTES_SHARP: [Note; 12] = [
    NOTE_C,
    NOTE_C_SHARP,
    NOTE_D,
    NOTE_D_SHARP,
    NOTE_E,
    NOTE_F,
    NOTE_F_SHARP,
    NOTE_G,
    NOTE_G_SHARP,
    NOTE_A,
    NOTE_A_SHARP,
    NOTE_B,
];

const NOTES_FLAT: [Note; 12] = [
    NOTE_C,
    NOTE_D_FLAT,
    NOTE_D,
    NOTE_E_FLAT,
    NOTE_E,
    NOTE_F,
    NOTE_G_FLAT,
    NOTE_G,
    NOTE_A_FLAT,
    NOTE_A,
    NOTE_B_FLAT,
    NOTE_B,
];

impl Note {
    /// ```
    /// # use monk::note::*;
    /// assert_eq!(Note::new(NoteRoot::F, -1), NOTE_F_FLAT);
    /// assert_eq!(Note::new(NoteRoot::A, 0), NOTE_A);
    /// assert_eq!(Note::new(NoteRoot::G, 1), NOTE_G_SHARP);
    /// ```
    pub fn new(base: NoteRoot, alt: i8) -> Self {
        Note { base, alt }
    }

    /// Get the base of the note.
    ///
    /// ```
    /// # use monk::note::*;
    /// assert_eq!(NOTE_G_SHARP.base(), NoteRoot::G);
    /// ```
    pub fn base(&self) -> NoteRoot {
        self.base.clone()
    }

    /// Get the alterations of the note.
    ///
    /// The return value is a signed integer indicating the number of flats (`< 0`)
    /// or sharps (`> 0`) beared by the note.
    ///
    /// ```
    /// # use monk::note::*;
    /// assert_eq!(NOTE_C_SHARP.alt(), 1);
    /// assert_eq!(NOTE_F.alt(), 0);
    /// assert_eq!(NOTE_A_FLAT.alt(), -1);
    /// ```
    pub fn alt(&self) -> i8 {
        self.alt
    }

    /// Get the chromatic degree of a note.
    ///
    /// The degree is the sum of the note root's degree and its alterations.
    ///
    /// ```
    /// # use monk::note::*;
    /// assert_eq!(NOTE_B_FLAT.degree(), NoteRoot::B.degree() - 1);
    /// assert_eq!(NOTE_C_SHARP.degree(), NoteRoot::C.degree() + 1);
    /// ```
    pub fn degree(&self) -> i8 {
        self.base.degree() + self.alt
    }

    /// Construct a new note given its chromatic degree.
    ///
    /// `degree` may be any signed byte. If `sharp` is `true`, consider any half-step as an
    /// augmented neighbor. Otherwise, consider half-steps as a diminished neighbor.
    ///
    /// ```
    /// # use monk::note::*;
    /// assert_eq!(Note::from_degree(3, true), NOTE_D_SHARP);
    /// assert_eq!(Note::from_degree(3, false), NOTE_E_FLAT);
    /// assert_eq!(Note::from_degree(48, true), NOTE_C);
    /// assert_eq!(Note::from_degree(-18, true), NOTE_F_SHARP);
    /// ```
    pub fn from_degree(degree: i8, sharp: bool) -> Self {
        let degree = wrap_degree(degree) as usize;
        if sharp {
            NOTES_SHARP[degree].clone()
        } else {
            NOTES_FLAT[degree].clone()
        }
    }

    /// Simplify a note and return its enharmonic equivalent.
    ///
    /// ```
    /// # use monk::note::*;
    /// let note = Note::new(NoteRoot::B, 7);
    /// assert_eq!(format!("{}", note), String::from("B#######"));
    /// assert_eq!(note.reduce(), NOTE_F_SHARP);
    /// ```
    pub fn reduce(&self) -> Self {
        Self::from_degree(self.degree(), self.alt >= 0)
    }

    /// Add one sharp to the note.
    ///
    /// This doesn't affect the note's base.
    ///
    /// ```
    /// # use monk::note::*;
    /// assert_eq!(NOTE_C.augment(), NOTE_C_SHARP);
    /// assert_eq!(NOTE_B.augment(), NOTE_B_SHARP);
    /// ```
    pub fn augment(&self) -> Self {
        return Self {
            base: self.base.clone(),
            alt: self.alt + 1,
        };
    }

    /// Add one flat to the note.
    ///
    /// This doesn't affect the note's base.
    ///
    /// ```
    /// # use monk::note::*;
    /// assert_eq!(NOTE_C.diminish(), NOTE_C_FLAT);
    /// assert_eq!(NOTE_G.diminish(), NOTE_G_FLAT);
    /// ```
    pub fn diminish(&self) -> Self {
        return Self {
            base: self.base.clone(),
            alt: self.alt - 1,
        };
    }

    /// Add any number of alterations to the note.
    ///
    /// ```
    /// # use monk::note::*;
    /// assert_eq!(NOTE_D_FLAT.alter(2), NOTE_D_SHARP);
    /// assert_eq!(NOTE_A_SHARP.alter(-1), NOTE_A);
    /// ```
    pub fn alter(&self, alt: i8) -> Self {
        return Self {
            base: self.base.clone(),
            alt: self.alt + alt,
        };
    }

    /// Add a sharp to the note inplace.
    ///
    /// ```
    /// # use monk::note::*;
    /// let mut note = NOTE_C;
    /// note.mut_augment();
    /// assert_eq!(note, NOTE_C_SHARP);
    /// ```
    pub fn mut_augment(&mut self) {
        self.alt += 1
    }

    /// Add a flat to the note inplace.
    ///
    /// ```
    /// # use monk::note::*;
    /// let mut note = NOTE_F;
    /// note.mut_diminish();
    /// assert_eq!(note, NOTE_F_FLAT);
    /// ```
    pub fn mut_diminish(&mut self) {
        self.alt -= 1
    }

    /// Add an arbitrary number of alterations to the note inplace.
    ///
    /// ```
    /// # use monk::note::*;
    /// let mut note = NOTE_G_SHARP;
    /// note.mut_alter(-2);
    /// assert_eq!(note, NOTE_G_FLAT);
    /// ```
    pub fn mut_alter(&mut self, alt: i8) {
        self.alt += alt
    }

    /// Returns true if the note has at least one sharp.
    pub fn is_sharp(&self) -> bool {
        self.alt > 0
    }

    /// Returns true if the note has at least one flat.
    pub fn is_flat(&self) -> bool {
        self.alt < 0
    }

    /// Test whether both notes correspond to the same degree on the chromatic scale.
    ///
    /// ```
    /// # use monk::note::*;
    /// assert!(NOTE_C_FLAT.is_enharmonic(NOTE_B));
    /// assert!(!NOTE_F.is_enharmonic(NOTE_G));
    /// ```
    pub fn is_enharmonic(&self, other: Note) -> bool {
        wrap_degree(self.degree() - other.degree()) == 0
    }

    fn fmt_alt(&self) -> String {
        if self.alt < 0 {
            "b".repeat(-self.alt as usize)
        } else {
            "#".repeat(self.alt as usize)
        }
    }
}

impl fmt::Display for Note {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.base.str(), self.fmt_alt())
    }
}

#[cfg(test)]
mod test {
    use crate::note::*;

    #[test]
    fn chromatic_run_ascending() {
        let mut note = NOTE_C;
        note = note.augment();
        assert_eq!(note, NOTE_C_SHARP);
        note = note.augment().reduce();
        assert_eq!(note, NOTE_D);
        note = note.augment();
        assert_eq!(note, NOTE_D_SHARP);
        note = note.augment().reduce();
        assert_eq!(note, NOTE_E);
        note = note.augment().reduce();
        assert_eq!(note, NOTE_F);
        note = note.augment();
        assert_eq!(note, NOTE_F_SHARP);
        note = note.augment().reduce();
        assert_eq!(note, NOTE_G);
        note = note.augment();
        assert_eq!(note, NOTE_G_SHARP);
        note = note.augment().reduce();
        assert_eq!(note, NOTE_A);
        note = note.augment();
        assert_eq!(note, NOTE_A_SHARP);
        note = note.augment().reduce();
        assert_eq!(note, NOTE_B);
        note = note.augment().reduce();
        assert_eq!(note, NOTE_C);
    }

    #[test]
    fn chromatic_run_descending() {
        let mut note = NOTE_C;
        note = note.diminish().reduce();
        assert_eq!(note, NOTE_B);
        note = note.diminish();
        assert_eq!(note, NOTE_B_FLAT);
        note = note.diminish().reduce();
        assert_eq!(note, NOTE_A);
        note = note.diminish();
        assert_eq!(note, NOTE_A_FLAT);
        note = note.diminish().reduce();
        assert_eq!(note, NOTE_G);
        note = note.diminish();
        assert_eq!(note, NOTE_G_FLAT);
        note = note.diminish().reduce();
        assert_eq!(note, NOTE_F);
        note = note.diminish().reduce();
        assert_eq!(note, NOTE_E);
        note = note.diminish();
        assert_eq!(note, NOTE_E_FLAT);
        note = note.diminish().reduce();
        assert_eq!(note, NOTE_D);
        note = note.diminish();
        assert_eq!(note, NOTE_D_FLAT);
        note = note.diminish().reduce();
        assert_eq!(note, NOTE_C);
    }
}
