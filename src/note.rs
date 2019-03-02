//! # Note manipulation module
//!
//! In `monk`, a music note is the association of a base name (C, D, ..., B)
//! and a number of alterations (sharp, flat, double-sharp).
//!
//! For convenience, all notes up to one sharp or one flat are defined as clonable constants:
//!
//! * `C`: C
//! * `F_SHARP`: F♯
//! * `B_FLAT`: B♭
//! * ...
//!
//! ```
//! use monk::note::*;
//!
//! let c_natural: Note = C;
//! let f_sharp: Note = F_SHARP;
//! let b_flat: Note = B_FLAT;
//!
//! let g_double_sharp = G_SHARP.augment(); // Add one more sharp
//!
//! assert_eq!(format!("{}", c_natural), String::from("C"));
//! assert_eq!(format!("{}", f_sharp), String::from("F#"));
//! assert_eq!(format!("{}", b_flat), String::from("Bb"));
//! assert_eq!(format!("{}", g_double_sharp), String::from("G##"));
//! ```
//!
//! ## Chromatic representation and enharmony
//!
//! It is often easier to think of notes as degrees of the chromatic scale ("chromas"). For this reason,
//! we can convert `Note`s from and to integer values (`Chroma`, i.e. `i8`) representing chromatic degrees.
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
//! assert_eq!(C.chroma(), 0);
//! assert_eq!(C_SHARP.chroma(), 1);
//! assert_eq!(D.chroma(), 2);
//! assert_eq!(B.chroma(), 11);
//!
//! assert_eq!(Note::from_chroma(0, true), C);
//! assert_eq!(Note::from_chroma(1, true), C_SHARP);
//! assert_eq!(Note::from_chroma(2, true), D);
//!
//! // Represent chroma 2 as a diminished D instead of an augmented C
//! assert_eq!(Note::from_chroma(1, false), D_FLAT);
//!
//! // Any i8 value is accepted.
//! assert_eq!(Note::from_chroma(-34, true), D);
//! ```
//!
//! The second parameter to `Note::from_chroma` is used to choose whether chromatic notes should be
//! considered as augmented (`true`) or diminished (`false`) neighbour notes.
//!
//! In order to stay theoretically sound, `monk` distinguishes note equality from enharmony.
//! For instance, C♯ and D♭ are distinct notes, but they are enharmonic because they correspond to the same
//! degree on the chromatic scale:
//!
//! ```
//! # use monk::note::*;
//! assert!(C_SHARP != D_FLAT);              // C# and Db are not the same note
//! assert!(C_SHARP.is_enharmonic(D_FLAT));  // But they are enharmonic
//! ```
use std::fmt;

pub type Chroma = i8;

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
    /// assert_eq!(NoteRoot::C.chroma(), 0);
    /// assert_eq!(NoteRoot::B.chroma(), 11);
    /// ```
    pub fn chroma(&self) -> Chroma {
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
/// # use monk::note::wrap_chroma;
/// assert_eq!(wrap_chroma(3), 3);
/// assert_eq!(wrap_chroma(-5), 7);
/// assert_eq!(wrap_chroma(26), 2);
/// ```
pub fn wrap_chroma(n: Chroma) -> Chroma {
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

pub const C: Note = Note {
    base: NoteRoot::C,
    alt: NATURAL,
};

pub const C_SHARP: Note = Note {
    base: NoteRoot::C,
    alt: SHARP,
};

pub const C_FLAT: Note = Note {
    base: NoteRoot::C,
    alt: FLAT,
};

pub const D: Note = Note {
    base: NoteRoot::D,
    alt: NATURAL,
};

pub const D_SHARP: Note = Note {
    base: NoteRoot::D,
    alt: SHARP,
};

pub const D_FLAT: Note = Note {
    base: NoteRoot::D,
    alt: FLAT,
};

pub const E: Note = Note {
    base: NoteRoot::E,
    alt: NATURAL,
};

pub const E_SHARP: Note = Note {
    base: NoteRoot::E,
    alt: SHARP,
};

pub const E_FLAT: Note = Note {
    base: NoteRoot::E,
    alt: FLAT,
};

pub const F: Note = Note {
    base: NoteRoot::F,
    alt: NATURAL,
};

pub const F_SHARP: Note = Note {
    base: NoteRoot::F,
    alt: SHARP,
};

pub const F_FLAT: Note = Note {
    base: NoteRoot::F,
    alt: FLAT,
};

pub const G: Note = Note {
    base: NoteRoot::G,
    alt: NATURAL,
};

pub const G_SHARP: Note = Note {
    base: NoteRoot::G,
    alt: SHARP,
};

pub const G_FLAT: Note = Note {
    base: NoteRoot::G,
    alt: FLAT,
};

pub const A: Note = Note {
    base: NoteRoot::A,
    alt: NATURAL,
};

pub const A_SHARP: Note = Note {
    base: NoteRoot::A,
    alt: SHARP,
};

pub const A_FLAT: Note = Note {
    base: NoteRoot::A,
    alt: FLAT,
};

pub const B: Note = Note {
    base: NoteRoot::B,
    alt: NATURAL,
};

pub const B_SHARP: Note = Note {
    base: NoteRoot::B,
    alt: SHARP,
};

pub const B_FLAT: Note = Note {
    base: NoteRoot::B,
    alt: FLAT,
};

const NOTES_SHARP: [Note; 12] = [
    C, C_SHARP, D, D_SHARP, E, F, F_SHARP, G, G_SHARP, A, A_SHARP, B,
];

const NOTES_FLAT: [Note; 12] = [C, D_FLAT, D, E_FLAT, E, F, G_FLAT, G, A_FLAT, A, B_FLAT, B];

impl Note {
    /// ```
    /// # use monk::note::*;
    /// assert_eq!(Note::new(NoteRoot::F, -1), F_FLAT);
    /// assert_eq!(Note::new(NoteRoot::A, 0), A);
    /// assert_eq!(Note::new(NoteRoot::G, 1), G_SHARP);
    /// ```
    pub fn new(base: NoteRoot, alt: i8) -> Self {
        Note { base, alt }
    }

    /// Get the base of the note.
    ///
    /// ```
    /// # use monk::note::*;
    /// assert_eq!(G_SHARP.base(), NoteRoot::G);
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
    /// assert_eq!(C_SHARP.alt(), 1);
    /// assert_eq!(F.alt(), 0);
    /// assert_eq!(A_FLAT.alt(), -1);
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
    /// assert_eq!(B_FLAT.chroma(), NoteRoot::B.chroma() - 1);
    /// assert_eq!(C_SHARP.chroma(), NoteRoot::C.chroma() + 1);
    /// ```
    pub fn chroma(&self) -> Chroma {
        self.base.chroma() + self.alt
    }

    /// Construct a new note given its chromatic degree.
    ///
    /// `chroma` may be any signed byte. If `sharp` is `true`, consider any half-step as an
    /// augmented neighbor. Otherwise, consider half-steps as a diminished neighbor.
    ///
    /// ```
    /// # use monk::note::*;
    /// assert_eq!(Note::from_chroma(3, true), D_SHARP);
    /// assert_eq!(Note::from_chroma(3, false), E_FLAT);
    /// assert_eq!(Note::from_chroma(48, true), C);
    /// assert_eq!(Note::from_chroma(-18, true), F_SHARP);
    /// ```
    pub fn from_chroma(chroma: Chroma, sharp: bool) -> Self {
        let chroma = wrap_chroma(chroma) as usize;
        if sharp {
            NOTES_SHARP[chroma].clone()
        } else {
            NOTES_FLAT[chroma].clone()
        }
    }

    /// Simplify a note and return its enharmonic equivalent.
    ///
    /// ```
    /// # use monk::note::*;
    /// let note = Note::new(NoteRoot::B, 7);
    /// assert_eq!(format!("{}", note), String::from("B#######"));
    /// assert_eq!(note.reduce(), F_SHARP);
    /// ```
    pub fn reduce(&self) -> Self {
        Self::from_chroma(self.chroma(), self.alt >= 0)
    }

    /// Add one sharp to the note.
    ///
    /// This doesn't affect the note's base.
    ///
    /// ```
    /// # use monk::note::*;
    /// assert_eq!(C.augment(), C_SHARP);
    /// assert_eq!(B.augment(), B_SHARP);
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
    /// assert_eq!(C.diminish(), C_FLAT);
    /// assert_eq!(G.diminish(), G_FLAT);
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
    /// assert_eq!(D_FLAT.alter(2), D_SHARP);
    /// assert_eq!(A_SHARP.alter(-1), A);
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
    /// let mut note = C;
    /// note.mut_augment();
    /// assert_eq!(note, C_SHARP);
    /// ```
    pub fn mut_augment(&mut self) {
        self.alt += 1
    }

    /// Add a flat to the note inplace.
    ///
    /// ```
    /// # use monk::note::*;
    /// let mut note = F;
    /// note.mut_diminish();
    /// assert_eq!(note, F_FLAT);
    /// ```
    pub fn mut_diminish(&mut self) {
        self.alt -= 1
    }

    /// Add an arbitrary number of alterations to the note inplace.
    ///
    /// ```
    /// # use monk::note::*;
    /// let mut note = G_SHARP;
    /// note.mut_alter(-2);
    /// assert_eq!(note, G_FLAT);
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

    /// Test whether both notes correspond to the same chroma on the chromatic scale.
    ///
    /// ```
    /// # use monk::note::*;
    /// assert!(C_FLAT.is_enharmonic(B));
    /// assert!(!F.is_enharmonic(G));
    /// ```
    pub fn is_enharmonic(&self, other: Note) -> bool {
        wrap_chroma(self.chroma() - other.chroma()) == 0
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
        let mut note = C;
        note = note.augment();
        assert_eq!(note, C_SHARP);
        note = note.augment().reduce();
        assert_eq!(note, D);
        note = note.augment();
        assert_eq!(note, D_SHARP);
        note = note.augment().reduce();
        assert_eq!(note, E);
        note = note.augment().reduce();
        assert_eq!(note, F);
        note = note.augment();
        assert_eq!(note, F_SHARP);
        note = note.augment().reduce();
        assert_eq!(note, G);
        note = note.augment();
        assert_eq!(note, G_SHARP);
        note = note.augment().reduce();
        assert_eq!(note, A);
        note = note.augment();
        assert_eq!(note, A_SHARP);
        note = note.augment().reduce();
        assert_eq!(note, B);
        note = note.augment().reduce();
        assert_eq!(note, C);
    }

    #[test]
    fn chromatic_run_descending() {
        let mut note = C;
        note = note.diminish().reduce();
        assert_eq!(note, B);
        note = note.diminish();
        assert_eq!(note, B_FLAT);
        note = note.diminish().reduce();
        assert_eq!(note, A);
        note = note.diminish();
        assert_eq!(note, A_FLAT);
        note = note.diminish().reduce();
        assert_eq!(note, G);
        note = note.diminish();
        assert_eq!(note, G_FLAT);
        note = note.diminish().reduce();
        assert_eq!(note, F);
        note = note.diminish().reduce();
        assert_eq!(note, E);
        note = note.diminish();
        assert_eq!(note, E_FLAT);
        note = note.diminish().reduce();
        assert_eq!(note, D);
        note = note.diminish();
        assert_eq!(note, D_FLAT);
        note = note.diminish().reduce();
        assert_eq!(note, C);
    }
}
