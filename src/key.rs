//! # Key definition module
//!
//! Keys are the cornerstone of the tonal system. They provide context, color and
//! meaning to the notes that we play or hear.
//!
//! This module exposes constants for the 30 keys (up to 7 sharps or 7 flats) of
//! the circle of fifths.
//!
//! Constant keys are named in the form `KEY_[NOTE]_[MODE]`, e.g. :
//!
//! * `KEY_C_MAJOR`,
//! * `KEY_F_SHARP_MAJOR`,
//! * `KEY_B_FLAT_MINOR`,
//! * ...
//!
//! ## Diatonicism
//!
//! Each key defines a reference (major or minor) scale:
//!
//! ```
//! use monk::key::*;
//! use monk::note::*;
//!
//! // Print the G major scale on screen
//! for note in KEY_G_MAJOR.notes() {
//!     println!("{}", note)
//! }
//!
//! assert_eq!(
//!     KEY_G_MAJOR.notes(),
//!     vec![NOTE_G, NOTE_A, NOTE_B, NOTE_C, NOTE_D, NOTE_E, NOTE_F_SHARP]
//! );
//! assert_eq!(
//!     KEY_E_FLAT_MINOR.notes(),
//!     vec![
//!         NOTE_E_FLAT,
//!         NOTE_F,
//!         NOTE_G_FLAT,
//!         NOTE_A_FLAT,
//!         NOTE_B_FLAT,
//!         NOTE_C_FLAT,
//!         NOTE_D_FLAT
//!     ]
//! );
//! ```
//!
//! As keys define a context, they can be used more precisely than bare `Note` objects
//! to map between chromatic degrees and actual notes. For instance, the 7th degree of
//! the chromatic scale doesn't correspond to the same note in the keys of G major or
//! E♭ minor.
//!
//! ```
//! use monk::key::*;
//! use monk::note::*;
//! assert_eq!(KEY_G_MAJOR.note_from_chroma(6), NOTE_F_SHARP);
//! assert_eq!(KEY_E_FLAT_MINOR.note_from_chroma(6), NOTE_G_FLAT);
//! ```
//!
//!
//! ## Alterations
//!
//! In practice, we often reason about keys using their number of alterations,
//! because by construction, there is a one-to-one mapping between major keys
//! and their alterations:
//!
//! ```
//! use monk::key::*;
//!
//! // Create keys given their number of alterations
//! let key = Key::from_alterations(4);  // 4 sharps: E major
//! assert_eq!(key, KEY_E_MAJOR);
//!
//! let key = Key::from_alterations(-2); // 2 flats: Bb major
//! assert_eq!(key, KEY_B_FLAT_MAJOR);
//!
//! assert_eq!(KEY_C_FLAT_MAJOR.alterations(), -7);  // Cb major has 7 flats
//! assert_eq!(KEY_C_SHARP_MAJOR.alterations(), 7);  // C# major has 7 sharps
//! ```

use crate::note::*;
use std::fmt;

/// The mode of a key is either major or minor.

#[derive(PartialEq, Debug, Clone)]
pub enum KeyMode {
    Major,
    Minor,
}

impl KeyMode {
    fn str(&self) -> &'static str {
        match self {
            KeyMode::Major => "major",
            KeyMode::Minor => "minor",
        }
    }
}

impl fmt::Display for KeyMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.str())
    }
}

/// In the tonal system, the key defines a context in which each
/// note/degree has a specific function.

#[derive(PartialEq, Debug, Clone)]
pub struct Key {
    tonic: Note,
    mode: KeyMode,
}

const SCALE_ORDER: [NoteRoot; 7] = [
    NoteRoot::C,
    NoteRoot::D,
    NoteRoot::E,
    NoteRoot::F,
    NoteRoot::G,
    NoteRoot::A,
    NoteRoot::B,
];

const SHARP_ORDER: [NoteRoot; 7] = [
    NoteRoot::F,
    NoteRoot::C,
    NoteRoot::G,
    NoteRoot::D,
    NoteRoot::A,
    NoteRoot::E,
    NoteRoot::B,
];

const FLAT_ORDER: [NoteRoot; 7] = [
    NoteRoot::B,
    NoteRoot::E,
    NoteRoot::A,
    NoteRoot::D,
    NoteRoot::G,
    NoteRoot::C,
    NoteRoot::F,
];

const PERFECT_FOURTH: i8 = 5;
const PERFECT_FIFTH: i8 = 7;

fn count_key_jumps(tonic_chroma: Chroma, interval: i8) -> i8 {
    let mut tonic = wrap_chroma(tonic_chroma);
    let mut jumps = 0;
    while tonic % 12 != 0 {
        tonic -= interval;
        jumps += 1;
    }
    jumps
}

#[inline]
fn count_flats(tonic_chroma: Chroma) -> i8 {
    count_key_jumps(tonic_chroma, PERFECT_FOURTH)
}

#[inline]
fn count_sharps(tonic_chroma: Chroma) -> i8 {
    count_key_jumps(tonic_chroma, PERFECT_FIFTH)
}

fn count_alterations(tonic: &Note, minor: bool) -> i8 {
    let chroma = match minor {
        true => tonic.chroma() + 3,
        false => tonic.chroma(),
    };
    if tonic.is_flat() {
        return -count_flats(chroma);
    }
    let sharps = count_sharps(chroma);
    if tonic.is_sharp() || sharps <= 6 {
        return sharps;
    }
    return -count_flats(chroma);
}

impl Key {
    /// Create a new major key given its number of alterations.
    /// `alts` should be in the interval [-7, 7].
    ///
    /// ```
    /// # use monk::key::*;
    /// assert_eq!(Key::from_alterations(0), KEY_C_MAJOR);
    /// assert_eq!(Key::from_alterations(4), KEY_E_MAJOR);
    /// assert_eq!(Key::from_alterations(-2), KEY_B_FLAT_MAJOR);
    /// ```
    ///
    /// # Panics
    ///
    /// This function panics when called with `alts < -7` or `alts > 7`.
    pub fn from_alterations(alts: i8) -> Self {
        if alts < -7 || alts > 7 {
            panic!("Attempted to create a key with alts < -7 or > 7.")
        }
        let chroma = PERFECT_FIFTH * (alts % 7);
        Key {
            tonic: Note::from_chroma(chroma, alts > 0).alter(alts / 7),
            mode: KeyMode::Major,
        }
    }

    /// Get the key's tonic.
    ///
    /// ```
    /// # use monk::key::*;
    /// # use monk::note::*;
    /// assert_eq!(KEY_A_FLAT_MINOR.tonic(), NOTE_A_FLAT)
    /// ```
    pub fn tonic(&self) -> Note {
        self.tonic.clone()
    }

    /// Get the key's mode.
    ///
    /// ```
    /// # use monk::key::*;
    /// assert_eq!(KEY_C_MAJOR.mode(), KeyMode::Major);
    /// assert_eq!(KEY_G_MINOR.mode(), KeyMode::Minor);
    /// ```
    pub fn mode(&self) -> KeyMode {
        self.mode.clone()
    }

    /// Compute the number of flats (`< 0`) or sharps (`> 0`) in the key.
    ///
    /// ```
    /// # use monk::key::*;
    /// assert_eq!(KEY_C_MAJOR.alterations(), 0);
    /// assert_eq!(KEY_B_MAJOR.alterations(), 5);
    /// assert_eq!(KEY_A_FLAT_MAJOR.alterations(), -4);
    /// ```
    pub fn alterations(&self) -> i8 {
        count_alterations(&self.tonic, self.mode == KeyMode::Minor)
    }

    /// Get the relative minor of the current key.
    ///
    /// ```
    /// # use monk::key::*;
    /// assert_eq!(KEY_G_MAJOR.relative_minor(), KEY_E_MINOR);
    /// assert_eq!(KEY_G_MINOR.relative_minor(), KEY_G_MINOR);
    /// ```
    pub fn relative_minor(&self) -> Self {
        match self.mode {
            KeyMode::Minor => self.clone(),
            KeyMode::Major => Key {
                // Relative minor tonic is the 6th degree of the major scale
                tonic: self.notes()[5].clone(),
                mode: KeyMode::Minor,
            },
        }
    }

    /// Get the relative major of the current key.
    ///
    /// ```
    /// # use monk::key::*;
    /// assert_eq!(KEY_F_MINOR.relative_major(), KEY_A_FLAT_MAJOR);
    /// assert_eq!(KEY_F_MAJOR.relative_major(), KEY_F_MAJOR);
    /// ```
    pub fn relative_major(&self) -> Self {
        match self.mode {
            KeyMode::Minor => Key {
                // Relative major tonic is the 3rd degree of the minor scale
                tonic: self.notes()[2].clone(),
                mode: KeyMode::Major,
            },
            KeyMode::Major => self.clone(),
        }
    }

    /// Get the scale corresponding to this key.
    ///
    /// ```
    /// # use monk::key::*;
    /// # use monk::note::*;
    /// assert_eq!(
    ///     KEY_G_MAJOR.notes(),
    ///     vec![NOTE_G, NOTE_A, NOTE_B, NOTE_C, NOTE_D, NOTE_E, NOTE_F_SHARP]
    /// );
    ///
    /// assert_eq!(
    ///     KEY_G_MINOR.notes(),
    ///     vec![NOTE_G, NOTE_A, NOTE_B_FLAT, NOTE_C, NOTE_D, NOTE_E_FLAT, NOTE_F]
    /// );
    /// ```
    pub fn notes(&self) -> Vec<Note> {
        let alts = self.alterations();
        let direction = if alts == 0 { 1 } else { alts / alts.abs() };
        let tonic_base = self.tonic.base();
        let mut altered = match (alts % 7) < 0 {
            true => FLAT_ORDER.to_vec(),
            false => SHARP_ORDER.to_vec(),
        };
        altered.truncate(alts.abs() as usize);

        let mut res: Vec<Note> = Vec::with_capacity(7);
        for base in SCALE_ORDER.iter().cycle() {
            if *base == tonic_base {
                res.push(self.tonic.clone());
            } else if !res.is_empty() {
                let alter = if altered.contains(base) { 1 } else { 0 };
                res.push(Note::new(base.clone(), direction * alter));
                if res.len() >= 7 {
                    break;
                }
            }
        }
        res
    }

    /// Get the note corresponding to a chromatic degree, within the context of this key.
    ///
    /// If the note isn't diatonic to this key, the result is the same as calling
    /// `Note::from_chroma` with the key's inherent "flatness" or "sharpness".
    ///
    /// ```
    /// # use monk::key::*;
    /// # use monk::note::*;
    /// assert_eq!(KEY_C_MAJOR.note_from_chroma(0), NOTE_C);
    /// assert_eq!(KEY_C_SHARP_MAJOR.note_from_chroma(0), NOTE_B_SHARP);
    ///
    /// // C is non-diatonic to Cb Major
    /// assert_eq!(KEY_C_FLAT_MAJOR.note_from_chroma(0), NOTE_C);
    /// ```
    pub fn note_from_chroma(&self, chroma: Chroma) -> Note {
        for note in self.notes() {
            if wrap_chroma(note.chroma() - chroma) == 0 {
                return note;
            }
        }
        // Non-diatonic note, fall back to Note::from_chroma
        Note::from_chroma(chroma, !self.tonic.is_flat())
    }
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.tonic, self.mode)
    }
}

pub const KEY_C_MAJOR: Key = Key {
    tonic: NOTE_C,
    mode: KeyMode::Major,
};
pub const KEY_A_MINOR: Key = Key {
    tonic: NOTE_A,
    mode: KeyMode::Minor,
};

// Circle of fifths, ascending (sharps)
pub const KEY_G_MAJOR: Key = Key {
    tonic: NOTE_G,
    mode: KeyMode::Major,
};
pub const KEY_E_MINOR: Key = Key {
    tonic: NOTE_E,
    mode: KeyMode::Minor,
};

pub const KEY_D_MAJOR: Key = Key {
    tonic: NOTE_D,
    mode: KeyMode::Major,
};
pub const KEY_B_MINOR: Key = Key {
    tonic: NOTE_B,
    mode: KeyMode::Minor,
};

pub const KEY_A_MAJOR: Key = Key {
    tonic: NOTE_A,
    mode: KeyMode::Major,
};
pub const KEY_F_SHARP_MINOR: Key = Key {
    tonic: NOTE_F_SHARP,
    mode: KeyMode::Minor,
};

pub const KEY_E_MAJOR: Key = Key {
    tonic: NOTE_E,
    mode: KeyMode::Major,
};
pub const KEY_C_SHARP_MINOR: Key = Key {
    tonic: NOTE_C_SHARP,
    mode: KeyMode::Minor,
};

pub const KEY_B_MAJOR: Key = Key {
    tonic: NOTE_B,
    mode: KeyMode::Major,
};
pub const KEY_G_SHARP_MINOR: Key = Key {
    tonic: NOTE_G_SHARP,
    mode: KeyMode::Minor,
};

pub const KEY_F_SHARP_MAJOR: Key = Key {
    tonic: NOTE_F_SHARP,
    mode: KeyMode::Major,
};
pub const KEY_D_SHARP_MINOR: Key = Key {
    tonic: NOTE_D_SHARP,
    mode: KeyMode::Minor,
};

pub const KEY_C_SHARP_MAJOR: Key = Key {
    tonic: NOTE_C_SHARP,
    mode: KeyMode::Major,
};
pub const KEY_A_SHARP_MINOR: Key = Key {
    tonic: NOTE_A_SHARP,
    mode: KeyMode::Minor,
};

// Circle of fifths, descending (flats)
pub const KEY_F_MAJOR: Key = Key {
    tonic: NOTE_F,
    mode: KeyMode::Major,
};
pub const KEY_D_MINOR: Key = Key {
    tonic: NOTE_D,
    mode: KeyMode::Minor,
};

pub const KEY_B_FLAT_MAJOR: Key = Key {
    tonic: NOTE_B_FLAT,
    mode: KeyMode::Major,
};
pub const KEY_G_MINOR: Key = Key {
    tonic: NOTE_G,
    mode: KeyMode::Minor,
};

pub const KEY_E_FLAT_MAJOR: Key = Key {
    tonic: NOTE_E_FLAT,
    mode: KeyMode::Major,
};
pub const KEY_C_MINOR: Key = Key {
    tonic: NOTE_C,
    mode: KeyMode::Minor,
};

pub const KEY_A_FLAT_MAJOR: Key = Key {
    tonic: NOTE_A_FLAT,
    mode: KeyMode::Major,
};
pub const KEY_F_MINOR: Key = Key {
    tonic: NOTE_F,
    mode: KeyMode::Minor,
};

pub const KEY_D_FLAT_MAJOR: Key = Key {
    tonic: NOTE_D_FLAT,
    mode: KeyMode::Major,
};
pub const KEY_B_FLAT_MINOR: Key = Key {
    tonic: NOTE_B_FLAT,
    mode: KeyMode::Minor,
};

pub const KEY_G_FLAT_MAJOR: Key = Key {
    tonic: NOTE_G_FLAT,
    mode: KeyMode::Major,
};
pub const KEY_E_FLAT_MINOR: Key = Key {
    tonic: NOTE_E_FLAT,
    mode: KeyMode::Minor,
};

pub const KEY_C_FLAT_MAJOR: Key = Key {
    tonic: NOTE_C_FLAT,
    mode: KeyMode::Major,
};
pub const KEY_A_FLAT_MINOR: Key = Key {
    tonic: NOTE_A_FLAT,
    mode: KeyMode::Minor,
};

#[cfg(test)]
mod test {
    use crate::key::*;

    #[test]
    fn alterations() {
        assert_eq!(KEY_C_MAJOR.alterations(), 0);
        assert_eq!(KEY_A_MINOR.alterations(), 0);
        assert_eq!(KEY_G_MAJOR.alterations(), 1);
        assert_eq!(KEY_E_MINOR.alterations(), 1);
        assert_eq!(KEY_D_MAJOR.alterations(), 2);
        assert_eq!(KEY_B_MINOR.alterations(), 2);
        assert_eq!(KEY_A_MAJOR.alterations(), 3);
        assert_eq!(KEY_F_SHARP_MINOR.alterations(), 3);
        assert_eq!(KEY_E_MAJOR.alterations(), 4);
        assert_eq!(KEY_C_SHARP_MINOR.alterations(), 4);
        assert_eq!(KEY_B_MAJOR.alterations(), 5);
        assert_eq!(KEY_G_SHARP_MINOR.alterations(), 5);
        assert_eq!(KEY_F_SHARP_MAJOR.alterations(), 6);
        assert_eq!(KEY_D_SHARP_MINOR.alterations(), 6);
        assert_eq!(KEY_C_SHARP_MAJOR.alterations(), 7);
        assert_eq!(KEY_A_SHARP_MINOR.alterations(), 7);
        assert_eq!(KEY_F_MAJOR.alterations(), -1);
        assert_eq!(KEY_D_MINOR.alterations(), -1);
        assert_eq!(KEY_B_FLAT_MAJOR.alterations(), -2);
        assert_eq!(KEY_G_MINOR.alterations(), -2);
        assert_eq!(KEY_E_FLAT_MAJOR.alterations(), -3);
        assert_eq!(KEY_C_MINOR.alterations(), -3);
        assert_eq!(KEY_A_FLAT_MAJOR.alterations(), -4);
        assert_eq!(KEY_F_MINOR.alterations(), -4);
        assert_eq!(KEY_D_FLAT_MAJOR.alterations(), -5);
        assert_eq!(KEY_B_FLAT_MINOR.alterations(), -5);
        assert_eq!(KEY_G_FLAT_MAJOR.alterations(), -6);
        assert_eq!(KEY_E_FLAT_MINOR.alterations(), -6);
        assert_eq!(KEY_C_FLAT_MAJOR.alterations(), -7);
        assert_eq!(KEY_A_FLAT_MINOR.alterations(), -7);
    }

    #[test]
    fn from_alterations() {
        assert_eq!(Key::from_alterations(0), KEY_C_MAJOR);
        assert_eq!(Key::from_alterations(1), KEY_G_MAJOR);
        assert_eq!(Key::from_alterations(2), KEY_D_MAJOR);
        assert_eq!(Key::from_alterations(3), KEY_A_MAJOR);
        assert_eq!(Key::from_alterations(4), KEY_E_MAJOR);
        assert_eq!(Key::from_alterations(5), KEY_B_MAJOR);
        assert_eq!(Key::from_alterations(6), KEY_F_SHARP_MAJOR);
        assert_eq!(Key::from_alterations(7), KEY_C_SHARP_MAJOR);
        assert_eq!(Key::from_alterations(-1), KEY_F_MAJOR);
        assert_eq!(Key::from_alterations(-2), KEY_B_FLAT_MAJOR);
        assert_eq!(Key::from_alterations(-3), KEY_E_FLAT_MAJOR);
        assert_eq!(Key::from_alterations(-4), KEY_A_FLAT_MAJOR);
        assert_eq!(Key::from_alterations(-5), KEY_D_FLAT_MAJOR);
        assert_eq!(Key::from_alterations(-6), KEY_G_FLAT_MAJOR);
        assert_eq!(Key::from_alterations(-7), KEY_C_FLAT_MAJOR);
    }

    #[test]
    fn major_scales() {
        assert_eq!(
            KEY_C_MAJOR.notes(),
            vec![NOTE_C, NOTE_D, NOTE_E, NOTE_F, NOTE_G, NOTE_A, NOTE_B]
        );
        assert_eq!(
            KEY_G_MAJOR.notes(),
            vec![NOTE_G, NOTE_A, NOTE_B, NOTE_C, NOTE_D, NOTE_E, NOTE_F_SHARP]
        );
        assert_eq!(
            KEY_A_FLAT_MAJOR.notes(),
            vec![
                NOTE_A_FLAT,
                NOTE_B_FLAT,
                NOTE_C,
                NOTE_D_FLAT,
                NOTE_E_FLAT,
                NOTE_F,
                NOTE_G,
            ]
        );
        assert_eq!(
            KEY_C_FLAT_MAJOR.notes(),
            vec![
                NOTE_C_FLAT,
                NOTE_D_FLAT,
                NOTE_E_FLAT,
                NOTE_F_FLAT,
                NOTE_G_FLAT,
                NOTE_A_FLAT,
                NOTE_B_FLAT,
            ]
        );
        assert_eq!(
            KEY_C_SHARP_MAJOR.notes(),
            vec![
                NOTE_C_SHARP,
                NOTE_D_SHARP,
                NOTE_E_SHARP,
                NOTE_F_SHARP,
                NOTE_G_SHARP,
                NOTE_A_SHARP,
                NOTE_B_SHARP,
            ]
        );
    }

    #[test]
    fn minor_scales() {
        assert_eq!(
            KEY_A_MINOR.notes(),
            vec![NOTE_A, NOTE_B, NOTE_C, NOTE_D, NOTE_E, NOTE_F, NOTE_G]
        );
        assert_eq!(
            KEY_E_MINOR.notes(),
            vec![NOTE_E, NOTE_F_SHARP, NOTE_G, NOTE_A, NOTE_B, NOTE_C, NOTE_D]
        );
        assert_eq!(
            KEY_F_MINOR.notes(),
            vec![
                NOTE_F,
                NOTE_G,
                NOTE_A_FLAT,
                NOTE_B_FLAT,
                NOTE_C,
                NOTE_D_FLAT,
                NOTE_E_FLAT,
            ]
        );
        assert_eq!(
            KEY_A_FLAT_MINOR.notes(),
            vec![
                NOTE_A_FLAT,
                NOTE_B_FLAT,
                NOTE_C_FLAT,
                NOTE_D_FLAT,
                NOTE_E_FLAT,
                NOTE_F_FLAT,
                NOTE_G_FLAT,
            ]
        );
        assert_eq!(
            KEY_A_SHARP_MINOR.notes(),
            vec![
                NOTE_A_SHARP,
                NOTE_B_SHARP,
                NOTE_C_SHARP,
                NOTE_D_SHARP,
                NOTE_E_SHARP,
                NOTE_F_SHARP,
                NOTE_G_SHARP,
            ]
        );
    }
}
