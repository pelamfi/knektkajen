// https://www.wikiwand.com/en/Major_scale
// whole, whole, half, whole, whole, whole, half
// +2, +2, +1, +2 +2 +2 +1
// chromatic circle
// https://upload.wikimedia.org/wikipedia/commons/thumb/2/2a/Major_scale_in_the_chromatic_circle.png/440px-Major_scale_in_the_chromatic_circle.png
type noteInCMajor =
  | C
  | D
  | E
  | F
  | G
  | A
  | B; // H in finnish notation..

// I think chromatic scale is the one where all 12 notes are present.
// So I guess chromaticNote is somewhat correct name for this.
type chromaticNote =
  | C
  | CSharpDFlat
  | D
  | DSharpEFlat
  | E
  | F
  | FSharpGFlat
  | G
  | GSharpAFlat
  | A
  | ASharpBFlat
  | B;

type sharpOrFlat =
  | Flat // single letter note + flat
  | Sharp; // single letter note + sharp

// One note can have many names, depending on context.
// It is here expressed as the note in C major plus possible half step flat or sharp.
type noteName =
  | C
  | CSharpDFlat(sharpOrFlat)
  | D
  | DSharpEFlat(sharpOrFlat)
  | E
  | F
  | FSharpGFlat(sharpOrFlat)
  | G
  | GSharpAFlat(sharpOrFlat)
  | A
  | ASharpBFlat(sharpOrFlat)
  | B;

type noteNameClass =
  | Flat // single letter note - flat
  | Sharp // single letter note + sharp
  | Natural; // one of the "single letter notes" ie notes that make up CMajor

type noteHalfStep =
  | Flat // -1 half step
  | Natural // 0
  | Sharp; // +1 half step https://www.wikiwand.com/en/Sharp_(music)

type note = {
  // https://www.wikiwand.com/en/Piano_key_frequencies
  // offset from middle C (which is then denoted as 0 here)
  offset: int,
};

type scaleClass =
  | Chromatic
  | Major
  | Minor;

type scale =
  | Chromatic
  | Major(note)
  | Minor(note);

type scaleName = {
  noteName,
  scaleClass,
};

type interval = {steps: int};

// The number is in the scientific octave naming system. 0 is the octave where A is 27.5
// https://www.wikiwand.com/en/Musical_note#/section_Note_designation_in_accordance_with_octave_name
// https://www.wikiwand.com/en/Scientific_pitch_notation
type octave = {number: int}

let octaveOfNote = (note: note): octave => {
  let shiftedBy0Octave = note.offset + 12 * 4
  if (shiftedBy0Octave < 0) {
    {number: shiftedBy0Octave / 12 - 1}
  } else {
    {number: shiftedBy0Octave / 12}
  }
}

let subscriptOfOctave = (octave: octave): string => {
  switch(octave.number) {
    | 0 => {js|₀|js}
    | 1 => {js|₁|js}
    | 2 => {js|₂|js}
    | 3 => {js|₃|js}
    | 4 => {js|₄|js}
    | 5 => {js|₅|js}
    | 6 => {js|₆|js}
    | 7 => {js|₇|js}
    | 8 => {js|₈|js}
    | 9 => {js|₉|js}
    | _ => ""
  }
}

let middleC: note = {offset: 0};

let cMajorName: scaleName = {noteName: C, scaleClass: Major};

let moduloOffset = (n: note): int =>
  if (n.offset < 0) {
    11 - (- n.offset - 1) mod 12;
  } else {
    n.offset mod 12;
  };

let asChromaticNote = (n: note): chromaticNote => {
  switch (moduloOffset(n)) {
  | 0 => C
  | 1 => CSharpDFlat
  | 2 => D
  | 3 => DSharpEFlat
  | 4 => E
  | 5 => F
  | 6 => FSharpGFlat
  | 7 => G
  | 8 => GSharpAFlat
  | 9 => A
  | 10 => ASharpBFlat
  | _ => B
  };
};

// https://www.wikiwand.com/en/Chromatic_scale
// In descending notes, flat notation would be used, but I'm not interested
// in adding ascending/descending to this model yet..
let chromaticNoteNames: list(noteName) = [
  C,
  CSharpDFlat(Sharp),
  D,
  DSharpEFlat(Sharp),
  E,
  F,
  FSharpGFlat(Sharp),
  G,
  GSharpAFlat(Sharp),
  A,
  ASharpBFlat(Sharp),
  B,
];

let range_of_int = (base: note, start: int, rangeEnd: int): list(note) => {
  RangeOfInt.make(start, rangeEnd)
  |> RangeOfInt.map(_, x => {offset: base.offset + x});
};

let inverse = (i: interval): interval => {steps: i.steps * (-1)};

let chromaticIntervals = Array.init(12, i => {steps: i}) |> Array.to_list;

let scaleIntervals = (s: scaleClass): list(interval) => {
  switch (s) {
  | Chromatic => chromaticIntervals
  | Major => [{steps: 0}, {steps: 2}, {steps: 3}] // TODO: Correct steps
  | Minor => [{steps: 0}, {steps: 1}]
  };
};

let noteApplyInterval = (n: note, i: interval): note => {
  offset: n.offset + i.steps,
};

//let noteApplyIntervalWithLooping = (n: note, i: interval): note => {
//  offset: n.offset + i.steps,
//};

let scale = (rootNote: note, s: scaleClass): list(note) => {
  let intervals = scaleIntervals(s);
  Belt.List.map(intervals, interval => noteApplyInterval(rootNote, interval));
};

let asNoteNameClass = (n: sharpOrFlat): noteNameClass => {
  switch (n) {
  | Sharp => Sharp
  | Flat => Flat
  };
};

let asNoteNameClass = (n: noteName): noteNameClass => {
  switch (n) {
  | DSharpEFlat(sharpOrFlatValue)
  | CSharpDFlat(sharpOrFlatValue)
  | FSharpGFlat(sharpOrFlatValue)
  | GSharpAFlat(sharpOrFlatValue)
  | ASharpBFlat(sharpOrFlatValue) => asNoteNameClass(sharpOrFlatValue)
  | _ => Natural
  };
};

// If you have a chromatic note and a flat or sharp scale, the
// scale sharp/flat decides wheter the note is denoted flat or sharp.
let asNoteName = (n: chromaticNote, c: noteNameClass): noteName => {
  let sharpOrFlatValue: sharpOrFlat =
    switch (c) {
    | Flat => Flat
    | Natural => Sharp
    | Sharp => Sharp
    };

  switch (n) {
  | C => C
  | CSharpDFlat => CSharpDFlat(sharpOrFlatValue)
  | D => D
  | DSharpEFlat => DSharpEFlat(sharpOrFlatValue)
  | E => E
  | F => F
  | FSharpGFlat => FSharpGFlat(sharpOrFlatValue)
  | G => G
  | GSharpAFlat => GSharpAFlat(sharpOrFlatValue)
  | A => A
  | ASharpBFlat => ASharpBFlat(sharpOrFlatValue)
  | B => B
  };
};

// If you know a note and a scale, you know what that note is called
let noteNameForScaleName = (n: note, scale: scaleName): noteName => {
  let chromaticNote = asChromaticNote(n);
  let noteClass = asNoteNameClass(scale.noteName);
  asNoteName(chromaticNote, noteClass);
};

let stringOfNoteName = (n: noteName): string => {
  switch (n) {
  | C => "C"
  | CSharpDFlat(Sharp) => {js|C♯|js}
  | CSharpDFlat(Flat) => {js|D♭|js}
  | D => "D"
  | DSharpEFlat(Sharp) => {js|D♯|js}
  | DSharpEFlat(Flat) => {js|E♭|js}
  | E => "E"
  | F => "F"
  | FSharpGFlat(Sharp) => {js|F♯|js}
  | FSharpGFlat(Flat) => {js|G♭|js}
  | G => "G"
  | GSharpAFlat(Sharp) => {js|G♯|js}
  | GSharpAFlat(Flat) => {js|A♭|js}
  | A => "A"
  | ASharpBFlat(Sharp) => {js|A♯|js}
  | ASharpBFlat(Flat) => {js|B♭|js}
  | B => "B"
  };
};

let nameOfNoteInCMajor = (note: note): string => {
  stringOfNoteName(noteNameForScaleName(note, cMajorName)) ++ subscriptOfOctave(octaveOfNote(note))
};

let min = (note: note, otherNote: note): note => {
  {offset: Js.Math.min_int(note.offset, otherNote.offset)}
}

let frequency = (n: note): float => {
  // middle C is the 0, so shift by 9 half tones to A which is the nice round 440 hz
  Js.Math.pow_float(~base=2.0, ~exp=float(n.offset - 9) /. 12.0) *. 440.0;
};