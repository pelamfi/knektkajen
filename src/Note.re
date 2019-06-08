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

let cMajorName: scaleName = {noteName: C, scaleClass: Major};

let moduloOffset = (n: note): int =>
  if (n.offset < 0) {
    11 - abs(n.offset) mod 12;
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

let range = (base: note, start: int, rangeEnd: int): list(note) => {
  Range.make(start, rangeEnd) |> Range.map(_, x => {offset: base.offset + x});
};

// TODO: Interval should probably be sum type of 12 items
type interval = {steps: int};

let chromaticIntervals = Array.init(12, i => {steps: i}) |> Array.to_list;

let scaleIntervals = (s: scaleClass): list(interval) => {
  switch (s) {
  | Chromatic => chromaticIntervals
  | Major => [{steps: 0}, {steps: 2}, {steps: 3}] // TODO: Correct steps
  | Minor => [{steps: 0}, {steps: 1}]
  };
};

let middleC: note = {offset: 0};

let apply = (n: note, i: interval): note => {offset: n.offset + i.steps};

let scale = (rootNote: note, s: scaleClass): list(note) => {
  let intervals = scaleIntervals(s);
  Belt.List.map(intervals, interval => apply(rootNote, interval));
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
let name = (n: note, scale: scaleName): noteName => {
  let chromaticNote = asChromaticNote(n);
  let noteClass = asNoteNameClass(scale.noteName);
  asNoteName(chromaticNote, noteClass);
};

let asString = (n: noteName): string => {
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

let frequency = (n: note): float => {
  // middle C is the 0, so shift by 9 half tones to A which is the nice round 440 hz
  Js.Math.pow_float(2.0, float(n.offset - 9) /. 12.0) *. 440.0;
};