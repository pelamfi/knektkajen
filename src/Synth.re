
type synth;

let makeSynth: unit => synth = [%bs.raw
  {|
function () {
  const Tone = require('tone')
  const s = new Tone.Synth().toMaster();
  return s
}
|}
];

let play: (synth, float) => unit = [%bs.raw
  {|
function (synth, frequency) {
  synth.triggerAttackRelease(frequency, '8n')
}
|}
];

let effect: ((RelativeNotesState.acceptEvent) => (unit => option(unit => unit))) = {
    let synthRef: ref(option(synth)) = ref(None);
    RelativeNotesState.listenerEffect(stateChange => {
      switch (stateChange) {
      | CurrentNoteChanged(currentNote) =>
        switch (synthRef^) {
        | None =>
          // Webaudio can be initialized only after user input
          let synth = makeSynth();
          synthRef := Some(synth);
          play(synth, Note.frequency(currentNote));
        | Some(synth) => play(synth, Note.frequency(currentNote))
        };
      };
    })
  };
