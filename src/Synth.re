
type synth;

let makeSynth: unit => synth = [%bs.raw
  {|
function () {
  const Tone = require('tone')
  const s = new Tone.PolySynth(6, new Tone.Synth()).toMaster();
  return s
}
|}
];

let triggerAttackRelease: (synth, float) => unit = [%bs.raw
  {|
function (synth, frequency) {
  synth.triggerAttackRelease(frequency, '8n')
}
|}
];

let triggerAttack: (synth, float) => unit = [%bs.raw
  {|
function (synth, frequency) {
  synth.triggerAttack(frequency, '8n')
}
|}
];

let triggerRelease: (synth, float) => unit = [%bs.raw
  {|
function (synth, frequency) {
  synth.triggeRelease(frequency, '8n')
}
|}
];

let playVoice = (synth: synth, voice: RelativeNotesState.voice): unit => {
    switch(voice.key, voice.state, voice.prevState) {
        | (Single(note), _, Attack) =>
        triggerRelease(synth, Note.frequency(note))
        | _ => ()
    }
    switch(voice.key, voice.state) {
        | (Single(note), AttackRelease) =>
        triggerAttackRelease(synth, Note.frequency(note))
        | (Single(note), Attack) =>
        triggerAttack(synth, Note.frequency(note))
        | _ => ()
    }
}

let effect: ((RelativeNotesState.acceptEvent) => (unit => option(unit => unit))) = {
    let synthRef: ref(option(synth)) = ref(None);
    RelativeNotesState.listenerEffect(stateChange => {
      switch (stateChange) {
      | Voice(voice) =>
        switch (synthRef^) {
        | None =>
          // Webaudio can be initialized only after user input
          let synth = makeSynth();
          synthRef := Some(synth);
          playVoice(synth, voice);
        | Some(synth) => playVoice(synth, voice)
        };
      | _ => ()
      };
    })
  };
