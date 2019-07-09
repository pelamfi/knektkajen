
type synth;
type tone;

let requireTone: unit => tone  = [%bs.raw
  {|
function () {
  const Tone = require('tone')
  return Tone
}
|}
];

let makeSynth: tone => synth = [%bs.raw
  {|
function (Tone) {
  const s = new Tone.Synth().toMaster();
  return s
}
|}
];

let triggerAttackRelease: (synth, float) => unit = [%bs.raw
  {|
function (synth, frequency) {
    console.log("triggerAttackRelease", frequency)
    synth.triggerAttackRelease(frequency, '8n')
}
|}
];

let triggerAttack: (synth, float) => unit = [%bs.raw
  {|
function (synth, frequency) {
    console.log("triggerAttack", frequency)
    synth.triggerAttack(frequency)
}
|}
];

let triggerRelease: (synth) => unit = [%bs.raw
  {|
function (synth) {
    synth.triggerRelease()
}
|}
];

let playVoice = (synth: synth, voice: RelativeNotesState.voice): unit => {
    switch(voice.key, voice.state, voice.prevState) {
        | (Single(note), _, Attack) =>
        triggerRelease(synth)
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
          let tone = requireTone()
          let synth = makeSynth(tone);
          synthRef := Some(synth);
          playVoice(synth, voice);
        | Some(synth) => playVoice(synth, voice)
        };
      | _ => ()
      };
    })
  };
