
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
    synth.triggerAttackRelease(frequency, '8n')
}
|}
];

let triggerAttack: (synth, float) => unit = [%bs.raw
  {|
function (synth, frequency) {
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

let playVoice = (synths: list(synth), voice: RelativeNotesState.voice): unit => {
    let synth = List.nth(synths, voice.allocated)

    Js.log(Printf.sprintf("playVoice: allocated: %d, prevState: %s, state: %s", voice.allocated, RelativeNotesState.stringOfVoiceState(voice.prevState), RelativeNotesState.stringOfVoiceState(voice.state)))

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
    let synthsRef: ref(option(list(synth))) = ref(None);
    RelativeNotesState.listenerEffect(stateChange => {
      switch (stateChange) {
      | Voice(voice) =>
        switch (synthsRef^) {
        | None =>
            // Webaudio can be initialized only after user input
            let tone = requireTone()
            let synths: list(synth) = RangeOfInt.make(0, RelativeNotesState.voices) |> RangeOfInt.map(_, _ => makeSynth(tone));

            synthsRef := Some(synths);
            playVoice(synths, voice);
        | Some(synths) => playVoice(synths, voice)
        };
      | _ => ()
      };
    })
  };
