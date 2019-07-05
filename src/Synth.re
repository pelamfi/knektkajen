
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
