open RelativeNotesState;
open ReactUtil;
open Belt;

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

type intervalKeyBinding = {
  keyCode: string,
  interval: Note.interval,
};

let intervalKeyBindings: list(intervalKeyBinding) =
  [
    "KeyQ",
    "KeyW",
    "KeyE",
    "KeyR",
    "KeyT",
    "KeyY",
    "KeyU",
    "KeyI",
    "KeyO",
    "KeyP",
    "BracketLeft",
    "BracketRight",
    "Backslash",
  ]
  |> List.mapWithIndex(_, (index: int, keyCode: string) =>
       (
         {
           {
             keyCode,
             interval: {
               steps: index,
             },
           };
         }: intervalKeyBinding
       )
     );

[@react.component]
let make = () => {
  let componentFactory = (i: int, current: int, id: string): reactComponent => {
    let current = current == i;
    let note: Note.note = {offset: i};
    <RelativeNoteComponent
      current
      acceptEvent=dispatch
      key={string_of_int(note.offset)}
      id
      note
    />;
  };

  let sliderConfig: InfiniteSlider.config = {
    componentFactory,
    styleBaseName: "relativeNotes",
    componentBaseName: "relativeNotes",
    itemSelectedDispatch: i => {
      let note: Note.note = {offset: i};
      dispatch(ClickNote(note));
    },
    itemsWindow: RangeOfInt.make((-12) * 2, 12 * 2),
    maxJump: 12,
  };

  /*
   let (foo, fooSet) = React.useReducer((_, x) => {x}, true);
   React.useEffect(() => {
     if (foo) {
       fooSet(false);
       Js.Global.setTimeout(
         () => {
           dispatch(ClickNote({offset: 1}))
         },
         100
       ) |> ignore;
       Js.Global.setTimeout(
         () => {
           dispatch(ClickNote({offset: 2}))
         },
         300
       ) |> ignore;
       Js.Global.setTimeout(
         () => {
           dispatch(ClickNote({offset: 0}))
         },
         600
       ) |> ignore;
     }
     None;
   });
   */

  let (currentNote, setCurrentNote) =
    React.useReducer((_, x) => x, initialState.currentNote);
  React.useEffect0(() => {
    let synthRef: ref(option(synth)) = ref(None);
    let listener = (stateChange): unit => {
      switch (stateChange) {
      | CurrentNoteChanged(currentNote) =>
        setCurrentNote(currentNote);
        switch (synthRef^) {
        | None =>
          // Webaudio can be initialized only after user input
          let synth = makeSynth();
          synthRef := Some(synth);
          play(synth, Note.frequency(currentNote));
        | Some(synth) => play(synth, Note.frequency(currentNote))
        };
      };
    };
    dispatch(RegisterListener(listener));
    Some(() => dispatch(UnregisterListener(listener)));
  });

  React.useEffect0(() => {
    let document = Webapi.Dom.Document.asEventTarget(Webapi.Dom.document);
    let listener = (event: Dom.keyboardEvent): unit => {
      let code = Webapi.Dom.KeyboardEvent.code(event);
      let shift: bool = Webapi.Dom.KeyboardEvent.shiftKey(event);
      let interval: option(Note.interval) =
        List.getBy(intervalKeyBindings, keyBinding =>
          keyBinding.keyCode == code
        )
        |> Option.map(_, binding => binding.interval);
      switch (interval) {
      | Some(interval) when !shift => dispatch(ClickInterval(interval))
      | Some(interval) when shift =>
        dispatch(ClickInterval(Note.inverse(interval)))
      | _ => ()
      };
    };
    Webapi.Dom.EventTarget.addKeyDownEventListener(listener, document);
    Some(
      () =>
        Webapi.Dom.EventTarget.removeKeyDownEventListener(listener, document),
    );
  });

  <InfiniteSlider config=sliderConfig selected={currentNote.offset} />;
};