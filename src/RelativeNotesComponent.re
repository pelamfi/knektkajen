open RelativeNotesState;
open ReactUtil;
open Belt;

type intervalKeyBinding = {
  keyCode: string,
  interval: Note.interval
}

let intervalKeyBindings: list(intervalKeyBinding) = [
  "KeyQ", "KeyW", "KeyE", "KeyR", "KeyT", "KeyY", "KeyU", "KeyI", "KeyO", "KeyP", "BracketLeft", "BracketRight", "Backslash"] 
    |> List.mapWithIndex(_, (index: int, keyCode: string): intervalKeyBinding => {{keyCode: keyCode, interval: {steps: index}}});

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducer(updateState, initialState);

  let componentFactory = (i: int, current: int, id: string): reactComponent => {
    let current = current == i;
    let note: Note.note = {offset: i};
    <RelativeNoteComponent
      current
      acceptEvent=dispatch
      key={string_of_int(note.offset)}
      id={id}
      note
    />;
  };

  let sliderConfig: InfiniteSlider.config = {
    componentFactory,
    styleBaseName: "relativeNotes",
    componentBaseName: "relativeNotes",
    itemSelectedDispatch: i => {
      let note: Note.note = {offset: i};
      dispatch(ClickNote(note))
    },
    itemsWindow: RangeOfInt.make((-12) * 1, 12 * 1),
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

  React.useEffect0(() => {
    let document = Webapi.Dom.Document.asEventTarget(Webapi.Dom.document);
    let listener = (event: Dom.keyboardEvent): unit => {
      let code = Webapi.Dom.KeyboardEvent.code(event);
      let shift: bool = Webapi.Dom.KeyboardEvent.shiftKey(event);
      let interval: option(Note.interval) = List.getBy(intervalKeyBindings, keyBinding => {keyBinding.keyCode == code}) |> Option.map(_, binding => {binding.interval});
      switch(interval) {
        | Some(interval) when !shift =>
        dispatch(ClickInterval(interval));
        | Some(interval) when shift =>
        dispatch(ClickInterval(Note.inverse(interval)));
        | _ => ()
      }
    };
    Webapi.Dom.EventTarget.addKeyDownEventListener(listener, document);
    Some(() => {
      Webapi.Dom.EventTarget.removeKeyDownEventListener(listener, document);
    })
  });

  <InfiniteSlider config=sliderConfig selected={state.currentNote.offset}/>
  
  
};