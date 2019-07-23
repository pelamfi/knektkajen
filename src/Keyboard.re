open Belt;
open Note;

type intervalKeyBinding = {
  keyCode: string,
  interval: Note.interval,
};

let chordKeyBindings: list(intervalKeyBinding) =
  [
    "KeyZ",
    "KeyX",
    "KeyC",
    "KeyV",
    "KeyB",
    "KeyN",
    "KeyM",
    "Comma",
    "Period",
    "Slash",
  ]
  |> List.mapWithIndex(_, (index: int, keyCode: string) =>
       (
         {
           {
             keyCode,
             interval: {
               steps: index + 1,
             },
           };
         }: intervalKeyBinding
       )
     );

let chordToggleKeyBindings: list(intervalKeyBinding) =
  [
    "KeyA",
    "KeyS",
    "KeyD",
    "KeyF",
    "KeyG",
    "KeyH",
    "KeyJ",
    "KeyK",
    "KeyL",
    "Semicolon",
    "Quote",
    "Enter",
  ]
  |> List.mapWithIndex(_, (index: int, keyCode: string) =>
       (
         {
           {
             keyCode,
             interval: {
               steps: index + 1,
             },
           };
         }: intervalKeyBinding
       )
     );

let intervalKeyBindingsForward: list(intervalKeyBinding) =
  [
    "Space",
    "Digit1",
    "Digit2",
    "Digit3",
    "Digit4",
    "Digit5",
    "Digit6",
    "Digit7",
    "Digit8",
    "Digit9",
    "Digit0",
    "Minus",
    "Equal",
    "Backspace",
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

let intervalKeyBindingsReverse: list(intervalKeyBinding) =
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
               steps: -index - 1,
             },
           };
         }: intervalKeyBinding
       )
     );

let intervalKeyBindingsOther: list(intervalKeyBinding) = [{keyCode: "Space", interval: {steps: 0}}]

let intervalKeyBindings: list(intervalKeyBinding) = List.concatMany([|intervalKeyBindingsForward, intervalKeyBindingsReverse, intervalKeyBindingsOther|])

let noteTriggerForKeyboardEvent = (event: Dom.keyboardEvent, ~keyUp: bool): option(RelativeNotesState.trigger) => {
    let code = Webapi.Dom.KeyboardEvent.code(event);
    let shift: bool = Webapi.Dom.KeyboardEvent.shiftKey(event);
    let repeat: bool = Webapi.Dom.KeyboardEvent.repeat(event);
    if (repeat) {
      None
    } else {
      let invert = (invert: bool, interval): interval => invert ? inverse(interval) : interval

      Js.log("code " ++ code);

      let interval = List.getBy(intervalKeyBindings, keyBinding =>
        keyBinding.keyCode == code
      )
      |> Option.map(_, binding => invert(shift, binding.interval));

      let chordInterval = List.getBy(chordKeyBindings, keyBinding =>
        keyBinding.keyCode == code
      )
      |> Option.map(_, binding => binding.interval);

      let chordIntervalToggle = List.getBy(chordToggleKeyBindings, keyBinding =>
        keyBinding.keyCode == code
      )
      |> Option.map(_, binding => binding.interval);

      switch(interval, chordInterval, chordIntervalToggle, keyUp) {
        | (Some(_), None, None, true) =>
          Some(Release(Keyboard(code)))
        | (None, Some(_), None, true) =>
          Some(ChordRelease(Keyboard(code)))
        | (Some(interval), None, None, false) =>
          Some(IntervalAttack(interval, Keyboard(code)))
        | (None, Some(chordInterval), None, false) =>
          Some(ChordPrime(chordInterval, Keyboard(code)))
        | (None, None, Some(chordInterval), false) =>
          Some(ChordToggle(chordInterval))
        | (_, _, _, _) => None
      }
    }
}

let listenerEffect = (dispatch: RelativeNotesState.acceptEvent, _): option(unit => unit) => {
    let document = Webapi.Dom.Document.asEventTarget(Webapi.Dom.document);

    let keyDownListener = (event: Dom.keyboardEvent): unit => {
      noteTriggerForKeyboardEvent(event, ~keyUp = false)
        |> Option.map(_, trigger => {dispatch(NoteTrigger(trigger))})
        |> ignore
    };
    
    let keyUpListener = (event: Dom.keyboardEvent): unit => {
      noteTriggerForKeyboardEvent(event, ~keyUp = true)
        |> Option.map(_, trigger => {dispatch(NoteTrigger(trigger))})
        |> ignore
    };

    Webapi.Dom.EventTarget.addKeyDownEventListener(keyDownListener, document);
    Webapi.Dom.EventTarget.addKeyUpEventListener(keyUpListener, document);
    Some(
      () => {
        Webapi.Dom.EventTarget.removeKeyDownEventListener(keyDownListener, document)
        Webapi.Dom.EventTarget.removeKeyUpEventListener(keyUpListener, document)
      }
    );
  }