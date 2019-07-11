open Belt;
open Note;

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

let noteTriggerForKeyboardEvent = (event: Dom.keyboardEvent, ~keyUp: bool): option(RelativeNotesState.trigger) => {
    let code = Webapi.Dom.KeyboardEvent.code(event);
    let shift: bool = Webapi.Dom.KeyboardEvent.shiftKey(event);
    let repeat: bool = Webapi.Dom.KeyboardEvent.repeat(event);
    if (repeat) {
      None
    } else {
      let invert = (invert: bool, interval): interval => invert ? inverse(interval) : interval

      let interval = List.getBy(intervalKeyBindings, keyBinding =>
        keyBinding.keyCode == code
      )
      |> Option.map(_, binding => invert(shift, binding.interval));

      switch(interval, keyUp) {
        | (Some(_), true) =>
          Some(Release(Keyboard(code)))
        | (Some(interval), false) =>
          Some(IntervalAttack(interval, Keyboard(code)))
        | (None, _) => None
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