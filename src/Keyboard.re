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

let intervalForKeyboardEventNote = (event: Dom.keyboardEvent): option(Note.interval) => {
    let code = Webapi.Dom.KeyboardEvent.code(event);
    let shift: bool = Webapi.Dom.KeyboardEvent.shiftKey(event);
    let invert = (invert: bool, interval): interval => invert ? inverse(interval) : interval
    
    List.getBy(intervalKeyBindings, keyBinding =>
      keyBinding.keyCode == code
    )
    |> Option.map(_, binding => invert(shift, binding.interval));
}

let listenerEffect = (dispatch: RelativeNotesState.acceptEvent, _): option(unit => unit) => {
    let document = Webapi.Dom.Document.asEventTarget(Webapi.Dom.document);

    let keyDownListener = (event: Dom.keyboardEvent): unit => {
      intervalForKeyboardEventNote(event) 
        |> Option.map(_, interval => {dispatch(KeyDownInterval(interval))})
        |> ignore
    };
    
    let keyUpListener = (event: Dom.keyboardEvent): unit => {
      intervalForKeyboardEventNote(event) 
        |> Option.map(_, interval => {dispatch(KeyUpInterval(interval))})
        |> ignore
    };

    Webapi.Dom.EventTarget.addKeyDownEventListener(keyDownListener, document);
    Webapi.Dom.EventTarget.addKeyUpEventListener(keyDownListener, document);
    Some(
      () => {
        Webapi.Dom.EventTarget.removeKeyDownEventListener(keyDownListener, document)
        Webapi.Dom.EventTarget.removeKeyUpEventListener(keyUpListener, document)
      }
    );
  }