open Belt;

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

let listenerEffect = (dispatch: RelativeNotesState.acceptEvent, _): option(unit => unit) => {
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
  }