open Belt;
open ReactUtil;

type appComponent =
  | NoteInfoStrips
  | CanvasExperiment;

type debugMode =
  | NoteInfoStrips2xZoom;

type appTopLevelCommand =
  | ToggleAppComponent(appComponent)
  | ToggleDebugMode(debugMode);

// About Set: https://stackoverflow.com/a/58268653/1148030

module AppComponentComparable =
  Id.MakeComparable({
    type t = appComponent;
    let cmp = Pervasives.compare;
  });

module DebugModeComparable =
  Id.MakeComparable({
    type t = debugMode;
    let cmp = Pervasives.compare;
  });

type appComponents = Set.t(appComponent, AppComponentComparable.identity);
type debugModes = Set.t(debugMode, DebugModeComparable.identity);

type appTopLevelState = {
  appComponents,
  debugModes,
};

let initial: appTopLevelState = {
  appComponents:
    Set.fromArray([|NoteInfoStrips|], ~id=(module AppComponentComparable)),
  debugModes: Set.fromArray([||], ~id=(module DebugModeComparable)),
};

let toggleAppComponent =
    (current: appComponents, key: appComponent): appComponents =>
  if (Set.has(current, key)) {
    Set.remove(current, key);
  } else {
    Set.add(current, key);
  };

let appTopLevelStateReducer =
    (prev: appTopLevelState, command: appTopLevelCommand): appTopLevelState => {
  switch (command) {
  | ToggleAppComponent(component) =>
    let t = toggleAppComponent(prev.appComponents, component);
    {...prev, appComponents: t};
  | _ => prev
  };
};

type dispatch = appTopLevelCommand => unit;

let debugKeyboardListenerEffect =
    (dispatch: dispatch, _): option(unit => unit) => {
  let document = Webapi.Dom.Document.asEventTarget(Webapi.Dom.document);

  let keyUpListener = (event: Dom.keyboardEvent): unit => {
    Webapi.Dom.KeyboardEvent.(
      if (shiftKey(event) && ctrlKey(event) && altKey(event)) {
        let code = code(event);
        switch (code) {
        | "KeyC" => dispatch(ToggleAppComponent(CanvasExperiment))
        | "KeyS" => dispatch(ToggleAppComponent(NoteInfoStrips))
        | _ => ()
        };
      }
    );
  };

  Webapi.Dom.EventTarget.addKeyUpEventListener(keyUpListener, document);
  Some(
    () =>
      Webapi.Dom.EventTarget.removeKeyUpEventListener(
        keyUpListener,
        document,
      ),
  );
};

[@react.component]
let make = () => {
  let (state, dispatchCommand) =
    React.useReducer(appTopLevelStateReducer, initial);

  let elements: list(reactComponent) = [
    if (Set.has(state.appComponents, CanvasExperiment)) {
      <CanvasExperiment key="canvasExperiment" />;
    } else {
      emptyFragment;
    },
    if (Set.has(state.appComponents, NoteInfoStrips)) {
      <NoteInfoStrips key="noteInfoStrips" />;
    } else {
      emptyFragment;
    },
  ];

  React.useEffect0(debugKeyboardListenerEffect(dispatchCommand));

  <> {asReact(elements)} </>;
};