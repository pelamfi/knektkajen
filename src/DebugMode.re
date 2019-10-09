open Belt;

type debugMode =
  | NoteInfoStrips2xZoom;

module DebugModeComparable =
  Id.MakeComparable({
    type t = debugMode;
    let cmp = Pervasives.compare;
  });

let initial = Set.fromArray([||], ~id=(module DebugModeComparable))

type debugModes = Set.t(debugMode, DebugModeComparable.identity);