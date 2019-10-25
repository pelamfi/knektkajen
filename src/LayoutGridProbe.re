open Belt;
open Webapi;

type config = {
  id0: string,
  id1: string
};

type layoutGridInfo = {
  leftX: float,
  pitchX: float,
};

let getLayoutGridInfo =
    (config: config): option(layoutGridInfo) => {
  let doc = Webapi.Dom.document;
  let id0 = config.id0;
  let id1 = config.id1;
  let left = (id: string): option(float) => {
    let e = Webapi.Dom.Document.getElementById(id, doc);
    // https://stackoverflow.com/a/18053642/1148030
    let boundingClientRect =
      Option.map(e, Webapi.Dom.Element.getBoundingClientRect);
    Option.map(boundingClientRect, Dom.DomRect.left);
  };
  Option.flatMap(left(id0), left0 =>
    Option.map(left(id1), left1 => {
        let pitchX = left1 -. left0;
        {leftX: left0, pitchX}
      }
    )
  );
};

let config: config = {id0: "probe0", id1: "probe1"};

type layoutGridInfoCallback = (layoutGridInfo) => unit;

[@react.component]
let make = (~debugModes: DebugMode.debugModes, ~infoCallback: layoutGridInfoCallback) => {
  
  // The debugModes is here only as a hack to get new dimensions when zoom debug mode is used.
  // Resize observer does not update when transform change

  React.useLayoutEffect2(
    () => {
      let sendUpdate = () => {
        Option.map(getLayoutGridInfo(config), infoCallback) |> ignore;
        ()
      }

      let observer = ObserveResize.observeResize(config.id0, sendUpdate);
      Some(() => {ObserveResize.unobserve(observer)})
    },
    ((), debugModes)
  );

  <div className="noteInfoGridProbeStrip">
  <div className="noteInfoGridProbeRow">
  <div id="probe0" className="noteInfoGridProbeCell">
  {React.string("0")}
  </div>
  <div id="probe1" className="noteInfoGridProbeCell">
  {React.string("1")}
  </div>
  </div>
  </div>;
};