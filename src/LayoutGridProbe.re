open ReactUtil;
open Belt;
open Webapi;

type config = {
  id0: string,
  id1: string
};

type gridLayoutInfo = {
  leftX: float,
  pitchX: float,
};

let getGridLayoutInfo =
    (config: config): option(gridLayoutInfo) => {
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
        Js.log("foo " ++ Js.Float.toString(left0) ++ " foo " ++ Js.Float.toString(pitchX));
        {leftX: left0, pitchX}
      }
    )
  );
};

let config: config = {id0: "probe0", id1: "probe1"};

type gridLayoutInfoCallback = (gridLayoutInfo) => unit;

[@react.component]
let make = (~infoCallback: gridLayoutInfoCallback) => {
  
  React.useLayoutEffect0(
    () => {
      let observer = ObserveResize.observeResize(config.id0, () => {
        Option.map(getGridLayoutInfo(config), infoCallback) |> ignore;
        ()
      });

      Option.map(getGridLayoutInfo(config), infoCallback) |> ignore;

      Some(() => {ObserveResize.unobserve(observer)})
    }
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