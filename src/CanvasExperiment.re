open Belt;
type refState = ref(option(ReasonReact.reactRef));
open Webapi.Canvas;

let initial: refState = ref(None);

type dimensions = {
  width: float,
  height: float,
};

let drawOnContext = (context: Canvas2d.t, dimensions: dimensions): unit => {
  Canvas2d.clearRect(context, ~x=0., ~y=0., ~w=dimensions.width, ~h=dimensions.height);

  // https://github.com/reasonml-community/bs-webapi-incubator/blob/master/tests/Webapi/Webapi__Canvas/Webapi__Canvas__Canvas2d__test.re
  // https://stackoverflow.com/a/58604725/1148030
  Canvas2d.setFillStyle(context, String, "rgba(0,128,169,0.1)");
  Canvas2d.fillRect(context, ~x=10.0, ~y=10.0, ~w=30.0, ~h=30.0);
  Canvas2d.setFillStyle(context, String, "rgba(0,170,40,0.2)");
  Canvas2d.fillRect(context, ~x=20.0, ~y=20.0, ~w=30.0, ~h=30.0);
};

let canvasDimensions = (canvasElement: Dom.element): dimensions => {
  Webapi.Canvas.CanvasElement.{
    width: float_of_int(width(canvasElement)),
    height: float_of_int(height(canvasElement)),
  };
};

let drawOnCanvasElement = (canvasElement: Dom.element): unit => {
  open Webapi.Canvas.CanvasElement;
  let context = getContext2d(canvasElement);
  drawOnContext(context, canvasDimensions(canvasElement));
};

[@react.component]
let make = () => {
  // https://github.com/reasonml/reason-react/issues/407#issuecomment-486083140
  // https://github.com/af/shmup.re/blob/master/src/Main.re#L39
  let canvasElementRef: React.Ref.t(option(Dom.element)) = React.useRef(None);
  React.useLayoutEffect2(
    () => {
      React.Ref.current(canvasElementRef) |> Option.map(_, drawOnCanvasElement) |> ignore;
      None;
    },
    ((), ()),
  );

  <div>
    <canvas
      id="myCanvas"
      width="200"
      height="100"
      ref={ReactDOMRe.Ref.callbackDomRef(elem => React.Ref.setCurrent(canvasElementRef, Js.Nullable.toOption(elem)))}
    />
  </div>;
};