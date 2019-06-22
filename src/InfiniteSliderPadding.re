open Belt

type animationRequest = {
  durationS: float,
  startWidth: float,
  endWidth: float,
};

type animationState = {
  t: float,
  request: animationRequest
};

type command =
  | Start(animationState)
  | Nop
  | Stop;

type dispatchCompleted = (animationState) => unit;

type state =
  | Idle
  | Animating(animationState);

type event =
  | Start(animationState)
  | Stop
  | Frame(float);

type effectCleanup = unit => unit;
type actionDispatch = (event) => unit;

let string_of_event = (event: event): string => {
  switch (event) {
  | Stop  => "AnimationComplete"
  | Start(_)  => "Start"
  | Frame(_) => "Frame"
  };
};

let paddingWidthStyle = (dist: float): string => {
  string_of_int(int_of_float(dist)) ++ "px"
} 

let animationStateMachine = (state, event): state => {
  switch (state, event) {
  | (Animating(_), Stop) => 
    Idle
  | (Idle, Start(animationState)) =>
    Animating(animationState)
  | (Animating(animationState), Frame(tFromStart)) =>
    let newT = tFromStart /. animationState.request.durationS
    let newAnimationState = {...animationState, t: newT}
    Animating(newAnimationState)
  | (state, _) =>
    Js.log("INVALID TRANSITION")
    state
  }
};


let computeWidth = (state: state): float => {
switch (state) {
    | Idle => 0.0
    | Animating(animationState) =>
      let width = (animationState.request.endWidth -. animationState.request.startWidth) *. animationState.t;
      width
  }  
};

type effect = unit => option(effectCleanup)

let timerEffect = (state, dispatch: actionDispatch): effect => {
  () => {
  switch (state) {
      | Animating(_) =>
        let startTimestamp: ref(option(float)) = ref(None)
        let rafId: ref(option(Webapi.rafId)) = ref(None)
        let rec rafCallback = (time: float) => {
          let startTime = startTimestamp^ |> Option.mapWithDefault(_, time, x => x)
          rafId := Some(Webapi.requestCancellableAnimationFrame(rafCallback))
          startTimestamp := Some(startTime)
          let t = (time -. startTime)
          dispatch(Frame(t))
        }
        rafId := Some(Webapi.requestCancellableAnimationFrame(rafCallback))
        Some(() => {
          rafId^ |> Option.map(_, Webapi.cancelAnimationFrame) |> ignore
          })
      | Idle => 
        None
      }  
  }
};

let string_of_animationState = (animationState: animationState): string => "TODO";

[@react.component]
let make = (~command: command, ~dispatchCompleted: dispatchCompleted, ~id: string) => {

  let (state, dispatch) = React.useReducer(animationStateMachine, Idle);
  
  switch (command, state) {
    | (Start(animationState), Idle) => dispatch(Start(animationState))
    | (Nop, Animating(animationState)) when animationState.t >= 1.0 =>
      dispatchCompleted(animationState)
      dispatch(Stop)
    | (Stop, Animating(animationState)) => 
      dispatchCompleted(animationState)
      dispatch(Stop)
    | (_, _) => ()
  };

  React.useEffect2(timerEffect(state, dispatch), (0, state != Idle));

  let width = computeWidth(state)

  let widthStyle = paddingWidthStyle(width);
  let style = ReactDOMRe.Style.make(~background="red", ~width = widthStyle, ());
  <div key="infiniteSliderAnimationPadding" id={id} className="infiniteSliderAnimationPadding" style={style} />;
};