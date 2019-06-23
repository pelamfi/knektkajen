open Belt

type animationState = {
  t: float,
  durationMs: float,
  startWidth: float,
  endWidth: float,
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

let stringOfAnimationState = (state: animationState): string => {
  "{t:" 
  ++ Js.Float.toString(state.t)
  ++ ", durationMs:"
  ++ Js.Float.toString(state.durationMs)
  ++ ", startWidth:"
  ++ Js.Float.toString(state.startWidth)
  ++ ", endWidth:"
  ++ Js.Float.toString(state.endWidth)
  ++ "}"  
}

let stringOfEvent = (event: event): string => {
  switch (event) {
  | Stop  => "AnimationComplete"
  | Start(animationState)  => "Start("++ stringOfAnimationState(animationState)++ ")"
  | Frame(t) => "Frame("++Js.Float.toString(t)++")"
  };
};

let stringOfState = (state: state): string => {
  switch(state) {
    | Idle => "Idle"
    | Animating(animationState) => "Animating(" ++ stringOfAnimationState(animationState) ++ ")"
  }
}

let stringOfCommand = (command): string => {
  switch (command) {
  | Nop => "Nop"
  | Stop => "Stop"
  | Start(animationState) => "Start(" ++ stringOfAnimationState(animationState) ++ ")"
  }
};

let paddingWidthStyle = (dist: float): string => {
  string_of_int(int_of_float(dist)) ++ "px"
} 

let stateMachine = (state, event): state => {
  switch (state, event) {
  | (Animating(_), Stop) => 
    Idle
  | (Idle, Start(animationState)) =>
    Animating(animationState)
  | (Animating(animationState), Frame(tFromStart)) =>
    let newT = tFromStart /. animationState.durationMs
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
      let width = (animationState.endWidth -. animationState.startWidth) *. animationState.t;
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

let logTransition = ((state: state, dispatch: event => unit)) => {
  let wrapped: event => unit = (event: event): unit => {
    Js.log("transition on event" ++ stringOfEvent(event) ++ " to state " ++ stringOfState(stateMachine(state, event)))
    dispatch(event)
  };
  (state, wrapped)
};

[@react.component]
let make = (~command: command, ~dispatchCompleted: dispatchCompleted, ~id: string) => {

  // let (state, dispatch) = logTransition(React.useReducer(stateMachine, Idle));
  let (state, dispatch) = React.useReducer(stateMachine, Idle);
  
  switch (command, state) {
    | (Start(animationState), Idle) => dispatch(Start(animationState))
      Js.log("foo start")
    | (_, Animating(animationState)) when animationState.t >= 1.0 =>
      Js.log("foo")
      dispatchCompleted(animationState)
      dispatch(Stop)
    | (Stop, Animating(animationState)) => 
      Js.log("bar")
      dispatchCompleted(animationState)
      dispatch(Stop)
    | (_, Animating(animationState)) => 
      Js.log(stringOfAnimationState(animationState) ++ " FOO " ++ stringOfCommand(command))
    | (_, Idle) =>
      ()
  };

  React.useEffect2(timerEffect(state, dispatch), (0, state != Idle));

  switch (state) {
    | Animating(animationState) =>
      if (animationState.t >= animationState.durationMs) {
        dispatchCompleted(animationState)
        dispatch(Stop)
      }
    | Idle => ()
  }

  let width = computeWidth(state)

  let widthStyle = paddingWidthStyle(width);
  let style = ReactDOMRe.Style.make(~background="red", ~width = widthStyle, ());
  <div key="infiniteSliderAnimationPadding" id={id} className="infiniteSliderAnimationPadding" style={style} />;
};