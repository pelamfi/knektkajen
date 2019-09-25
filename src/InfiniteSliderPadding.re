open Belt;

type timerMs = {
  start: float,
  last: float,
};

type animationState = {
  tInitial: float,
  t: float,
  durationMs: float,
  timer: option(timerMs),
  startWidth: float,
  endWidth: float,
};

type command =
  | Start(animationState)
  | Nop
  | Stop;

type dispatchCompleted = animationState => unit;

type state =
  | Idle(float)
  | Animating(animationState);

type event =
  | Start(animationState)
  | Stop
  | Frame(float);

type effectCleanup = unit => unit;
type actionDispatch = event => unit;

let initialState: animationState = {
  tInitial: 0.0,
  t: 0.0,
  durationMs: 1.0,
  timer: None,
  startWidth: 0.0,
  endWidth: 1.0,
};

let stringOfTimer = (timer: option(timerMs)): string => {
  Option.mapWithDefault(timer, "-", timer =>
    "("
    ++ Js.Float.toString(timer.start)
    ++ " - "
    ++ Js.Float.toString(timer.last)
    ++ " ms)"
  );
};

let stringOfAnimationState = (state: animationState): string => {
  "{tInitial:"
  ++ Js.Float.toString(state.tInitial)
  ++ ", t:"
  ++ Js.Float.toString(state.t)
  ++ ", durationMs:"
  ++ Js.Float.toString(state.durationMs)
  ++ ", timer:"
  ++ stringOfTimer(state.timer)
  ++ ", startWidth:"
  ++ Js.Float.toString(state.startWidth)
  ++ ", endWidth:"
  ++ Js.Float.toString(state.endWidth)
  ++ "}";
};

let stringOfEvent = (event: event): string => {
  switch (event) {
  | Stop => "Stop"
  | Start(animationState) =>
    "Start(" ++ stringOfAnimationState(animationState) ++ ")"
  | Frame(t) => "Frame(" ++ Js.Float.toString(t) ++ ")"
  };
};

let stringOfState = (state: state): string => {
  switch (state) {
  | Idle(width) => "Idle(" ++ Js.Float.toString(width) ++ ")"
  | Animating(animationState) =>
    "Animating(" ++ stringOfAnimationState(animationState) ++ ")"
  };
};

let stringOfCommand = (command): string => {
  switch (command) {
  | Nop => "Nop"
  | Stop => "Stop"
  | Start(animationState) =>
    "Start(" ++ stringOfAnimationState(animationState) ++ ")"
  };
};

let paddingWidthStyle = (dist: float): string => {
  string_of_int(int_of_float(dist)) ++ "px";
};

let computeWidth = (state: state): float => {
  switch (state) {
  | Idle(width) => width
  | Animating(animationState) =>
    if (animationState.endWidth < animationState.startWidth) {
      (animationState.startWidth -. animationState.endWidth)
      *. (1.0 -. animationState.t);
    } else {
      (animationState.endWidth -. animationState.startWidth)
      *. animationState.t;
    }
  };
};

let duration = (timer: option(timerMs)): float => {
  switch (timer) {
  | Some(timer) => timer.last -. timer.start
  | None => 0.0
  };
};

let updatedLast = (timer: option(timerMs), newLast: float): option(timerMs) => {
  switch (timer) {
  | Some(timer) =>
    Some({start: timer.start, last: Js.Math.max_float(timer.start, newLast)})
  | None => Some({start: newLast, last: newLast})
  };
};

let restart = (timer: option(timerMs)): option(timerMs) => {
  switch (timer) {
  | Some(timer) => Some({start: timer.last, last: timer.last})
  | None => None
  };
};

let stateMachine = (state, event): state => {
  let newState =
    switch (state, event) {
    | (Animating(_), Stop) => Idle(computeWidth(state))
    | (Idle(_), Start(animationState)) => Animating(animationState)
    | (Animating(animationState), Frame(timerMs)) =>
      let updatedTimer = updatedLast(animationState.timer, timerMs);
      let msFromStart = duration(updatedTimer);
      let newT =
        Js.Math.min_float(
          Js.Math.max_float(
            0.0,
            msFromStart /. animationState.durationMs +. animationState.tInitial,
          ),
          1.0,
        );
      Animating({...animationState, t: newT, timer: updatedTimer});
    | (state, _) =>
      Js.log("InfiniteSliderPadding: Error: INVALID TRANSITION");
      state;
    };
  newState;
};

type effect = unit => option(effectCleanup);

let commandEffect =
    (
      command: command,
      state: state,
      dispatch: actionDispatch,
      dispatchCompleted: dispatchCompleted,
    ): effect => {
    () => {
    switch (command, state) {
    | (Start(animationState), Idle(_)) =>
      dispatch(Start(animationState));
      None
    | (command, Animating(animationState))
        when command == Stop || animationState.t >= 1.0 =>
      dispatch(Stop);
      dispatchCompleted(animationState);
      None
    | (_, _) => None
    };
  }
};

let isAnimating = (state: state): bool => {
  switch (state) {
  | Animating(_) => true
  | _ => false
  };
}

let animatingEffect =
    (
      isAnimating: bool,
      dispatch: actionDispatch,
    )
    : effect => {
  () => {
    if (isAnimating) {
      Some(GroupedRaf.register((timerMs: float) => {dispatch(Frame(timerMs))}));
    } else {
      None
    };
  };
};

let logTransition = ((state: state, dispatch: event => unit)) => {
  let wrapped: event => unit =
    (event: event) => (
      {
        switch (event) {
        | Frame(_) => ()
        | event =>
          Js.log(
            "padding transition on event "
            ++ stringOfEvent(event)
            ++ " to state "
            ++ stringOfState(stateMachine(state, event)),
          )
        };
        dispatch(event);
      }: unit
    );
  (state, wrapped);
};

[@react.component]
let make =
    (~command: command, ~dispatchCompleted: dispatchCompleted, ~id: string) => {
  let (state, dispatch) =
    React.useReducer(stateMachine, Idle(0.0));
    //logTransition(React.useReducer(stateMachine, Idle(0.0)));

  let isAnimating = isAnimating(state)

  React.useEffect2(
    animatingEffect(isAnimating, dispatch),
    ((), isAnimating),
  );

  React.useEffect2(
    commandEffect(command, state, dispatch, dispatchCompleted),
    (command, state),
  );

  let state =
    switch (command, state) {
    | (Start(newState), Idle(_)) => Animating(newState)
    | (_, _) => state
    };

  let width = computeWidth(state);

  switch (state) {
  | Animating(a) when a.t < 0.0 => Js.log("t " ++ Js.Float.toString(a.t))
  | _ => ()
  };

  let widthStyle = paddingWidthStyle(width);
  let style = ReactDOMRe.Style.make(~width=widthStyle, ());
  <div
    key="infiniteSliderAnimationPadding"
    id
    className="infiniteSliderAnimationPadding"
    style
  />;
};