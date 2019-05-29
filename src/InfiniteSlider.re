open ReactUtil
open Belt.List

type componentFactory = (int, int) => reactComponent;

type animatingWithQueue = {fromIndex: int, toIndex: int, toIndexQueued: int};
type animating = {fromIndex: int, toIndex: int};

type slideState = 
  | Idle 
  | Animating(animating) // CSS animation slide and a timer running
  | AnimatingWithQueue(animatingWithQueue) // if another prop change comes while we animate
;

type state = {
    current: int, 
    timeoutId: option(Js.Global.timeoutId),
    slideState};

let string_of_slide_state = (state: slideState): string => {
    switch(state) {
      | Idle => "Idle"
      | Animating({fromIndex, toIndex}) => "Animating("++ string_of_int(fromIndex) ++ ", " ++ string_of_int(toIndex) ++ ")"
      | AnimatingWithQueue({fromIndex, toIndex, toIndexQueued}) => "AnimatingWithQueue("++ string_of_int(fromIndex) ++ ", " ++ string_of_int(toIndex) ++ ", " ++ string_of_int(toIndexQueued) ++ ")"
    }
}

let string_of_state = (state: state): string => {
    "[current:" ++ string_of_int(state.current) ++ " slideState: " ++ string_of_slide_state(state.slideState) ++ "]"
}

type event = 
| AnimationComplete
| AnimationStart(int, int, Js.Global.timeoutId)

let string_of_event = (event: event): string => {
    switch(event) {
        | AnimationComplete => "AnimationComplete"
        | AnimationStart(from, toIndex, _) => "AnimationStart(" ++ string_of_int(from) ++ ", " ++ string_of_int(toIndex) ++ ")"
    }
}

type config = {
    componentFactory: componentFactory,
    styleBaseName: string,
    itemsWindow: Range.range, // current is at 0
    maxJump: int
};

let paddingCount = (state: state, maxJump: int): int => {
    switch(state.slideState) {
        | Idle => 1 + maxJump
        | Animating({fromIndex, toIndex}) | AnimatingWithQueue({fromIndex, toIndex}) => min(max(1, (1 + maxJump) - (toIndex - fromIndex)), 1 + 2 * maxJump)
    }
}

let elems = (state: state, config: config): list(reactComponent) => {
    let offset = switch(state.slideState) {
        | Idle => state.current
        | Animating({fromIndex}) | AnimatingWithQueue({fromIndex}) => fromIndex
    }
    config.itemsWindow |> Range.map(_, i => config.componentFactory(i + offset, state.current));
};

[@react.component]
let make = (~config: config, ~current: int) => {
    let rowClassName = config.styleBaseName ++ "Row";   

    <div>{ReasonReact.string("FIXME")}</div>
    /*
    {

        initialState: (): state => {current: 0, timeoutId: None, slideState: Idle},
        reducer: (event: event, state: state) => {
            // ReactSwipeable.foo("foo");
            let newState = switch(event) {
                | AnimationComplete => 
                state.timeoutId
                |> Belt.Option.map(_, Js.Global.clearTimeout)
                |> ignore;
                switch (state.slideState) {
                    | Idle => ReasonReact.NoUpdate
                    | Animating(_) => ReasonReact.Update({...state, timeoutId: None, slideState: Idle} )
                    | AnimatingWithQueue({toIndex, toIndexQueued}) => ReasonReact.UpdateWithSideEffects({...state, timeoutId: None, slideState: Idle}, (self): unit => {
                        self.send(AnimationStart(
                            toIndex,
                            toIndexQueued,
                            Js.Global.setTimeout(
                                () => {
                                self.send(AnimationComplete);
                                },
                                333,
                            )));
                        ()
                    })
                }
                | AnimationStart(fromIndex, toIndex, timeoutId) =>
                    ReasonReact.Update({...state, timeoutId: Some(timeoutId), slideState: Animating({fromIndex, toIndex})} )
            };
            let newStateString = switch (newState) {
                | ReasonReact.NoUpdate => "no update"
                | ReasonReact.Update(state) => string_of_state(state)
                | ReasonReact.UpdateWithSideEffects(state, _) => string_of_state(state) ++ " with side effects"
                | ReasonReact.SideEffects(_) => "side effects"
            }
            Js.log("state " ++ string_of_state(state) ++ "->" ++ newStateString ++ " on " ++ string_of_event(event) )
            newState
        },
        retainedProps: current,
        willReceiveProps: (self) => {
            let newState = if (self.retainedProps !== current) {
                switch self.state.slideState {
                    | Idle =>
                        let timeoutId = 
                            Js.Global.setTimeout(
                                () => {
                                self.send(AnimationComplete);
                                },
                                333,
                            );
                        {current: current, timeoutId: Some(timeoutId), 
                            slideState: Animating({fromIndex: self.state.current, toIndex: current})}
                    | Animating(animState) =>
                        {...self.state, current: current, slideState: AnimatingWithQueue({fromIndex: animState.fromIndex, toIndex: animState.toIndex, toIndexQueued: current})}
                    | AnimatingWithQueue(animState) =>
                        {...self.state, current: current, slideState: AnimatingWithQueue({...animState, toIndexQueued: current})}
                }
            } else {
                self.state
            }

            Js.log("state by props " ++ string_of_state(self.state) ++ "->" ++ string_of_state(newState) ++ string_of_int(self.retainedProps) ++ " " ++ string_of_int(current));

            newState
        },
        render: self => {
            let paddingAnimClass = switch(self.state.slideState) {
                | Animating(_) | AnimatingWithQueue(_)=> " " ++ config.styleBaseName ++ "Padding-anim"
                | Idle => ""
            }
            let paddingClass = config.styleBaseName ++ "Padding-" ++ string_of_int(paddingCount(self.state, config.maxJump))
             ++ paddingAnimClass;
            let e: list(reactComponent) = elems(self.state, config);
            Js.log("paddingclass " ++ paddingClass);
            <div className="infiniteSlider">
                <div className={rowClassName}>
                    <div className={paddingClass}/>
                    {asReact(e)}
                </div>
            </div>
        },
    }
    */
};
