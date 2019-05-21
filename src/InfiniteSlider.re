open ReactUtil
open Belt.List

type componentFactory = (int, int) => React.element;

type state = {current: int};

type event = SetCurrent(int);

type config = {
    componentFactory: componentFactory,
    styleBaseName: string,
    itemsWindow: Range.range, // current is at 0
    maxJump: int
};

let component = ReasonReact.reducerComponent("InfiniteSlider");

let make = (~config: config, ~current: int, _children) => {
    let paddingCount = min(max(1, (1 + config.maxJump) - current), 1 + 2 * config.maxJump);
    let paddingClass = config.styleBaseName ++ "Padding-" ++ string_of_int(paddingCount);
    let rowClassName = config.styleBaseName ++ "Row";
    // let elems = config.itemsWindow |> Range.map(_, i => config.componentFactory(i + current, current));
    let elems = config.itemsWindow |> Range.map(_, i => config.componentFactory(i, current));
    {
        ...component,

        initialState: (): state => {current: 0},
        reducer: (event: event, state: state) => {
            switch(event) {
                | SetCurrent(newCurrent) => ReasonReact.Update({current: newCurrent})
            }
        },
        
        render: self => {
            <Fragment>
            <div className={rowClassName}>
                <div className={paddingClass}/>
                {asReact(elems)}
            </div>
            </Fragment>
        },
    }
}
;
