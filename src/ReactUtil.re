type reactComponent2 =
  ReasonReact.componentSpec(
    ReasonReact.stateless,
    ReasonReact.stateless,
    ReasonReact.noRetainedProps,
    ReasonReact.noRetainedProps,
    ReasonReact.actionless,
  );

type reactComponent = ReasonReact.reactElement;

let asReact = (elementList: list(reactComponent)): ReasonReact.reactElement =>
  ReasonReact.array(Belt.List.toArray(elementList));

let emptyFragment: reactComponent = asReact([])

let emptyStyle: ReactDOMRe.Style.t = ReactDOMRe.Style.make(())