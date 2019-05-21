let asReact = (elementList: list(_)): React.element =>
  ReasonReact.array(Belt.List.toArray(elementList));