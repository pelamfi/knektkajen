
let component = ReasonReact.statelessComponent("App");

let make = (_children) => {
  ...component,
  render: _ =>
    <Fragment>
      <Component1 message="Click component and check console!" />
      <Component2 greeting="Hello!" />
    </Fragment>,
};


