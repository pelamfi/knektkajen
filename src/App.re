
let component = ReasonReact.statelessComponent("App");

let make = (_children) => {
  ...component,
  render: _ =>
    <Fragment>
      <div className="mainRow">
        <div className="left"/>
        <SvgView/>
        <div className="right"/>
      </div>
      <div className="bottomRow">
      <Component1 message="Click component and check console!" />
      <Component2 greeting="Hello!" />
      </div>
    </Fragment>,
};


