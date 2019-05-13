
let component = ReasonReact.statelessComponent("RelativeNotesComponent");

let make = (_children) => {
  ...component,
  render: _ =>
    <Fragment>
      <div className="relativeNotesRow">
      <div className="noteCell">{ReasonReact.string("foo")}</div>
      <div className="noteCell">{ReasonReact.string("foo")}</div>
      </div>
    </Fragment>,
};
