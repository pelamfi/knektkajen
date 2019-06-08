let handleClick = () => Js.log("clicked!");

[@react.component]
let make = (~message) => {
  <div className="testCell" onClick={_event => handleClick()}>
    {ReasonReact.string(message)}
  </div>;
};