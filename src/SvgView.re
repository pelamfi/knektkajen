let component = ReasonReact.statelessComponent("Component1");

let fontStyle = ReactDOMRe.Style.make(~fontFamily="roboto", ())

let make = (_children) => {
  ...component,
  render: _ =>
    <div className="svgView">
      <svg version="1.1" baseProfile="full" viewBox="-100 -100 200 200"/*width="80%" height="200" */ xmlns="http://www.w3.org/2000/svg">
        <rect width="100%" height="100%" fill="#FFFAFD" />
        <circle cx="0" cy="0" r="40" fill="#F699CB" />
        <text style=fontStyle x="0" y="20" fontSize="50" textAnchor="middle" fill="#FFFAFD">
        {ReasonReact.string("C#")}
        </text>
      </svg>
    </div>,
};
