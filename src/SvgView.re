let fontStyle = ReactDOMRe.Style.make(~fontFamily="roboto", ());

[@react.component]
let make = () => {
  <div className="svgView">
    <svg
      version="1.1"
      baseProfile="full"
      viewBox="-100 -100 200 200" /*width="80%" height="200" */
      xmlns="http://www.w3.org/2000/svg">
      <rect width="100%" height="100%" fill="#FFFAFD" />
      <circle id="questionCircle" cx="0" cy="0" r="26" fill="#F699CB" />
      <animate
        xlinkHref="#questionCircle"
        attributeName="cx"
        from="0"
        to_="100"
        dur="1s"
        begin_="click"
        fill="freeze"
      />
      <text
        alignmentBaseline="central"
        style=fontStyle
        x="0"
        y="0"
        fontSize="30"
        textAnchor="middle"
        fill="#FFFAFD">
        {ReasonReact.string("C#")}
      </text>
      <animate
        accumulate="sum"
        xlinkHref="#answerCircle"
        attributeName="cx"
        from="0"
        to_="10"
        dur="1s"
        begin_="click"
        fill="freeze"
      />
      <circle id="answerCircle" cx="-10" cy="-70" r="23" fill="#F699CB" />
    </svg>
  </div>;
};