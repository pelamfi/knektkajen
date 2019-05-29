
[@react.component]
let make = () => {
  <fragment>
    <div className="mainRow">
      <div className="left" />
      <SvgView/>
      <div className="right" />
    </div>
    <div className="bottomRow">
      <Component1 message="Click component and check console!" />
      <Component2 greeting="Hello!" />
    </div>
  </fragment>
};