// Add fragment support with snippet found in here https://reasonml.chat/t/how-to-use-react-fragement/660/2

[@bs.module "react"] external fragment : ReasonReact.reactClass = "Fragment";

let make = children => ReasonReact.wrapJsForReason(~reactClass=fragment, ~props=Js.Obj.empty(), children);
