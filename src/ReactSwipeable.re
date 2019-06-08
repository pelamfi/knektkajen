type error;

// [@bs.module] external reactSwipeable : (string) => unit = "foo";

//[@bs.module] external reactSwipeable : unit => unit = "react-swipeable";

/*
 import { useSwipeable, Swipeable } from 'react-swipeable';

 */
let swipeTest = [%bs.raw {|
function swipeTest() {
    const ReactSwipeable = require("react-swipeable");
     let config = {
         delta: 10,
         preventDefaultTouchmoveEvent: true,
         trackTouch: true,
         trackMouse: true,
         rotationAngle: 0,
     }
     function swipeEventHandler(eventData) {
         console.log(handlers)
     }
     let handlers = ReactSwipeable.useSwipeable({ onSwiped: (eventData) => swipeEventHandler, ...config })
     console.log(handlers)
 }
 |}];
 
[@bs.val] [@bs.module "react-swipeable"]

external useSwipeableInternal: string => array(string) = "useSwipeable";
