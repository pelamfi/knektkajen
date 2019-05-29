type error;

// [@bs.module] external reactSwipeable : (string) => unit = "foo";

//[@bs.module] external reactSwipeable : unit => unit = "react-swipeable";


/*
import { useSwipeable, Swipeable } from 'react-swipeable';

function swipe() {
    return <h1>Hello, {props.name}</h1>;
    let config = {
        delta: 10,
        preventDefaultTouchmoveEvent: true,
        trackTouch: true,
        trackMouse: true,
        rotationAngle: 0,
    }
    function swipeEventHandler(eventData) {

    }
    let handlers = useSwipeable({ onSwiped: (eventData) => swipeEventHandler, ...config })
    console.log(handlers)
}
*/


[@bs.val] [@bs.module "react-swipeable"] external foo : string => array(string) = "useSwipeable";