type observer;

type callback = unit => unit;

let observeResize: (string, callback) => observer = [%bs.raw {|
function observeResize(selector, callback) {
    const target = document.querySelector(selector)
    console.log("observeResize selector: " + selector + " target: " + target)
    // https://drafts.csswg.org/resize-observer-1/
    // https://stackoverflow.com/questions/6492683/how-to-detect-divs-dimension-changed
    const ro = new ResizeObserver(callback);
    ro.observe(target)
    ro
}
|}];

let unobserve: (observer) => unit = [%bs.raw {|
function unobserveResize(observer) {
    observer.disconnect()
}
|}];
