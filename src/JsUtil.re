let fmod: (float, float) => float = [%bs.raw
  {|
function fmod(a, b) {
    return a % b
}
 |}
];