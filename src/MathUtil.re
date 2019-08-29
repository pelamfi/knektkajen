
// https://www.wikiwand.com/en/Modulo_operation
let flooredDivisionRemainder = (a: int, b: int): int => {
  (a mod b + b) mod b
};

// NOTE: This is not properly defined floored division if b < 0
let flooredDivision = (a: int, b: int): int => {
  if (a < 0) {
    (a - (b - 1)) / b
  } else {
    a / b
  }
};

let stringOfIntWithSign = (a: int): string => {
  if (a < 0) {
    string_of_int(a)
  } else if (a > 0){
    "+" ++ string_of_int(a)
  } else {
    "0"
  }
}