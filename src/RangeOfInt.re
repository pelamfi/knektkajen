// Range with exclusive end
type rangeOfInt =
  | Range(int, int);

let map = (f: int => 'a, r: rangeOfInt): list('a) => {
  switch (r) {
  | Range(first, afterLast) =>
    let count = afterLast - first;
    Belt.List.fromArray(Belt.Array.makeBy(count, i => f(first + i)));
  // for (i in first to afterLast - 1) {}
  };
};

let size = (r: rangeOfInt) => {
  let Range(left, right) = r;
  right - left;
};

let drop = (count: int, r: rangeOfInt): rangeOfInt => {
  let Range(left, right) = r;
  if (size(r) > count) {
    Range(left + count, right);
  } else {
    Range(right, right);
  };
};

let dropRight = (count: int, r: rangeOfInt): rangeOfInt => {
  let Range(left, right) = r;
  if (size(r) > count) {
    Range(left, right - count);
  } else {
    Range(left, left);
  };
};


let make = (left: int, right: int): rangeOfInt => Range(left, right) /* let example: list(int) = make(1, 10) |> map(i => i * 10*/;