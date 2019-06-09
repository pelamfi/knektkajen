// Range with exclusive end
type range_of_int =
  | Range(int, int);

let map = (r: range_of_int, f: int => 'a): list('a) => {
  switch (r) {
  | Range(first, afterLast) =>
    let count = afterLast - first;
    Belt.List.fromArray(Belt.Array.makeBy(count, i => f(first + i)));
  // for (i in first to afterLast - 1) {}
  };
};

let make = (left: int, right: int): range_of_int => Range(left, right) /* let example: list(int) = Range(1, 10) |> map(_, i => i * 10*/;