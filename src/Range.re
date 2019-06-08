// Range with exclusive end
type range =
  | Range(int, int);

let map = (r: range, f: int => 'a): list('a) => {
  switch (r) {
  | Range(first, afterLast) =>
    let count = afterLast - first;
    Belt.List.fromArray(Belt.Array.makeBy(count, i => f(first + i)));
  // for (i in first to afterLast - 1) {}
  };
};

let make = (left: int, right: int): range => Range(left, right) /* let example: list(int) = Range(1, 10) |> map(_, i => i * 10*/;