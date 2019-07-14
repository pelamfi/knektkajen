// https://stackoverflow.com/questions/49944067/join-array-of-strings
let commaSeparated = (items: list(string)): string => {
  switch(items) {
    | [] => ""
    | [single] =>  single
    | [head, ...tail] => List.fold_left((a, b) => {a ++ ", " ++ b}, head, tail)
  }
}
