open Belt.Set

let setToggle =
    (set: t('a, 'b), key: 'a): t('a, 'b) =>
  if (has(set, key)) {
    remove(set, key);
  } else {
    add(set, key);
  };
