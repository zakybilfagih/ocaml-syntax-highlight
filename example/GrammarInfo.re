type grammarLocation = {
  scopeName: string,
  path: string,
};

module StringMap =
  Map.Make({
    type t = string;
    let compare = String.compare;
  });
type t = StringMap.t(string);

let initial = StringMap.empty;

let getGrammarPathFromScope = (v: t, scope: string) => {
  StringMap.find_opt(scope, v);
};

let ofGrammarLocations = (locations: list(grammarLocation)) => {
  List.fold_left(
    (prev, curr) => {StringMap.add(curr.scopeName, curr.path, prev)},
    StringMap.empty,
    locations,
  );
};
