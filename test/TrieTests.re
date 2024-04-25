module Trie = Textmate.Trie;

let assertScope = (expected, value) =>
  Alcotest.(
    check(list(pair(string, option(int))), "same list", expected, value)
  );

let case = (title, fn) => Alcotest.test_case(title, `Quick, fn);

let tests = [
  (
    "Trie",
    [
      case("matches empty tree returns empty list", () => {
        let ret = Trie.matches(Trie.empty, ["scope"]);
        assertScope([], ret);
      }),
      case("single item matches", () => {
        let update = _ => Some(2);
        let trie = Trie.update(["scope"], update, Trie.empty);
        let ret = Trie.matches(trie, ["scope"]);
        assertScope([("scope", Some(2))], ret);
      }),
      case("multiple item matches", () => {
        let update1 = _ => Some(1);
        let update2 = _ => Some(2);

        let trie =
          Trie.empty
          |> Trie.update(["scope"], update1)
          |> Trie.update(["scope", "js"], update2);

        let ret = Trie.matches(trie, ["scope", "js"]);
        assertScope([("js", Some(2)), ("scope", Some(1))], ret);
      }),
      case("partial matches", () => {
        let update1 = _ => Some(1);
        let update2 = _ => Some(2);

        let trie =
          Trie.empty
          |> Trie.update(["scope"], update1)
          |> Trie.update(["scope", "js"], update2);

        let ret = Trie.matches(trie, ["scope", "js", "extra"]);
        assertScope([("js", Some(2)), ("scope", Some(1))], ret);
      }),
    ],
  ),
];
