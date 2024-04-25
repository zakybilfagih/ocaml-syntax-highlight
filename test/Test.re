let () =
  Alcotest.run(
    ~show_errors=true,
    ~tail_errors=`Unlimited,
    "TextMate",
    List.flatten([
      TrieTests.tests,
      TokenThemeTests.tests,
      FixtureTests.tests,
    ]),
  );
