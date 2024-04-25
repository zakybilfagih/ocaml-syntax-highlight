module TokenTheme = Textmate.TokenTheme;
module Scope = Textmate.ThemeScopes.Scope;
module Selector = Textmate.ThemeScopes.Selector;
module ResolvedStyle = Textmate.ThemeScopes.ResolvedStyle;
module TokenStyle = Textmate.ThemeScopes.TokenStyle;

let case = (title, fn) => Alcotest.test_case(title, `Quick, fn);

let assertString = (expected, value) => {
  Alcotest.(check(string, "same string", expected, value));
};

let assertTrue = value => {
  Alcotest.(check(bool, "to be true", true, value));
};

let assertFalse = value => {
  Alcotest.(check(bool, "to be false", false, value));
};

let simpleTokenTheme =
  TokenTheme.create(
    ~defaultBackground="#000",
    ~defaultForeground="#fff",
    [
      ("var", TokenStyle.create(~foreground=Some("#92D3CA"), ())),
      (
        "var.identifier",
        TokenStyle.create(
          ~foreground=Some("#007fff"),
          ~bold=Some(true),
          (),
        ),
      ),
      (
        "constant",
        TokenStyle.create(
          ~foreground=Some("#00FFFF"),
          ~italic=Some(true),
          (),
        ),
      ),
      (
        "constant.numeric",
        TokenStyle.create(~foreground=Some("#990000"), ()),
      ),
      ("constant.numeric.hex", TokenStyle.create(~bold=Some(true), ())),
      ("foo, bar", TokenStyle.create(~foreground=Some("lavender"), ())),
      ("entity", TokenStyle.create(~bold=Some(true), ())),
      (
        "entity.other.attribute-name.foo,entity.other.attribute-name.bar",
        TokenStyle.create(~foreground=Some("salmon"), ()),
      ),
      ("html", TokenStyle.create(~foreground=Some("slateGray"), ())),
      ("meta html", TokenStyle.create(~foreground=Some("smoke"), ())),
      (
        "source.php string",
        TokenStyle.create(~foreground=Some("peachPuff"), ()),
      ),
      (
        "text.html source.php",
        TokenStyle.create(~foreground=Some("navy"), ()),
      ),
      (
        "text.html source.js",
        TokenStyle.create(
          ~foreground=Some("navy"),
          ~background=Some("cornflowerBlue"),
          (),
        ),
      ),
    ],
  );

let test_match = (
  "Token theme match",
  [
    /* Test theme inspired by:
          https://code.visualstudio.com/blogs/2017/02/08/syntax-highlighting-optimizations#_finally-whats-new-in-vs-code-19
       */
    case("superfluous styles should still match", () => {
      let style: ResolvedStyle.t =
        TokenTheme.match(
          simpleTokenTheme,
          "constant.numeric.meta.js source.js",
        );

      assertString("#990000", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertTrue(style.italic);
    }),
    case("unmatched style should pass-through", () => {
      let style: ResolvedStyle.t =
        TokenTheme.match(
          simpleTokenTheme,
          "some-unmatched-style constant.numeric.meta.js source.js",
        );

      assertString("#990000", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertTrue(style.italic);
    }),
    case("background color should be picked up", () => {
      let style: ResolvedStyle.t =
        TokenTheme.match(simpleTokenTheme, "source.js text.html.basic");

      assertString("navy", style.foreground);
      assertString("cornflowerBlue", style.background);
      assertFalse(style.bold);
      assertFalse(style.italic);
    }),
    case(
      "deeper rule should win (source.php string over text.html source.php)",
      () => {
      let style: ResolvedStyle.t =
        TokenTheme.match(
          simpleTokenTheme,
          "string.quoted source.php.embedded.html text.html.basic",
        );

      assertString("peachPuff", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertFalse(style.italic);
    }),
    case("parent rule (meta html) gets applied", () => {
      let style: ResolvedStyle.t =
        TokenTheme.match(simpleTokenTheme, "html meta");

      assertString("smoke", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertFalse(style.italic);

      let style: ResolvedStyle.t =
        TokenTheme.match(simpleTokenTheme, "html meta.source.js");

      assertString("smoke", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertFalse(style.italic);

      let style: ResolvedStyle.t = TokenTheme.match(simpleTokenTheme, "html");

      assertString("slateGray", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertFalse(style.italic);
    }),
    case("parent rule gets ignored", () => {
      let style: ResolvedStyle.t =
        TokenTheme.match(simpleTokenTheme, "meta foo");

      assertString("lavender", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertFalse(style.italic);
    }),
    case("foo & bar gets correctly style (compound rule)", () => {
      let style: ResolvedStyle.t = TokenTheme.match(simpleTokenTheme, "foo");

      assertString("lavender", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertFalse(style.italic);

      let style: ResolvedStyle.t = TokenTheme.match(simpleTokenTheme, "bar");

      assertString("lavender", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertFalse(style.italic);
    }),
    case(
      "entity.other.attribute-name.foo & bar gets correctly style (more interesting compound rule)",
      () => {
        let style: ResolvedStyle.t =
          TokenTheme.match(
            simpleTokenTheme,
            "entity.other.attribute-name.foo",
          );

        assertString("salmon", style.foreground);
        assertString("#000", style.background);
        assertTrue(style.bold);
        assertFalse(style.italic);

        let style: ResolvedStyle.t =
          TokenTheme.match(
            simpleTokenTheme,
            "entity.other.attribute-name.bar",
          );

        assertString("salmon", style.foreground);
        assertString("#000", style.background);
        assertTrue(style.bold);
        assertFalse(style.italic);
      },
    ),
    case("baz gets default style (no match)", () => {
      let style: ResolvedStyle.t = TokenTheme.match(simpleTokenTheme, "baz");

      assertString("#fff", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertFalse(style.italic);
    }),
    case("var gets correct style", () => {
      let style: ResolvedStyle.t = TokenTheme.match(simpleTokenTheme, "var");

      assertString("#92D3CA", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertFalse(style.italic);
    }),
    case("var.baz gets correct style (should match var)", () => {
      let style: ResolvedStyle.t =
        TokenTheme.match(simpleTokenTheme, "var.baz");

      assertString("#92D3CA", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertFalse(style.italic);
    }),
    case("var.identifier gets correct style", () => {
      let style: ResolvedStyle.t =
        TokenTheme.match(simpleTokenTheme, "var.identifier");

      assertString("#007fff", style.foreground);
      assertString("#000", style.background);
      assertTrue(style.bold);
      assertFalse(style.italic);
    }),
    case("constant gets correct style", () => {
      let style: ResolvedStyle.t =
        TokenTheme.match(simpleTokenTheme, "constant");

      assertString("#00FFFF", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertTrue(style.italic);
    }),
    case("constant.numeric gets correct style", () => {
      let style: ResolvedStyle.t =
        TokenTheme.match(simpleTokenTheme, "constant.numeric");

      assertString("#990000", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertTrue(style.italic);
    }),
    case("constant.numeric.hex gets correct style", () => {
      let style: ResolvedStyle.t =
        TokenTheme.match(simpleTokenTheme, "constant.numeric.hex");

      assertString("#990000", style.foreground);
      assertString("#000", style.background);
      assertTrue(style.bold);
      assertTrue(style.italic);
    }),
  ],
);

let test_of_yojson = (
  "Token theme of yojson",
  [
    case("empty array parses", () => {
      let json = Yojson.Safe.from_string("[]");
      let _ =
        TokenTheme.of_yojson(
          ~defaultForeground="#fff",
          ~defaultBackground="#000",
          json,
        );
      assertTrue(true);
    }),
    case("resolves to default colors if no match", () => {
      let json = Yojson.Safe.from_string("[]");
      let theme =
        TokenTheme.of_yojson(
          ~defaultForeground="#fff",
          ~defaultBackground="#000",
          json,
        );

      let style: ResolvedStyle.t =
        TokenTheme.match(theme, "constant.numeric.hex");
      assertString("#fff", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertFalse(style.italic);
    }),
    case("simple match", () => {
      let json =
        Yojson.Safe.from_string(
          {|
       [{
        "name": "Text",
        "scope": "constant.numeric.hex",
        "settings": {
            "foreground": "#bbbbbb",
            "fontStyle": "italic"
        }
       },
       {
        "name": "Oct text",
        "scope": "constant.numeric.oct",
        "settings": {
            "foreground": "#0f0",
            "background": "#f00",
            "fontStyle": "bold"
        }
       }]
      |},
        );

      let theme =
        TokenTheme.of_yojson(
          ~defaultForeground="#fff",
          ~defaultBackground="#000",
          json,
        );

      let style: ResolvedStyle.t =
        TokenTheme.match(theme, "constant.numeric.hex");

      assertString("#bbbbbb", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertTrue(style.italic);

      let style: ResolvedStyle.t =
        TokenTheme.match(theme, "constant.numeric.oct");

      assertString("#0f0", style.foreground);
      assertString("#f00", style.background);
      assertTrue(style.bold);
      assertFalse(style.italic);
    }),
    case("compound selector", () => {
      let json =
        Yojson.Safe.from_string(
          {|
       [{
        "name": "Text",
        "scope": "constant.numeric.hex, constant.numeric.oct",
        "settings": {
            "foreground": "#bbbbbb",
            "fontStyle": "italic"
        }
       }]
      |},
        );

      let theme =
        TokenTheme.of_yojson(
          ~defaultForeground="#fff",
          ~defaultBackground="#000",
          json,
        );

      let style: ResolvedStyle.t =
        TokenTheme.match(theme, "constant.numeric.hex");

      assertString("#bbbbbb", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertTrue(style.italic);

      let style: ResolvedStyle.t =
        TokenTheme.match(theme, "constant.numeric.oct");

      assertString("#bbbbbb", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertTrue(style.italic);
    }),
    case("compound array selector", () => {
      let json =
        Yojson.Safe.from_string(
          {|
       [{
        "name": "Text",
        "scope": ["constant.numeric.hex", "constant.numeric.oct"],
        "settings": {
            "foreground": "#bbbbbb",
            "fontStyle": "italic"
        }
       }]
      |},
        );

      let theme =
        TokenTheme.of_yojson(
          ~defaultForeground="#fff",
          ~defaultBackground="#000",
          json,
        );

      let style: ResolvedStyle.t =
        TokenTheme.match(theme, "constant.numeric.hex");

      assertString("#bbbbbb", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertTrue(style.italic);

      let style: ResolvedStyle.t =
        TokenTheme.match(theme, "constant.numeric.oct");
      assertString("#bbbbbb", style.foreground);
      assertString("#000", style.background);
      assertFalse(style.bold);
      assertTrue(style.italic);
    }),
    case("dracula: source should not override all styles", () => {
      let json =
        Yojson.Safe.from_string(
          {|
       [
        {
            "scope": [
                "source"
            ],
            "settings": {
                "foreground": "#FF0000"
            }
        },
        {
            "name": "Built-in functions / properties",
            "scope": [
                "support.function",
                "support.type.property-name"
            ],
            "settings": {
                "fontStyle": "regular",
                "foreground": "#AAAAAA"
            }
        }
       ]
      |},
        );
      let theme =
        TokenTheme.of_yojson(
          ~defaultForeground="#fff",
          ~defaultBackground="#000",
          json,
        );

      // Just support.function.console.js should resolve to the support.function
      let style: ResolvedStyle.t =
        TokenTheme.match(theme, "support.function.console.js");
      assertString("#AAAAAA", style.foreground);

      //...and introducing source.js should still be the same, since it is less specific
      let style: ResolvedStyle.t =
        TokenTheme.match(theme, "support.function.console.js source.js");
      assertString("#AAAAAA", style.foreground);
    }),
  ],
);

let tests = [test_match, test_of_yojson];
