let themePaths: list(ThemeInfo.themeLocation) = [
  {name: "rose-pine", path: "./tm-themes/themes/rose-pine.json"},
  {name: "rose-pine-dawn", path: "./tm-themes/themes/rose-pine-dawn.json"},
  {name: "github-light", path: "./tm-themes/themes/github-light.json"},
  {name: "github-dark", path: "./tm-themes/themes/github-dark.json"},
];

let themeRepository = {
  let info = ThemeInfo.ofThemeLocations(themePaths);
  let repo = ThemeRepository.create(info);
  Textmate.ThemeRepository.create(themeName =>
    ThemeRepository.getTheme(~themeName, repo)
  );
};

let grammarPaths: list(GrammarInfo.grammarLocation) = [
  {scopeName: "source.js", path: "./tm-grammars/grammars/javascript.json"},
  {scopeName: "source.ocaml", path: "./tm-grammars/grammars/ocaml.json"},
];

let grammarRepository = {
  let info = GrammarInfo.ofGrammarLocations(grammarPaths);
  let repo = GrammarRepository.create(info);
  Textmate.GrammarRepository.create(scope =>
    GrammarRepository.getGrammar(~scope, repo)
  );
};

module ThemeConfig = {
  type themeConfig = {
    variant: string,
    themeName: string,
    tokenTheme: TokenTheme.t,
  };

  type t = list(themeConfig);

  let create = (themeRepository, themes) => {
    List.map(
      ((variant, themeName)) => {
        let tokenTheme =
          Textmate.ThemeRepository.getTheme(themeRepository, themeName)
          |> Option.get
          |> Textmate.Theme.getTokenColors
          |> TokenTheme.create;

        {variant, themeName, tokenTheme};
      },
      themes,
    );
  };

  let getVariants = (v: t) => {
    List.map(({variant, _}) => variant, v);
  };
  let getThemeNames = (v: t) => {
    List.map(({themeName, _}) => themeName, v);
  };

  let getTokenTheme = (variant', v: t) => {
    List.find_opt(({variant, _}) => variant == variant', v)
    |> Option.map(({tokenTheme, _}) => tokenTheme);
  };
};

let themeConfig =
  ThemeConfig.create(
    themeRepository,
    [("dark", "rose-pine"), ("light", "github-light")],
  );

module C = Cmarkit_renderer.Context;

let render_attribute = (c, k, v) => {
  C.byte(c, ' ');
  C.string(c, k);
  C.string(c, "=\"");
  C.string(c, v);
  C.byte(c, '"');
};

let render_key_attribute = (c, k) => {
  C.byte(c, ' ');
  C.string(c, k);
};

let style_prefix = "--shiki";

let render_tokenized_lines = (c, lang) =>
  fun
  | [] => ()
  | [(l, _), ...ls] => {
      let l = l ++ "\n";
      let line_span = (c, l, tokens) => {
        C.string(c, "<span");
        render_key_attribute(c, "data-line");
        C.byte(c, '>');
        List.iter(
          (token: Textmate.Token.t) => {
            let token_str = String.sub(l, token.position, token.length);
            if (String.equal(String.trim(token_str), "")) {
              Cmarkit_html.html_escaped_string(c, token_str);
            } else {
              C.string(c, "<span");

              C.string(c, " style=\"");
              let scopes = String.concat(" ", token.scopes);
              List.iteri(
                (i, variant) => {
                  if (i != 0) {
                    C.byte(c, ';');
                  };
                  C.string(
                    c,
                    Printf.sprintf("%s-%s:", style_prefix, variant),
                  );
                  let tokenTheme =
                    ThemeConfig.getTokenTheme(variant, themeConfig)
                    |> Option.get;
                  let resolvedStyle = TokenTheme.match(tokenTheme, scopes);
                  C.string(c, resolvedStyle.foreground);

                  C.byte(c, ';');
                  C.string(
                    c,
                    Printf.sprintf(
                      "%s-%s-font-style:%s",
                      style_prefix,
                      variant,
                      resolvedStyle.italic ? "italic" : "inherit",
                    ),
                  );

                  C.byte(c, ';');
                  C.string(
                    c,
                    Printf.sprintf(
                      "%s-%s-font-weight:%s",
                      style_prefix,
                      variant,
                      resolvedStyle.bold ? "bold" : "inherit",
                    ),
                  );

                  C.byte(c, ';');
                  C.string(
                    c,
                    Printf.sprintf(
                      "%s-%s-text-decoration:%s",
                      style_prefix,
                      variant,
                      resolvedStyle.underline ? "underline" : "inherit",
                    ),
                  );
                },
                ThemeConfig.getVariants(themeConfig),
              );

              C.byte(c, '"');

              C.byte(c, '>');
              Cmarkit_html.html_escaped_string(c, token_str);
              C.string(c, "</span>");
            };
          },
          tokens,
        );
        C.string(c, "</span>");
      };

      let line = (c, l, tokens) => line_span(c, l, tokens);
      let tokenizer =
        Textmate.Tokenizer.create(~repository=grammarRepository, ());
      let rec go = (c, i, scopeStack) => (
        fun
        | [] => ()
        | [(l, _), ...ls] => {
            let l = l ++ "\n";
            let (tokens, scopeStack) =
              Textmate.Tokenizer.tokenize(
                ~lineNumber=i,
                ~scopeStack,
                ~scope="source." ++ lang,
                tokenizer,
                l,
              );

            line(c, l, tokens);
            go(c, i + 1, Some(scopeStack), ls);
          }
      );

      let (tokens, scopeStack) =
        Textmate.Tokenizer.tokenize(
          ~lineNumber=1,
          ~scopeStack=None,
          ~scope="source." ++ lang,
          tokenizer,
          l,
        );

      line_span(c, l, tokens);
      go(c, 2, Some(scopeStack), ls);
    };

let block_lines = c =>
  fun
  /* newlines only between lines */
  | [] => ()
  | [(l, _), ...ls] => {
      let line = (c, (l, _)) => {
        C.byte(c, '\n');
        C.string(c, l);
      };

      C.string(c, l);
      List.iter(line(c), ls);
    };

let custom_cb = (~backend_blocks, c, cb) => {
  let i = Option.map(fst, Cmarkit.Block.Code_block.info_string(cb));
  let lang = Option.bind(i, Cmarkit.Block.Code_block.language_of_info_string);
  switch (lang) {
  | Some((lang, _env)) when backend_blocks && lang.[0] == '=' =>
    if (lang == "=html" && !Cmarkit_html.safe(c)) {
      block_lines(c, Cmarkit.Block.Code_block.code(cb));
    } else {
      ();
    }
  | _ =>
    C.string(c, "<figure");
    render_key_attribute(c, "data-rehype-pretty-code-figure");
    C.byte(c, '>');
    C.string(c, "<pre");

    C.string(c, " style=\"");
    List.iteri(
      (i, variant) => {
        let tokenTheme =
          ThemeConfig.getTokenTheme(variant, themeConfig) |> Option.get;
        if (i != 0) {
          C.byte(c, ';');
        };
        C.string(c, Printf.sprintf("%s-%s-bg:", style_prefix, variant));
        C.string(c, tokenTheme.theme.defaultBackground);
        C.byte(c, ';');
        C.string(c, Printf.sprintf("%s-%s:", style_prefix, variant));
        C.string(c, tokenTheme.theme.defaultForeground);
      },
      ThemeConfig.getVariants(themeConfig),
    );

    C.byte(c, '"');

    Option.iter(
      ((lang, _)) => render_attribute(c, "data-language", lang),
      lang,
    );

    let themeNames =
      ThemeConfig.getThemeNames(themeConfig) |> String.concat(" ");
    render_attribute(c, "data-theme", themeNames);

    C.byte(c, '>');
    C.string(c, "<code");

    render_attribute(c, "data-theme", themeNames);

    Option.iter(
      ((lang, _env)) => render_attribute(c, "data-language", lang),
      lang,
    );

    C.byte(c, '>');
    Option.iter(
      ((lang, _env)) =>
        render_tokenized_lines(c, lang, Cmarkit.Block.Code_block.code(cb)),
      lang,
    );
    C.string(c, "</code></pre></figure>\n");
  };
};

let custom_html = (~backend_blocks) => {
  let block = c =>
    fun
    | Cmarkit.Block.Code_block((cb, _)) => {
        custom_cb(~backend_blocks, c, cb);
        true;
      }
    | _ => false;

  Cmarkit_renderer.make(~block, ());
};

let custom_html_of_doc = (~safe, ~backend_blocks=false, doc) => {
  let default = Cmarkit_html.renderer(~safe, ~backend_blocks, ());
  let r = Cmarkit_renderer.compose(default, custom_html(~backend_blocks));
  Cmarkit_renderer.doc_to_string(r, doc);
};

let read_file = file => In_channel.with_open_bin(file, In_channel.input_all);

let write_file = (file, s) =>
  Out_channel.with_open_bin(file, oc => Out_channel.output_string(oc, s));

let write_content = content => {
  let content_str =
    Printf.sprintf(
      {|<!DOCTYPE html>
  <html lang="en">
    <head>
      <link rel="stylesheet" href="./index.css" />
      <script>
        function preferDarkTheme() {
          if (window.matchMedia) {
            return !!window.matchMedia('(prefers-color-scheme: dark)').matches
          }
          return false;
        }

        function toggleTheme() {
          let root = document.documentElement
          root.classList.toggle('dark')
        }

        (() => {
          if (preferDarkTheme()) {
            toggleTheme()
          }
        })()
      </script>
    </head>
    <body>
    %s
    </body>
  </html>
  |},
      content,
    );

  write_file("index.html", content_str);
};

let _ =
  read_file("test.md")
  |> Cmarkit.Doc.of_string
  |> custom_html_of_doc(~safe=false)
  |> write_content;
