open Textmate;

type t = {
  useCache: bool,
  cache: Hashtbl.t(string, ThemeScopes.ResolvedStyle.t),
  theme: Textmate.TokenTheme.t,
};

let match = (v: t, scopes: string) =>
  if (!v.useCache) {
    Textmate.TokenTheme.match(v.theme, scopes);
  } else {
    switch (Hashtbl.find_opt(v.cache, scopes)) {
    | Some(v) => v
    | None =>
      let resolvedStyle = Textmate.TokenTheme.match(v.theme, scopes);
      Hashtbl.add(v.cache, scopes, resolvedStyle);
      resolvedStyle;
    };
  };

let create = (~useCache=true, theme: Textmate.TokenTheme.t) => {
  let cache = Hashtbl.create(1024);
  {cache, theme, useCache};
};
