/*

 ThemeScopes.re
 */

/*
   [scope] is a list representation of TextMate scopes.
   For example, "source.js" would be represented as ["source", "js"]
 */

module Scope = {
  type t = list(string);

  let ofString = s => String.split_on_char('.', s);

  let toString = scope => String.concat(".", scope);

  let rec matches = (selector: t, v: t) => {
    switch (selector, v) {
    | ([], _) => true
    | ([selectorHd, ...selectorTl], [scopeHd, ...scopeTl]) =>
      if (String.equal(selectorHd, scopeHd)) {
        matches(selectorTl, scopeTl);
      } else {
        false;
      }
    | (_, []) => false
    };
  };
};

module Scopes = {
  /*
     In general, tokens have multiple scopes. For example,
     a token in a function might have the follow scopes:
     - "source.js"
     - "meta.definition.function.js"
     - "entity.name.function.js"

     The [t] type models this, as each bullet is a [scope].
   */
  type t = list(Scope.t);

  let ofString = s =>
    s
    |> String.split_on_char(' ')
    |> List.map(v => Scope.ofString(String.trim(v)));

  let toString = scopes =>
    String.concat("\n", scopes |> List.map(Scope.toString));

  let rec matches = (selector: t, v: t) => {
    switch (selector, v) {
    | ([], _) => true
    | ([selectorHd, ...selectorTl], [scopeHd, ...scopeTl]) =>
      if (Scope.matches(selectorHd, scopeHd)) {
        matches(selectorTl, scopeTl);
      } else {
        matches(selector, scopeTl);
      }
    | (_, []) => false
    };
  };
};

module ResolvedStyle = {
  type t = {
    foreground: string,
    background: string,
    bold: bool,
    italic: bool,
    underline: bool,
  };

  let default = (~foreground, ~background, ()) => {
    foreground,
    background,
    bold: false,
    italic: false,
    underline: false,
  };
};

module TokenStyle = {
  [@deriving show({with_path: false})]
  type t = {
    foreground: option(string),
    background: option(string),
    bold: option(bool),
    italic: option(bool),
    underline: option(bool),
  };

  let show = (v: t) => {
    switch (v.foreground) {
    | None => "Foreground: None"
    | Some(_) => "Foreground: Some"
    };
  };

  let merge = (prev, style) => {
    let foreground =
      switch (prev.foreground, style.foreground) {
      | (Some(v), _) => Some(v)
      | (_, Some(v)) => Some(v)
      | _ => None
      };

    let background =
      switch (prev.background, style.background) {
      | (Some(v), _) => Some(v)
      | (_, Some(v)) => Some(v)
      | _ => None
      };

    let bold =
      switch (prev.bold, style.bold) {
      | (Some(v), _) => Some(v)
      | (_, Some(v)) => Some(v)
      | _ => None
      };

    let italic =
      switch (prev.italic, style.italic) {
      | (Some(v), _) => Some(v)
      | (_, Some(v)) => Some(v)
      | _ => None
      };

    let underline =
      switch (prev.underline, style.underline) {
      | (Some(v), _) => Some(v)
      | (_, Some(v)) => Some(v)
      | _ => None
      };

    {background, foreground, bold, italic, underline};
  };

  let resolve = (~default: ResolvedStyle.t, style) => {
    let foreground =
      switch (style.foreground) {
      | Some(v) => v
      | None => default.foreground
      };

    let bold =
      switch (style.bold) {
      | Some(v) => v
      | None => default.bold
      };

    let italic =
      switch (style.italic) {
      | Some(v) => v
      | None => default.italic
      };

    let underline =
      switch (style.underline) {
      | Some(v) => v
      | None => default.underline
      };

    let background =
      switch (style.background) {
      | Some(v) => v
      | None => default.background
      };

    ResolvedStyle.{bold, italic, foreground, background, underline};
  };

  let create =
      (
        ~foreground: option(string)=None,
        ~background: option(string)=None,
        ~bold: option(bool)=None,
        ~italic: option(bool)=None,
        ~underline: option(bool)=None,
        (),
      ) => {
    foreground,
    background,
    bold,
    italic,
    underline,
  };

  let default = {
    foreground: None,
    background: None,
    bold: None,
    italic: None,
    underline: None,
  };
};

module Selector = {
  type t = {
    scopes: Scopes.t,
    style: TokenStyle.t,
  };

  let create = (~style=TokenStyle.default, ~scopes, ()) => {scopes, style};

  let matches = (selector: t, scopes: Scopes.t) =>
    Scopes.matches(selector.scopes, scopes);
};
