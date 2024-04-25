type t = {
  nameToTheme: Hashtbl.t(string, Textmate.Theme.t),
  themeInfo: ThemeInfo.t,
  log: string => unit,
};

let create = (~log=_ => (), themeInfo) => {
  log,
  themeInfo,
  nameToTheme: Hashtbl.create(32),
};

let getTheme = (~themeName: string, gr: t) => {
  switch (Hashtbl.find_opt(gr.nameToTheme, themeName)) {
  | Some(v) => Some(v)
  | None =>
    switch (ThemeInfo.getThemePathFromName(gr.themeInfo, themeName)) {
    | Some(themePath) =>
      gr.log("Loading theme from: " ++ themePath);

      switch (Textmate.Theme.from_file(themePath)) {
      | Ok(theme) =>
        Hashtbl.add(gr.nameToTheme, themeName, theme);
        Some(theme);
      | Error(msg) =>
        gr.log("Theme failed to load " ++ msg);
        None;
      };

    | None => None
    }
  };
};
