/*
 ThemeRepository.re
 */

type themeRepository = string => option(Theme.t);
type t = themeRepository;

let getTheme = (repository, name) => repository(name);

let ofTheme = (name, theme, s) =>
  switch (s) {
  | v when v == name => Some(theme)
  | _ => None
  };

let ofFilePath = (name: string, path: string) => {
  switch (Theme.from_file(path)) {
  | Ok(g) => ofTheme(name, g)
  | Error(_) => (_ => None)
  };
};

let create = v => v;
