type t = {
  scopeToGrammar: Hashtbl.t(string, Textmate.Grammar.t),
  grammarInfo: GrammarInfo.t,
  log: string => unit,
};

let create = (~log=_ => (), grammarInfo) => {
  log,
  grammarInfo,
  scopeToGrammar: Hashtbl.create(32),
};

let getGrammar = (~scope: string, gr: t) => {
  switch (Hashtbl.find_opt(gr.scopeToGrammar, scope)) {
  | Some(v) => Some(v)
  | None =>
    switch (GrammarInfo.getGrammarPathFromScope(gr.grammarInfo, scope)) {
    | Some(grammarPath) =>
      gr.log("Loading grammar from: " ++ grammarPath);

      switch (Textmate.Utility.JsonEx.from_file(grammarPath)) {
      | Ok(json) =>
        switch (Textmate.Grammar.Json.of_yojson(json)) {
        | Ok(grammar) =>
          gr.log("JSON Grammar loaded successfully");
          Hashtbl.add(gr.scopeToGrammar, scope, grammar);
          Some(grammar);

        | Error(e) =>
          gr.log("Grammar loading failed with: " ++ e);
          None;
        }

      | Error(msg) =>
        gr.log("JSON Grammar failed to load, falling back to XML: " ++ msg);
        switch (Textmate.Grammar.Xml.of_file(grammarPath)) {
        | Ok(grammar) =>
          gr.log("XML Grammar loaded successfully");
          Hashtbl.add(gr.scopeToGrammar, scope, grammar);
          Some(grammar);

        | Error(e) =>
          gr.log("Grammar loading failed with: " ++ e);
          None;
        };
      };

    | None => None
    }
  };
};
