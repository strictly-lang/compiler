module Compiler.Types.Model (compileModel) where

import Compiler.Types
import Types

compileModel :: Root -> [Indent]
compileModel (Model name options) =
  [ Ln (name ++ "(updateCallback) {"),
  Br,
    Ind
      [ Ln "const reducer = () => {};",
      Br,
        Ln "const result = {",
        Br,
        Ind
          [ Ln "state: 0,",
          Br,
            Ln "dispatch: (action) => {",
            Br,
            Ind
              [ Ln "const reducerResult = reducer(result.state, action)",
              Br,
                Ln "if (Object.is(reducerResult, result.state)) {",
                  Br,
                Ind [
                  Ln "result.state = reducerResult",
                  Br,
                  Ln "updateCallback();",
                  Br
                ],
                Ln "}",
                Br
              ],
            Ln "}",
            Br
          ],
        Ln "};",
        Br,
        Br,
        Ln "return result",
        Br
      ],
    Ln "}",
    Br
  ]
