module Compiler.Types.Model (compileModel) where

import Compiler.Types
import Types

compileModel :: Root -> [Indent]
compileModel (Model name options) =
  [ Ln (name ++ "(updateCallback) {"),
  Br,
    Ind
      [ Ln "const reducer = (state, action) => state + 1;",
      Br,
        Ln "const result = [",
        Br,
        Ind
          [ Ln "0,",
          Br,
            Ln "(action) => {",
            Br,
            Ind
              [ Ln "const reducerResult = reducer(result[0], action)",
              Br,
                Ln "if (Object.is(reducerResult, result[0])) {",
                  Br,
                Ind [
                  Ln "result[0] = reducerResult",
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
        Ln "];",
        Br,
        Br,
        Ln "return result",
        Br
      ],
    Ln "}",
    Br
  ]
