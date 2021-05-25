module Compiler.Types.Model (compileModel) where

import Compiler.Types
import Compiler.Util (functionToJs)
import Data.List (intersperse)
import Types

compileModel :: Root -> [Indent]
compileModel (Model name options) =
  [ Ln (name ++ "(updateCallback) {"),
    Br,
    Ind
      ( ( concat
            [Ln ("const __" ++ optionName ++ " = ") : functionToJs [] optionValue ++ [Br] | (optionName, optionValue) <- options]
        )
          ++ [ Br,
               Ln "const result = [",
               Br,
               Ind
                 [ Ln "__init(),",
                   Br,
                   Ln "(action) => {",
                   Br,
                   Ind
                     [ Ln "const reducerResult = __reducer(result[0], action)",
                       Br,
                       Ln "if (Object.is(reducerResult, result[0]) === false) {",
                       Br,
                       Ind
                         [ Ln "result[0] = reducerResult",
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
             ]
      ),
    Ln "}",
    Br
  ]
