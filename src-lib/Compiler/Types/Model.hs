module Compiler.Types.Model (compileModel) where

import Compiler.Types
import Compiler.Util (functionToJs)
import Data.List (intersperse)
import Types

compileModel :: Root -> VariableStack -> [Indent]
compileModel (Model name options) variableStack =
  let hasGnerator = any (\(_, mergedOption) -> any fst mergedOption) options
   in [ Ln (name ++ "(updateCallback, ...args) {"),
        Br,
        Ind
          ( ( concat
                [Ln ("const __" ++ optionName ++ " = ") : functionToJs variableStack (map snd optionValue) ++ [Br] | (optionName, optionValue) <- options]
            )
              ++ ( if hasGnerator
                     then
                       [ Ln "const iterable = __init(...args);",
                         Br,
                         Ln "let { value, done } = iterable.next();",
                         Br
                       ]
                     else []
                 )
              ++ [ Br,
                   Ln "const result = [",
                   Br,
                   Ind
                     [ if hasGnerator
                         then Ln "value,"
                         else Ln "__init(...args),",
                       Br,
                       Ln "(action) => {",
                       Br,
                       Ind
                         [ Ln "const reducerResult = __reducer(result[0], action)",
                           Br,
                           Ln "if (Object.is(reducerResult, result[0]) === false) {",
                           Br,
                           Ind
                             [ Ln "result[0] = reducerResult;",
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
                   Br
                 ]
              ++ ( if hasGnerator
                     then
                       [ Ln "(async () => {",
                         Br,
                         Ind
                           [ Ln "while (done === false) {",
                             Br,
                             Ind
                               [ Ln "const iterableResult = iterable.next();",
                                 Br,
                                 Ln "done = iterableResult.done;",
                                 Br,
                                 Ln "result[0] = await iterableResult.value;",
                                 Br,
                                 Ln "updateCallback();",
                                 Br
                               ],
                             Br,
                             Ln "}",
                             Br
                           ],
                         Ln "})();",
                         Br
                       ]
                     else []
                 )
              ++ [ Br,
                   Ln
                     "return result;",
                   Br
                 ]
          ),
        Ln "}",
        Br
      ]
