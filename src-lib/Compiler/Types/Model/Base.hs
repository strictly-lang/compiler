module Compiler.Types.Model.Base (compileModel) where

import Compiler.Types
import Compiler.Util (functionToJs)
import Data.List (intersperse)
import Types

compileModel :: Root -> VariableStack -> AppStateMonad [Indent]
compileModel (Model name options) variableStack =
  do
    let hasGnerator = any (\(_, mergedOption) -> any fst mergedOption) options
    functionJs <-
      mapM
        ( \(optionName, optionValue) ->
            do
              (functionJs, _) <- functionToJs variableStack (map snd optionValue)
              return (Ln ("const __" ++ optionName ++ " = ") : functionJs ++ [Br])
        )
        options

    return
      [ Ln (name ++ "(updateCallback, ...args) {"),
        Br,
        Ind
          ( concat
              functionJs
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
                                 Ln "result[1](await iterableResult.value);",
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
