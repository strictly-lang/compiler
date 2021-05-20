module Compiler.Types.Model (compileModel) where

import Compiler.Types
import Types

compileModel :: Root -> [Indent]
compileModel (Model name options) =
  [ Ln (name ++ " () {"),
    Ind
      [ Ln "return {",
        Ind
          [ Ln "state: 0,",
            Ln "dispatch: function() {}"
          ],
        Ln "}"
      ],
    Ln "}"
  ]
