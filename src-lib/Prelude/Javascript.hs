module Prelude.Javascript where

import Prelude.Types

webcomponent :: Macro
webcomponent ast = show ast

macros = [webcomponent]