module Swap

import Prelude

%default total

main : (String, String) -> (String, String)
main (param, storage) = (storage, param)
