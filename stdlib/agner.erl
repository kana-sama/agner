-module(agner).

-export([boxed_to_map/1, map_to_boxed/1]).
-primitive(boxed_to_map/1).
-primitive(map_to_boxed/1).

-export([put_char/1, put_str/1, println/1]).
-primitive(put_char/1).
-primitive(put_str/1).
-primitive(println/1).

-export([operator_not/1, operator_and/2]).
-primitive(operator_not/1).
-primitive(operator_and/2).

-export([operator_plus/2, operator_minus/2, operator_rem/2]).
-primitive(operator_plus/2).
-primitive(operator_minus/2).
-primitive(operator_rem/2).

-export([operator_plus_plus/2]).
-primitive(operator_plus_plus/2).

-export([operator_eq_eq/2, operator_less/2, operator_greater/2, operator_greater_eq/2, operator_eq_less/2]).
-primitive(operator_eq_eq/2).
-primitive(operator_less/2).
-primitive(operator_greater/2).
-primitive(operator_greater_eq/2).
-primitive(operator_eq_less/2).

-export([raise/2]).
-primitive(raise/2).
