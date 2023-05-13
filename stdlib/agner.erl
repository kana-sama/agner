-module(agner).

-export([boxed_to_map/1, map_to_boxed/1]).
-primitive(boxed_to_map/1).
-primitive(map_to_boxed/1).

-export([put_char/1, put_str/1, println/1]).
-primitive(put_char/1).
-primitive(put_str/1).
-primitive(println/1).

-export([not/1, and/2]).
-primitive(not/1).
-primitive(and/2).

-export([plus/2, minus/2, rem/2]).
-primitive(plus/2).
-primitive(minus/2).
-primitive(rem/2).

-export([plus_plus/2]).
-primitive(plus_plus/2).

-export([eq_eq/2, less/2, greater/2, greater_eq/2, eq_less/2]).
-primitive(eq_eq/2).
-primitive(less/2).
-primitive(greater/2).
-primitive(greater_eq/2).
-primitive(eq_less/2).

-export([raise/2]).
-primitive(raise/2).
