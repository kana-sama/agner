-module(erlang).

-export([is_integer/1, is_list/1]).
-export([integer_to_list/1]).
-export([garbage_collect/0]).
-export([self/0, send/2, spawn/1]).
-export([error/1]).

-primitive(is_integer/1).
-primitive(is_list/1).

-primitive(integer_to_list/1).

-primitive(garbage_collect/0).

-primitive(self/0).
-primitive(send/2).
-primitive(spawn/1).

-primitive(error/1).

-export([element/2]).
-primitive(element/2).
