-module(timer).
-export([sleep/1]).

% sleep(T) when is_integer(T), T > 16#ffffffff ->
%   receive after 16#ffffffff -> sleep(T - 16#ffffffff) end;
sleep(T) -> receive after T -> ok end.
