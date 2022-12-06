%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 三月 2019 20:14
%%%-------------------------------------------------------------------
-module(user_default).
-author("Administrator").

%% API
-export([u/1]).

u(Module) ->
  util:mu(Module).
