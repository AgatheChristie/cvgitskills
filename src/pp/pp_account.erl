%%%--------------------------------------
%%% @Module  : pp_account
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2010.05.10
%%% @Description:用户账户管理
%%%--------------------------------------
-module(pp_account).
-compile(export_all).
-include("common.hrl").
-include("record.hrl").


%%登陆验证
handle(10000, [], Data) ->
    try is_bad_pass(Data) of
        true -> true;
        false -> false
    catch
        _:_ -> false
    end;

%% 获取角色列表
handle(10002, Socket, Accname) when is_list(Accname) ->
    L = lib_account:get_role_list(Accname),
    {ok, BinData} = pt_10:write(10002, L),
    lib_send:send_one(Socket, BinData);

%% 创建角色
handle(10003, Socket, [Accid, Accname, Realm, Career, Sex, Name]) when is_list(Accname), is_list(Name) ->
    case pp_account:validate_name(Accname, Name) of  %% 角色名合法性检测
        {false, Msg} ->
            {ok, BinData} = pt_10:write(10003, {Msg, 0}),
            lib_send:send_one(Socket, BinData);
        true ->
            case lib_account:create_role(Accid, Accname, Name, Realm, Career, Sex) of
                {true, Id} ->
                    %%创建角色成功
                    {ok, BinData} = pt_10:write(10003, {1, Id}),
                    lib_send:send_one(Socket, BinData);
                false ->
                    %%角色创建失败
                    {ok, BinData} = pt_10:write(10003, {0, 0}),
                    lib_send:send_one(Socket, BinData);
                _E ->
                    ?I("Err:~p end", [_E])
            end;
        _E ->
            ?I("Err:~p end", [_E])
    end;

%% 删除角色
handle(10005, Socket, [Pid, Accname]) ->
    case lib_account:delete_role(Pid, Accname) of
        true ->
            {ok, BinData} = pt_10:write(10005, 1),
            lib_send:send_one(Socket, BinData);
        false ->
            {ok, BinData} = pt_10:write(10005, 0),
            lib_send:send_one(Socket, BinData)
    end;

%%心跳包
handle(10006, Socket, _R) ->
    ?DEBUG_PRINT("heart beat ~p", [Socket]),
    {ok, BinData} = pt_10:write(10006, []),
    lib_send:send_one(Socket, BinData);

handle(_Cmd, _Status, _Data) ->
    ?DEBUG("handle_account no match", []),
    {error, "handle_account no match"}.

%%通行证验证
is_bad_pass([_Accid, _Accname, _Tstamp, _Ts]) ->
%%    Md5 = integer_to_list(Accid) ++ Accname ++ integer_to_list(Tstamp) ++ ?TICKET,
%%    Hex = util:md5(Md5),
%%    %%?DEBUG("~p~n~p~n", [Md5, Hex]),
%%    Hex == Ts.
    true.

%% 角色名合法性检测
validate_name(Accname, Name) ->
    validate_name(len, Accname, Name).

%% 角色名合法性检测:长度
validate_name(len, Accname, Name) ->
    Len = string_width(Name),
    case Len < 11 andalso Len > 1 of
        true ->
            validate_name(existed, Accname, Name);
        false ->
            %%角色名称长度为1~5个汉字
            {false, 5}
    end;


%%判断角色名是否已经存在
%%Name:角色名
%%AccName:AccName
validate_name(existed, Accname, Name) ->
    case lib_player:is_exists(Name) of
        true ->
            %角色名称已经被使用
            {false, 3};
        false ->
            true
    end,
    case lib_player:is_accname_exists(Accname) of
        true ->
            %Accname名称已经被使用
            {false, 6};
        false ->
            true
    end;

validate_name(_, _Accname, _Name) ->
    {false, 2}.

%% 字符宽度，1汉字=2单位长度，1数字字母=1单位长度
string_width(String) ->
    string_width(String, 0).
string_width([], Len) ->
    Len;
string_width([H | T], Len) ->
    case H > 255 of
        true ->
            string_width(T, Len + 2);
        false ->
            string_width(T, Len + 1)
    end.
