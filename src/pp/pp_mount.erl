%%%--------------------------------------
%%% @Module  : pp_mount
%%% @Author  : xhg
%%% @Email   : xuhuguang@jieyou.com
%%% @Created : 2010.06.02
%%% @Description:  坐骑操作
%%%--------------------------------------

-module(pp_mount).
-export([handle/3]).
-include("common.hrl").
-include("record.hrl").


%% 查询坐骑详细信息
handle(16001, PlayerStatus, MounTaskId) ->
    [Res, MountTypeId, BindState, UseState] = gen_server:call(PlayerStatus#player_status.mount_pid, {'info', PlayerStatus, MounTaskId}),
    {ok, BinData} = pt_16:write(16001, [Res, MounTaskId, MountTypeId, BindState, UseState]),
    lib_send:send_one(PlayerStatus#player_status.socket, BinData);

%% 乘上坐骑
handle(16002, PlayerStatus, MounTaskId) ->
    [NewPlayerStatus, Res, OldMounTaskId, OldMountTypeId, OldMountCell, MountTypeId] = gen_server:call(PlayerStatus#player_status.mount_pid, {'get_on', PlayerStatus, MounTaskId}),
    {ok, BinData} = pt_16:write(16002, [Res, MounTaskId, OldMounTaskId, OldMountTypeId, OldMountCell, NewPlayerStatus#player_status.speed]),
    lib_send:send_one(NewPlayerStatus#player_status.socket, BinData),
    %% 广播
    {ok, BinData1} = pt_12:write(12010, [NewPlayerStatus#player_status.id, NewPlayerStatus#player_status.speed, MountTypeId]),
    lib_send:send_to_area_scene(NewPlayerStatus#player_status.scene, NewPlayerStatus#player_status.x, NewPlayerStatus#player_status.y, BinData1),
    {ok, NewPlayerStatus};

%% 离开坐骑
handle(16003, PlayerStatus, MounTaskId) ->
    [NewPlayerStatus, Res, MountTypeId, MountCell] = gen_server:call(PlayerStatus#player_status.mount_pid, {'get_off', PlayerStatus, MounTaskId}),
    {ok, BinData} = pt_16:write(16003, [Res, MounTaskId, MountTypeId, MountCell, NewPlayerStatus#player_status.speed]),
    lib_send:send_one(NewPlayerStatus#player_status.socket, BinData),
    %% 广播
    {ok, BinData1} = pt_12:write(12010, [NewPlayerStatus#player_status.id, NewPlayerStatus#player_status.speed, 0]),
    lib_send:send_to_area_scene(NewPlayerStatus#player_status.scene, NewPlayerStatus#player_status.x, NewPlayerStatus#player_status.y, BinData1),
    {ok, NewPlayerStatus};

%% 丢弃坐骑
handle(16004, PlayerStatus, MounTaskId) ->
    Res = gen_server:call(PlayerStatus#player_status.mount_pid, {'throw_away', PlayerStatus, MounTaskId}),
    {ok, BinData} = pt_16:write(16004, [Res, MounTaskId]),
    lib_send:send_one(PlayerStatus#player_status.socket, BinData);

handle(_Cmd, _Status, _Data) ->
    ?DEBUG("pp_goods no match", []),
    {error, "pp_goods no match"}.
