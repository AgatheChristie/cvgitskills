%%%--------------------------------------
%%% @Module  : pp_battle
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2010.05.19
%%% @Description: 战斗
%%%--------------------------------------
-module(pp_battle).
-export([handle/3]).
-include("common.hrl").
-include("record.hrl").

%%发动攻击 - 玩家VS怪
handle(20001, PS, [Id, SkillId]) ->
    case ets:lookup(?ETS_MON, Id) of
        [] ->
            skip;
        [Mon] ->
            case PS#player_status.hp > 0 andalso Mon#ets_mon.hp > 0 of
                true ->
                    case mod_battle:battle(PS#player_status.bid, [PS, Mon, SkillId]) of
                        none ->
                            ok;
                        Aer ->
                            {ok, Aer}
                    end;
                false ->
                    BData = [
                        2, PS#player_status.id, PS#player_status.hp, PS#player_status.x, PS#player_status.y,
                        1, Mon#ets_mon.id, Mon#ets_mon.hp, Mon#ets_mon.x, Mon#ets_mon.y
                    ],
                    mod_battle:battle_fail(1, BData, PS#player_status.socket)
            end
    end;

%%发动攻击 - 玩家VS玩家
%%Id:玩家ID
%%SkillId:玩家技能id
handle(20002, PS, [Id, SkillId]) ->
    case PS#player_status.id == Id of
        true ->
            case PS#player_status.hp > 0 of
                true ->
                    case  mod_battle:battle(PS#player_status.bid, [PS, PS, SkillId]) of
                        none ->
                            ok;
                        Aer ->
                            {ok, Aer}
                    end;
                false ->
                    BData = [
                            2, PS#player_status.id, PS#player_status.hp, PS#player_status.x, PS#player_status.y,
                            2, PS#player_status.id, PS#player_status.hp, PS#player_status.x, PS#player_status.y
                        ],
                    mod_battle:battle_fail(1, BData, PS#player_status.socket)
            end;
        false->
            case ets:lookup(?ETS_ONLINE, Id) of
                [] ->
                    skip;
                [Data] ->
                    case lib_scene:is_safe(Data#ets_online.scene, [Data#ets_online.x, Data#ets_online.y]) of
                        false ->
                            case catch gen:call(Data#ets_online.pid, '$gen_call', 'PLAYER', 2000) of
                                {'EXIT',_Reason} ->
                                    {ok, BinData} = pt_12:write(12004, Id),
                                    lib_send:send_to_scene(PS#player_status.scene, BinData),
                                    %%删除ETS记录
                                    ets:delete(?ETS_ONLINE, Id),
                                    ok;
                                {ok, Player} ->
                                    case PS#player_status.hp > 0 andalso Player#player_status.hp > 0 of
                                        true ->
                                            case  mod_battle:battle(PS#player_status.bid, [PS, Player, SkillId]) of
                                                none ->
                                                    ok;
                                                Aer ->
                                                    {ok, Aer}
                                            end;
                                        false ->
                                            BData = [
                                                    2, PS#player_status.id, PS#player_status.hp, PS#player_status.x, PS#player_status.y,
                                                    2, Player#player_status.id, Player#player_status.hp, Player#player_status.x, Player#player_status.y
                                                ],
                                            mod_battle:battle_fail(1, BData, PS#player_status.socket)
                                    end
                            end;
                        true ->
                            none
                            %mod_battle:battle_fail(2, Data#ets_online.id, PS#player_status.socket, 1)
                    end
            end
    end;

%%复活
handle(20004, PS, _D) ->
    case ets:lookup(?ETS_SCENE, PS#player_status.scene) of
        [] ->
            PS1 = PS#player_status{
                hp = PS#player_status.hp_lim,
                mp = PS#player_status.mp_lim
            };
        [Scene] ->
            PS1 = PS#player_status{
                hp = PS#player_status.hp_lim,
                mp = PS#player_status.mp_lim,
                x = Scene#ets_scene.x,
                y = Scene#ets_scene.y
            }
    end,
    %通知离开原来场景
    lib_scene:revive_to_scene(PS, PS1),
    {ok, PS1};


%%发动辅助技能
%%Id:玩家ID
%%SkillId:玩家技能id
handle(20006, PS, [Id, SkillId]) ->
    case PS#player_status.id == Id of
        true ->
            mod_battle:assist_skill(PS#player_status.bid, [PS, {}, SkillId]);
        false ->
            case ets:lookup(?ETS_ONLINE, Id) of
                [] ->
                    skip;
                [Data] ->
                    case catch gen:call(Data#ets_online.pid, '$gen_call', 'PLAYER', 2000) of
                        {'EXIT',_Reason} ->
                            {ok, BinData} = pt_12:write(12004, Id),
                            lib_send:send_to_scene(PS#player_status.scene, BinData),
                            %%删除ETS记录
                            ets:delete(?ETS_ONLINE, Id),
                            ok;
                        {ok, Player} ->
                            case PS#player_status.hp > 0 andalso Player#player_status.hp > 0 of
                                true ->
                                    mod_battle:assist_skill(PS#player_status.bid, [PS, Player, SkillId]);
                                false ->
                                    skip
                                    %mod_battle:battle_fail(2, Player#player_status.id, PS#player_status.socket, 1)
                            end
                    end
            end
    end;

handle(_Cmd, _PS, _Data) ->
    {error, "pp_battle no match"}.
