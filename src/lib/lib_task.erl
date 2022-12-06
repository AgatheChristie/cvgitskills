%%%-----------------------------------
%%% @Module  : lib_task
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2010.05.05
%%% @Description: 任务
%%%-----------------------------------
-module(lib_task).
-compile(export_all).

-include("common.hrl").
-include("record.hrl").

%% 从数据库加载角色的任务数据
load_player_task_from_db(PS) ->
    case get_player_triggered_task(PS) =/= [] orelse get_player_finished_task_list(PS) =/= [] of
        true -> true;   %% 已经加载过就不再加载
        false ->
            RoleTaskList = db_sql:get_all(<<"select * from task_bag where role_id=?">>, [PS#player_status.id]),
            [
                ets:insert(?ETS_ROLE_TASK_BAG, #role_task_bag{id={PS#player_status.id, Tid}, role_id=PS#player_status.id, task_id=Tid, trigger_time = Tt, state = S, end_state = ES, mark = binary_to_term(M)})
                || [_, Tid, Tt, S ,ES, M] <-RoleTaskList
            ],
            RoleTaskLogList = db_sql:get_all(<<"select * from task_log where role_id=?">>, [PS#player_status.id]),
            [
                ets:insert(?ETS_ROLE_TASK_LOG, #role_task_log{role_id=PS#player_status.id, task_id=Tid2, trigger_time = Tt2, finish_time = Ft2})
                || [_, Tid2, Tt2, Ft2] <-RoleTaskLogList
            ],
            refresh_can_receive_task(PS)
    end.

%% 角色下线操作
offline_clean_task(PS) ->
    %% 清除ets缓存
    ets:match_delete(?ETS_TASK_QUERY_CACHE, PS#player_status.id),
    ets:match_delete(?ETS_ROLE_TASK_BAG, #role_task_log{role_id=PS#player_status.id, _='_'}),
    ets:match_delete(?ETS_ROLE_TASK_LOG, #role_task_log{role_id=PS#player_status.id, _='_'}).


%% 获取今天完成某任务的数量
get_today_count(TaskId, PS) ->
    {M, S, MS} = now(),
    {_, Time} = calendar:now_to_local_time({M, S, MS}),
    TodaySec = M * 1000000 + S - calendar:time_to_seconds(Time),
    TomorrowSec = TodaySec + 86400,
    length([0 || RoleTaskLog <- get_player_finished_task_list(PS), TaskId=:=RoleTaskLog#role_task_log.task_id, RoleTaskLog#role_task_log.finish_time >= TodaySec, RoleTaskLog#role_task_log.finish_time < TomorrowSec]).

%% 获取下一等级的任务
next_lev_list(PS) ->
   TaskIdList = data_task:get_ids(),
   F = fun(Tid) -> TaskData = data_task:get(Tid, PS), (PS#player_status.lv + 1) =:= TaskData#task.level end,
   [XTid || XTid<-TaskIdList, F(XTid)].

%% 能否触发任务的其他非硬性影响条件
trigger_other_condition(TaskData, PS) ->
    case gen_server:call(PS#player_status.goods_pid, {'cell_num'}) < length(TaskData#task.start_item) of
        true -> false; %% 空位不足，放不下触发时能获得的物品
        false -> true
    end.

%% 触发任务
trigger_task(TaskId, PS) ->
    case can_trigger_task(TaskId, PS) of
        true ->
            TaskData = data_task:get(TaskId, PS),
            ets:insert(?ETS_ROLE_TASK_BAG,
                #role_task_bag{
                    id={PS#player_status.id, TaskId},
                    role_id=PS#player_status.id ,
                    task_id = TaskId,
                    trigger_time = util:unixtime(),
                    state=0,
                    end_state=TaskData#task.state,
                    mark = TaskData#task.content
                }),
            mod_task:add_trigger_log(PS#player_status.id, TaskId, util:unixtime(), 0, TaskData#task.state, term_to_binary(TaskData#task.content)),
            refresh_can_receive_task(PS),
            {true, PS};
        _ ->
            {false, <<"条件不足！">>}
    end.

%%是否可以接受任务
can_trigger_task(TaskId, PS) ->
    Result = case data_task:get(TaskId, PS) of
        null ->
            <<"没有这个任务">>;
        TaskData ->
            case is_task_triggered(TaskId, PS) of
                true -> <<"已经触发过了">>; %%已经触发过了
                false ->
                    case PS#player_status.lv < TaskData#task.level of
                        true -> <<"等级不足">>; %% 等级不足
                        false ->
                            case check_realm(TaskData#task.realm, PS#player_status.realm) of
                                false -> <<"阵营不符合">>; %% 阵营不符合
                                true ->
                                    case check_career(TaskData#task.career, PS#player_status.career) of
                                        false -> <<"职业不符合">>; %% 职业不符合
                                        true ->
                                            case check_prev_task(TaskData#task.prev, PS) of
                                                false -> <<"前置任务未完成">>; %%前置任务未完成
                                                true ->
                                                    case check_repeat(TaskId, TaskData#task.repeat, PS) of
                                                        false -> <<"不能重复接受">>; %%不 能重复做
                                                        true ->
                                                            length([1||ConditionItem <- TaskData#task.condition, check_task_condition(ConditionItem, TaskId, PS)=:=false]) =:=0
                                                    end
                                            end
                                    end
                            end
                    end
            end
    end,
    Result.


%% 是否已触发过
is_task_triggered(TaskId, Rid) when is_integer(Rid)->
    ets:lookup(?ETS_ROLE_TASK_BAG, {Rid, TaskId}) =/= [];

is_task_triggered(TaskId, PS) ->
    ets:lookup(?ETS_ROLE_TASK_BAG, {PS#player_status.id, TaskId}) =/= [].

%% 阵营检测
check_realm(Realm, PSRealm) ->
    case Realm =:= 0 of
        true -> true;
        false -> PSRealm =:= Realm
    end.
%% 职业检测
check_career(Career, PSCareer) ->
    case Career =:= 0 of
        true -> true;
        false -> PSCareer =:= Career
    end.

%% 是否重复可以接
check_repeat(TaskId, Repeat, PS) ->
    case Repeat =:= 0 of
        true -> is_task_finished(TaskId, PS) =/= true;
        false -> true
    end.

%% 前置任务
check_prev_task(PrevId, PS) ->
    case PrevId =:= 0 of
        true -> true;
        false -> is_task_finished(PrevId, PS)
    end.

%% 是否完成任务
check_task_condition({task, TaskId}, _, PS) ->
    is_task_finished(TaskId, PS);

%% 是否完成其中之一的任务
check_task_condition({task_one, TaskList}, _, PS) ->
    lists:any(fun(Tid)-> is_task_finished(Tid, PS) end, TaskList);

%% 今天的任务次数是否过多
check_task_condition({daily_limit, Num}, ThisTaskId, PS) ->
    get_today_count(ThisTaskId, PS) < Num;

%% 帮会任务等级
check_task_condition({guild_level, Lev}, _, PS) ->
    case PS#player_status.guild_id =:= 0 of
        true -> false;
        false ->
            case lib_guild:get_guild_lev_by_id(PS#player_status.guild_id) of
                null -> false;
                GLevel -> GLevel >= Lev
            end
    end;

%% 容错
check_task_condition(_Other, _, _PS) ->
    false.

%% 遍历所有任务看是否可接任务
refresh_can_receive_task(PS) ->
    TaskIdList = data_task:get_ids(),
    ActiveTaskIdList = [Tid || Tid<-TaskIdList, can_trigger_task(Tid, PS) =:= true],
    ActiveTaskIdList2 = [],
    ets:insert(?ETS_TASK_QUERY_CACHE, {PS#player_status.id, ActiveTaskIdList ++ ActiveTaskIdList2}).

%% 完成任务
finish_task(TaskId, _ParamList, RS) ->
    case is_task_finish(TaskId, RS) of
        false -> {false, <<"任务未完成！">>};
        true ->
            TaskData = data_task:get(TaskId, RS),
            case check_can_get_award(TaskData, RS) of
                {false, Reason} -> {false, Reason};
                {true, RS0} ->
                    %% 回收物品
                    case length(TaskData#task.end_item) > 0 of
                        true -> gen_server:call(RS#player_status.goods_pid, {'throw_more', [ ItemId || {ItemId, _} <- TaskData#task.end_item]});
                        false -> false
                    end,
                    %% 奖励固定物品
                    case get_task_award(TaskData, RS) of
                        [] -> false;
                        Items ->
                            F = fun(GoodsTypeId, GoodsNum) ->
                                gen_server:call(RS0#player_status.goods_pid, {'give_goods', RS0, GoodsTypeId, GoodsNum})
                                end,
                            [F(Id, Num) || {Id, Num} <- Items]
                    end,
                    case TaskData#task.contrib > 0 of
                        true -> lib_guild:add_donation(RS#player_status.id, TaskData#task.contrib);
                        false -> false
                    end,
                    RS1 = lib_player:add_coin(RS0, TaskData#task.coin),
                    RS2 = lib_player:add_exp(RS1, TaskData#task.exp),
                    RS3 = case TaskData#task.spt > 0 of
                              true -> RS2#player_status{spirit = RS2#player_status.spirit + TaskData#task.spt};
                              false -> RS2
                          end,
                    LastRS = RS3,
                    Time = util:unixtime(),
                    RoleTask = get_player_triggered_task(TaskId, LastRS),
                    ets:delete(?ETS_ROLE_TASK_BAG, {LastRS#player_status.id, TaskId}),
                    ets:insert(?ETS_ROLE_TASK_LOG, #role_task_log{role_id=LastRS#player_status.id, task_id=TaskId, trigger_time = RoleTask#role_task_bag.trigger_time, finish_time = Time}),
                    %% 数据库回写
                    mod_task:del_trigger_task(RS#player_status.id, TaskId),
                    mod_task:add_finish_log(RS#player_status.id, TaskId, RoleTask#role_task_bag.trigger_time, Time),
                    refresh_can_receive_task(LastRS),
                    %% 完成后一些特殊操作
                    case LastRS =/= RS of
                        true -> lib_player:refresh_client(LastRS);
                        false -> ok
                    end,
                    {true, LastRS}
            end
    end.

%% 检测任务是否完成
is_task_finish(TaskId, PS) when is_integer(TaskId) ->
    case get_player_triggered_task(TaskId, PS) of
        false -> false;
        RoleTask -> is_task_finish(RoleTask, PS)
    end;
is_task_finish(RoleTask, PS) when is_record(RoleTask, role_task_bag) ->
    is_task_finish_mark(RoleTask#role_task_bag.mark, PS);
is_task_finish(Mark, PS) when is_list(Mark) ->
    is_task_finish_mark(Mark, PS).

is_task_finish_mark([], _) ->
    true;
is_task_finish_mark([MarkItem | T], PS) ->
    case check_content_is_finished(MarkItem, PS) of
        false -> false;
        true -> is_task_finish_mark(T, PS)
    end.

%% 检测任务内容是否完成
check_content_is_finished([_, Finish, kill, _NpcId, Num, NowNum], _Rid) ->
    Finish =:=1 andalso Num =:= NowNum;
check_content_is_finished([_, Finish, talk, _, _], _Rid) ->
    Finish =:=1;
check_content_is_finished([_, Finish, item, _, Num, NowNum], _Rid) ->
    Finish =:=1 andalso Num =:= NowNum;
check_content_is_finished([_, Finish | _], _Rid) ->
    Finish =:= 1;
check_content_is_finished(Other, _PS) ->
    ?DEBUG("错误任务内容~p",[Other]),
    false.

%% 检查是否能完成奖励的条件
check_can_get_award(TaskData, RS) ->
    case gen_server:call(RS#player_status.goods_pid, {'cell_num'}) < award_item_num(TaskData, RS) of
        true -> {false, <<"背包空间不足！">>};  %% 空位不足
        false -> {true, RS}
    end.

%% 有部分任务内容在触发的时候可能就完成了，主要是物品收集类任务，可能接取任务前就有物品
preact_finish(Rid) ->
    lists:member(true, [preact_finish(RoleTask, Rid) || RoleTask <- get_player_triggered_task(Rid)]).

preact_finish(TaskId, Rid) when is_integer(TaskId) ->
    preact_finish(get_player_triggered_task(TaskId, Rid), Rid);
preact_finish(RoleTask, Rid) ->
    lists:member(true, [preact_finish_check([State, Fin | T], Rid) || [State, Fin | T] <- RoleTask#role_task_bag.mark, State =:= RoleTask#role_task_bag.state, Fin =:= 0]).

%% 装备武器
preact_finish_check([_, 0, equip, _ItemId | _], _Rid) ->
    false; %% 默认都没穿上吧
preact_finish_check([_, 0, buy_equip, ItemId | _], Rid) ->
    case goods_util:get_task_goods_num(Rid, ItemId) > 0 of
        false -> false;
        true -> task_event(buy_equip, {ItemId}, Rid)
    end;
%% 收集物品
preact_finish_check([_, 0, item, ItemId, _, NowNum | _], Rid) ->
    Num = goods_util:get_task_goods_num(Rid, ItemId),
    case Num >  NowNum of
        false -> false;
        true -> task_event(item, [{ItemId, Num}], Rid)
    end;
preact_finish_check(_, _) ->
    false.

%% 奖励物品所需要的背包空间
award_item_num(TaskData, PS) ->
    length(get_task_award(TaskData, PS)) + length(get_task_gift(TaskData, PS)) + TaskData#task.award_select_item_num - length(TaskData#task.end_item).

%% 触发并完成任务
trigger_and_finish(TaskId, ParamList, PS) ->
    case trigger_task(TaskId, PS) of
        {false, Reason} -> {false, Reason};
        {true, PS1} -> finish_task(TaskId, ParamList, PS1)
    end.

%% 放弃任务
abnegate_task(TaskId, PS) ->
    case get_player_triggered_task(TaskId, PS) of
        false -> false;
        _ ->
            ets:delete(?ETS_ROLE_TASK_BAG, {PS#player_status.id, TaskId}),
            mod_task:del_trigger_task(PS#player_status.id, TaskId),
            refresh_can_receive_task(PS),
            true
    end.

%% 已接所有任务更新判断
task_action(0, Rid, Event, ParamList)->
    case get_player_triggered_task(Rid) of
        [] -> false;
        RoleTaskLog ->
            Result = [task_action_one(RoleTask, Rid, Event, ParamList)|| RoleTask<- RoleTaskLog],
            lists:member(true, Result)
    end;
%% 单个任务更新判断
task_action(TaskId, Rid, Event, ParamList)->
    case get_player_triggered_task(TaskId, Rid) of
        false -> false;
        RoleTask -> task_action_one(RoleTask, Rid, Event, ParamList)
    end.

%% 判断当前任务阶段是否完成，更新任务阶段
%% 有任一任务阶段完成的则返回true
task_action_one(RoleTask, Rid, Event, ParamList) ->
    F = fun(MarkItem, Update)->
        [State, Finish, Eve| _T] = MarkItem,
        case State =:= RoleTask#role_task_bag.state andalso Finish =:= 0 andalso Eve=:=Event of
            false -> {MarkItem, Update};
            true ->
                {NewMarkItem, NewUpdate} = check_task_action(MarkItem, Rid, ParamList),
                case NewUpdate of
                    true -> {NewMarkItem, true};
                    false -> {NewMarkItem, Update}
                end
        end
        end,
    {NewMark, UpdateAble} = lists:mapfoldl(F ,false, RoleTask#role_task_bag.mark),
    case UpdateAble of
        false -> false;
        true ->
            NewState = case lists:member(false, [Fi=:=1||[Ts,Fi|_T1 ] <- NewMark,Ts=:=RoleTask#role_task_bag.state]) of
                           true -> RoleTask#role_task_bag.state; %%当前阶段有未完成的
                           false -> RoleTask#role_task_bag.state + 1 %%当前阶段全部完成，进入下一阶段
                       end,
            ets:insert(?ETS_ROLE_TASK_BAG, RoleTask#role_task_bag{state=NewState, mark = NewMark}),
            mod_task:upd_trigger_task_state(Rid, RoleTask#role_task_bag.task_id, NewState, term_to_binary(NewMark)),
            true
    end.

%% 杀怪
check_task_action([State, 0, kill, MonId, Num, NowNum], _Rid, NowMonId) ->
    case MonId =:= NowMonId of
        false ->{[State, 0, kill, MonId, Num, NowNum], false};
        true ->
            case NowNum + 1 >= Num of
                true -> {[State, 1, kill , MonId, Num, Num],  true};
                false ->{[State, 0, kill , MonId, Num, NowNum + 1], true}
            end
    end;

%% 对话
check_task_action([State, 0, talk, NpcId, TalkId], _Rid, [NowNpcId]) ->
    case NowNpcId =:= NpcId of
        true -> {[State, 1, talk, NpcId, TalkId], true};
        false -> {[State, 0, talk, NpcId, TalkId], false}
    end;

%% 物品
check_task_action([State, 0, item, ItemId, Num, NowNum], _Rid, [ItemList]) ->
    case [XNum || {XItemId, XNum} <- ItemList, XItemId =:= ItemId] of
        [] -> {[State, 0, item, ItemId, Num, NowNum], false}; %% 没有任务需要的物品
        [HaveNum | _] ->
            case HaveNum >= Num of
                true -> {[State, 1, item, ItemId, Num, Num], true};
                false -> {[State, 0, item, ItemId, Num, HaveNum], true}
            end
    end;

%% 装备物品
check_task_action([State, 0, buy_equip, ItemId | _], _Rid, [NowItemId]) ->
    case NowItemId =:= ItemId of
        false -> {[State, 0, buy_equip, ItemId], false};
        true -> {[State, 1, buy_equip, ItemId], true}
    end;

%% 购买物品
check_task_action([State, 0, equip, ItemId], _Rid, [NowItemId]) ->
    case NowItemId =:= ItemId of
        false -> {[State, 0, equip, ItemId], false};
        true -> {[State, 1, equip, ItemId], true}
    end;

%% 技能学习
check_task_action([State, 0, learn_skill, SkillId], _Rid, [NowSkillId]) ->
    case NowSkillId =:= SkillId of
        false -> {[State, 0, learn_skill, SkillId], false};
        true -> {[State, 1, learn_skill, SkillId], true}
    end;

%% 容错
check_task_action(MarkItem, _Other, _Other2) ->
    {MarkItem, false}.

%% 检查物品是否为任务需要
can_gain_item(Rid, ItemId) ->
    case get_player_triggered_task(Rid) of
        [] -> false;
        RoleTaskLog ->
            Result = [can_gain_item(marklist, get_task_content_unfinish(RoleTask), ItemId) || RoleTask <- RoleTaskLog],
            lists:member(true, Result)
    end.

can_gain_item(marklist, MarkList, ItemId) ->
    length([0 || [_, _, Type, Id | _T] <- MarkList, Type =:= item, Id =:= ItemId])>0.

%% 获取可接的任务
get_player_active_task(PS) ->
    case ets:lookup(?ETS_TASK_QUERY_CACHE, PS#player_status.id) of
        [] ->[];
        [{_,ActiveIds}] ->ActiveIds
    end.

%% 获取已触发任务列表
get_player_triggered_task(PS) when is_record(PS, player_status) ->
    ets:match_object(?ETS_ROLE_TASK_BAG, #role_task_bag{role_id=PS#player_status.id, _='_'});
get_player_triggered_task(Rid) when is_integer(Rid) ->
    ets:match_object(?ETS_ROLE_TASK_BAG, #role_task_bag{role_id=Rid, _='_'}).

%% 获取该阶段任务内容
get_task_content(RoleTask)->
    [[State | T] || [State | T] <- RoleTask#role_task_bag.mark, RoleTask#role_task_bag.state =:= State].

%% 获取任务阶段的未完成内容
get_task_content_unfinish(RoleTask)->
    [[State, Fin | T] || [State, Fin |T] <- RoleTask#role_task_bag.mark, RoleTask#role_task_bag.state =:= State ,Fin =:= 0].

%% 获取玩家已完成的任务列表
get_player_finished_task_list(PS) ->
    ets:match_object(?ETS_ROLE_TASK_LOG, #role_task_log{role_id=PS#player_status.id, _='_'}).

%% 获取玩家已触发的任务
get_player_triggered_task(TaskId, Rid) when is_integer(Rid) ->
    case ets:lookup(?ETS_ROLE_TASK_BAG, {Rid, TaskId}) of
        [] -> false;
        [RoleTask] -> RoleTask
    end;
get_player_triggered_task(TaskId, PS) when is_record(PS, player_status) ->
    case ets:lookup(?ETS_ROLE_TASK_BAG, {PS#player_status.id, TaskId}) of
        [] -> false;
        [RoleTask] -> RoleTask
    end.

%% 是否已完成任务列表里
is_task_finished(TaskId, PS)->
    ets:match_object(?ETS_ROLE_TASK_LOG, #role_task_log{role_id=PS#player_status.id, task_id=TaskId, _='_'}) =/= [].

%% 获取玩家能在该Npc接任务或者交任务
get_npc_task_list(NpcId, PS) ->
    {CanTrigger, Link, UnFinish, Finish} = get_npc_task(NpcId, PS),
    F = fun(Tid, NS) -> TaskData = data_task:get(Tid, PS), [Tid, NS, TaskData#task.name] end,
    L1 = [F(T1, 1) || T1 <- CanTrigger],
    L2 = [F(T2, 4) || T2 <- Link],
    L3 = [F(T3, 2) || T3 <- UnFinish],
    L4 = [F(T4, 3) || T4 <- Finish],
    L1 ++ L2 ++ L3 ++ L4.

%% 获取npc任务关联
%%{可接任务，关联，任务未完成，完成任务}
get_npc_task(NpcId, PS)->
    CanTrigger = get_npc_can_trigger_task(NpcId, PS),
    {Link, Unfinish, Finish} = get_npc_other_link_task(NpcId, PS),
    {CanTrigger, Link, Unfinish, Finish}.

%% 获取可接任务
get_npc_can_trigger_task(NpcId, PS) ->
    get_npc_can_trigger_task(get_player_active_task(PS), [], NpcId, PS).
get_npc_can_trigger_task([], Result, _, _) ->
    Result;
get_npc_can_trigger_task([TaskId | T ], Result, NpcId, PS) ->
    TaskData = data_task:get(TaskId, PS),
    case get_start_npc(TaskData#task.start_npc, PS#player_status.career) =:= NpcId of
        false -> get_npc_can_trigger_task(T, Result, NpcId, PS);
        true -> get_npc_can_trigger_task(T, Result ++ [TaskId], NpcId, PS)
    end.

%% 获取已触发任务
get_npc_other_link_task(NpcId, PS) ->
    get_npc_other_link_task(get_player_triggered_task(PS), {[], [], []}, NpcId, PS).
get_npc_other_link_task([], Result, _, _) ->
    Result;
get_npc_other_link_task([RoleTask | T], {Link, Unfinish, Finish}, NpcId, PS) ->
    TaskData = data_task:get(RoleTask#role_task_bag.task_id, PS),
    case is_task_finish(RoleTask, PS) andalso get_end_npc_id(RoleTask) =:= NpcId of  %% 判断是否完成
        true -> get_npc_other_link_task(T, {Link, Unfinish, Finish++[RoleTask#role_task_bag.task_id]}, NpcId, PS);
        false ->
            case task_talk_to_npc(RoleTask, NpcId) of %% 判断是否和NPC对话
                true -> get_npc_other_link_task(T, {Link++[RoleTask#role_task_bag.task_id], Unfinish, Finish}, NpcId, PS);
                false ->
                    case get_start_npc(TaskData#task.start_npc, PS#player_status.career) =:= NpcId of %% 判断是否接任务NPC
                        true -> get_npc_other_link_task(T, {Link, Unfinish++[RoleTask#role_task_bag.task_id], Finish}, NpcId, PS);
                        false -> get_npc_other_link_task(T, {Link, Unfinish, Finish}, NpcId, PS)
                    end
            end
    end.

%% 获取npc任务状态
get_npc_state(NpcId, PS)->
    {CanTrigger, Link, UnFinish, Finish} = get_npc_task(NpcId, PS),
    %% 0表示什么都没有，1表示有可接任务，2表示已接受任务但未完成，3表示有完成任务，4表示有任务相关
    case length(Finish) > 0 of
        true -> 3;
        false ->
            case length(Link)>0 of
                true-> 4;
                false->
                    case length(CanTrigger)>0 of
                        true ->    1;
                        false ->
                            case length(UnFinish)>0 of
                                true -> 2;
                                false -> 0
                            end
                    end
            end
    end.

%%检查任务的下一内容是否为与某npc的对话
task_talk_to_npc(RoleTask, NpcId)->
    Temp = [0||[State,Fin,Type,Nid|_]<- RoleTask#role_task_bag.mark, State=:= RoleTask#role_task_bag.state, Fin=:=0, Type=:=talk, Nid =:= NpcId],
    length(Temp)>0.

%% 获取任务对话id
get_npc_task_talk_id(TaskId, NpcId, PS) ->
    case data_task:get(TaskId, PS) of
        null -> 0;
        TaskData ->
            {CanTrigger, Link, UnFinish, Finish} = get_npc_task(NpcId, PS),
            case {
                lists:member(TaskId, CanTrigger),
                lists:member(TaskId, Link),
                lists:member(TaskId, UnFinish),
                lists:member(TaskId, Finish)
            }of
                {true, _, _, _} -> {start_talk, TaskData#task.start_talk};    %% 任务触发对话
                {_, true, _, _} ->    %% 关联对话
                    RoleTask = get_player_triggered_task(TaskId, PS),
                    [Fir|_] = [TalkId || [State,Fin,Type,Nid,TalkId|_] <- RoleTask#role_task_bag.mark, State=:= RoleTask#role_task_bag.state, Fin=:=0, Type=:=talk, Nid =:= NpcId],
                    {link_talk, Fir};
                {_, _, true, _} -> {unfinished_talk, TaskData#task.unfinished_talk};  %% 未完成对话
                {_, _, _, true} ->   %% 提交任务对话
                    RoleTask = get_player_triggered_task(TaskId, PS),
                    [Fir|_] = [TalkId || [_,_,Type,Nid,TalkId|_] <- RoleTask#role_task_bag.mark, Type=:=end_talk, Nid =:= NpcId],
                    {end_talk, Fir};
                _ -> {none, 0}
            end
    end.

%% 获取提示信息
get_task_tips(active, TaskId, PS) ->
    TaskData = data_task:get(TaskId, PS),
    case get_start_npc(TaskData#task.start_npc, PS#player_status.career) of
        0 -> [];
        StartNpcId -> [to_same_mark([0, 0, start_talk, StartNpcId], PS)]
    end;

get_task_tips(trigger, TaskId, PS) ->
    RoleTask = get_player_triggered_task(TaskId, PS),
    [to_same_mark([State|T], PS) || [State | T] <-RoleTask#role_task_bag.mark, RoleTask#role_task_bag.state=:= State].

%% 获取任务奖励
get_task_award(TaskData, PS) ->
    [{ItemId, Num} || {Career, ItemId, Num} <- TaskData#task.award_item, Career =:= 0 orelse Career =:= PS#player_status.career].

%% 获取任务礼包
get_task_gift(TaskData, PS) ->
    [{GiftId, Num} || {Career, GiftId, Num} <- TaskData#task.award_gift, Career =:= 0 orelse Career =:= PS#player_status.career].

%% 获取开始npc的id
%% 如果需要判断职业才匹配第2,3
get_start_npc(StartNpc, _) when is_integer(StartNpc) -> StartNpc;

get_start_npc([], _) -> 0;

get_start_npc([{career, Career, NpcId}|T], RoleCareer) ->
    case Career =:= RoleCareer of
        false -> get_start_npc(T, RoleCareer);
        true -> NpcId
    end.

%%获取结束任务的npcid
get_end_npc_id(TaskId, PS) ->
    case get_player_triggered_task(TaskId, PS) of
        false -> 0;
        RoleTask -> get_end_npc_id(RoleTask)
    end.

get_end_npc_id(RoleTask) when is_record(RoleTask, role_task_bag)->
    get_end_npc_id(RoleTask#role_task_bag.mark);

get_end_npc_id([]) -> 0;

get_end_npc_id(Mark) ->
    case lists:last(Mark) of
        [_, _, end_talk, NpcId, _] -> NpcId;
        _ -> 0  %% 这里是异常
    end.


%% 转换成一致的数据结构
to_same_mark([_, Finish, start_talk, NpcId | _], PS) ->
    {SId,SName} = get_npc_def_scene_info(NpcId, PS#player_status.realm),
    %% [类型, 完成, NpcId, Npc名称, 0, 0, 所在场景Id]
    [0, Finish, NpcId, lib_npc:get_name_by_npc_id(NpcId), 0, 0, SId, SName, []];

to_same_mark([_, Finish, end_talk, NpcId | _], PS) ->
    {SId,SName} = get_npc_def_scene_info(NpcId, PS#player_status.realm),
    %% [类型, 完成, NpcId, Npc名称, 0, 0, 所在场景Id]
    [1, Finish, NpcId, lib_npc:get_name_by_npc_id(NpcId), 0, 0, SId, SName, []];


to_same_mark([_, Finish, kill, MonId, Num, NowNum | _], _PS) ->
    {SId,SName, X, Y} = get_mon_def_scene_info(MonId),
    %% [类型, 完成, MonId, Npc名称, 需要数量, 已杀数量, 所在场景Id]
    [2, Finish, MonId, lib_mon:get_name_by_mon_id(MonId), Num, NowNum, SId, SName, [X, Y]];

to_same_mark([_, Finish, talk, NpcId | _], PS) ->
    {SId,SName} = get_npc_def_scene_info(NpcId, PS#player_status.realm),
    %% [类型, 完成, NpcId, Npc名称, 0, 0, 所在场景Id]
    [3, Finish, NpcId, lib_npc:get_name_by_npc_id(NpcId), 0, 0, SId, SName, []];

to_same_mark([_, Finish, item, ItemId, Num, NowNum | _], _PS) ->
    {NpcId, ItemName, SceneId, SceneName, X, Y} = case goods_util:get_task_mon(ItemId) of
                                                      0 -> {0, goods_util:get_goods_name(ItemId), 0, <<"未知场景">>, 0, 0};  %% 物品无绑定npc
                                                      XNpcId ->
                                                          {XSId,XSName, X0, Y0} = get_mon_def_scene_info(XNpcId),
                                                          {XNpcId, goods_util:get_goods_name(ItemId), XSId, XSName, X0, Y0}
                                                  end,
    %% [类型, 完成, 物品id, 物品名称, 0, 0, 0]
    [4, Finish, NpcId, ItemName, Num, NowNum, SceneId, SceneName, [NpcId, lib_mon:get_name_by_mon_id(NpcId), X, Y]];

to_same_mark([_, Finish, open_store | _], _PS) ->
    [5, Finish, 0, <<>>, 0, 0, 0, <<>>, []];

to_same_mark([_, Finish, equip ,ItemId | _], _PS) ->
    [6, Finish, ItemId, goods_util:get_goods_name(ItemId), 0, 0, 0, <<>>, []];

to_same_mark([_, Finish, buy_equip ,ItemId, NpcId| _], PS) ->
    {SId,SName} = get_npc_def_scene_info(NpcId, PS#player_status.realm),
    [7, Finish, ItemId, goods_util:get_goods_name(ItemId), 0, 0, SId, SName, [NpcId, lib_npc:get_name_by_npc_id(NpcId)]];

to_same_mark([_, Finish, learn_skill ,SkillId | _], _PS) ->
    [8, Finish, SkillId, <<"技能名称">>, 0, 0, 0, <<>>, []];

to_same_mark(MarkItem, _PS) ->
    MarkItem.

%%获取当前NPC所在的场景（自动寻路用）
get_npc_def_scene_info(NpcId, _Realm) ->
    case lib_npc:get_scene_by_npc_id(NpcId) of
        [] ->
            {0,<<>>};
        [SceneId, _, _] ->
            case ets:lookup(?ETS_SCENE, SceneId) of
                [] ->
                    {0,<<>>};
                [Scene] ->
                    {SceneId, Scene#ets_scene.name}
            end
    end.

%%获取当前NPC所在的场景（自动寻路用）
get_mon_def_scene_info(MonId) ->
    case lib_mon:get_scene_by_mon_id(MonId) of
        0 -> {0,<<>>};
        [SceneId, X, Y] ->
            case ets:lookup(?ETS_SCENE, SceneId) of
                [] -> {0,<<>>};
                [Scene] ->
                    {SceneId, Scene#ets_scene.name, X, Y}
            end
    end.

%% 任务事件------------------------------------

%% 对话事件
task_event(talk, {TaskId, NpcId}, Rid) ->
    case task_action(TaskId, Rid, talk, [NpcId]) of
        false-> false;
        true ->
            after_event(Rid),
            true
    end;

%% 打怪事件成功
task_event(kill, Monid, Rid) ->
    case task_action(0, Rid, kill, Monid) of
        false-> false;
        true ->
            after_event(Rid),
            true
    end;

%% 获得物品事件
task_event(item, ItemList, Rid) ->
    case task_action(0, Rid, item, [ItemList]) of
        false -> false;
        true ->
            after_event(Rid),
            true
    end;

%% 技能学习
task_event(learn_skill, {SkillId}, Rid) ->
    case task_action(0, Rid, learn_skill, [SkillId]) of
        false -> false;
        true ->
            after_event(Rid),
            true
    end;

%% 装备物品事件
task_event(equip, {ItemId}, Rid) ->
    case task_action(0, Rid, equip, [ItemId]) of
        false -> false;
        true ->
            after_event(Rid),
            true
    end;

%% 购买物品事件
task_event(buy_equip, {ItemId}, Rid) ->
    case task_action(0, Rid, buy_equip, [ItemId]) of
        false -> false;
        true ->
            after_event(Rid),
            true
    end.

after_event(Rid) ->
    case preact_finish(Rid) of
        true -> ok;
        false ->
            lib_scene:refresh_npc_ico(Rid),
            {ok, BinData} = pt_30:write(30006, []),
            lib_send:send_to_uid(Rid, BinData)
    end.

%% 条件
%% 任务是否完成
check_task_condition_daily_limit([], _, Num, _, _) ->
    Num > 0;
check_task_condition_daily_limit([RoleTaskLog | T], TaskId, Num, TodaySec, TomorrowSec) ->
    case 
        TaskId =:= RoleTaskLog#role_task_log.task_id andalso
        RoleTaskLog#role_task_log.finish_time > TodaySec andalso
        RoleTaskLog#role_task_log.finish_time < TomorrowSec
    of
        false -> check_task_condition_daily_limit(T, TaskId, Num, TodaySec, TomorrowSec);
        true -> %% 今天所完成的任务
            case Num - 1 > 0 of
                true -> check_task_condition_daily_limit(T, TaskId, Num - 1, TodaySec, TomorrowSec);
                false -> false
            end
    end.

after_finish(_TaskId, _PS) ->
    ok.

%% 筛选标签转换函数====================================================================

convert_select_tag(_, Val) when is_integer(Val) -> Val;

%% 职业筛选 战士，法师，刺客
convert_select_tag(RS, [career, Z, F, C]) ->
    case RS#player_status.career of
        1 -> Z;
        2 -> F;
        _ -> C
    end;

%% 职业筛选 天下 无双 傲视
convert_select_tag(RS, [realm, T, W, A]) ->
    case RS#player_status.realm of
        1 -> T;
        2 -> W;
        _ -> A
    end;

%% 性别
convert_select_tag(RS, [sex, Msg, Msg2]) ->
    case RS#player_status.sex of
        1 -> Msg;
        _ -> Msg2
    end;

convert_select_tag(_, Val) -> Val.

%% 触发、完成、奖励相关==============================================================================
