%%----------------------------------------
%% 任务模块
%%
%% @author cayleung@gmail.com
%%----------------------------------------
-module(pp_task).
-compile(export_all).
-include("common.hrl").
-include("record.hrl").

%% 获取任务列表
handle(30000, PlayerStatus, []) ->
    %% 可接任务
    ActiveIds = lib_task:get_player_active_task(PlayerStatus),
    ActiveList = lists:map(
        fun(TaskId) ->
            TaskData = data_task:get(TaskId, PlayerStatus),
            TipList = lib_task:get_task_tips(active, TaskId, PlayerStatus),
            {TaskData#task.id, TaskData#task.level, TaskData#task.type, TaskData#task.name, TaskData#task.desc, TipList, TaskData#task.coin, TaskData#task.exp, TaskData#task.spt, TaskData#task.binding_coin, TaskData#task.attainment, TaskData#task.guild_exp, TaskData#task.contrib, TaskData#task.award_select_item_num, lib_task:get_task_award(TaskData, PlayerStatus), TaskData#task.award_select_item}
        end,
        ActiveIds
    ),
    %% 已接任务
    TriggerBag = lib_task:get_player_triggered_task(PlayerStatus),
    TriggerList = lists:map(
        fun(RoleTask) ->
            TaskData = data_task:get(RoleTask#role_task_bag.task_id, PlayerStatus),
            TipList = lib_task:get_task_tips(trigger, RoleTask#role_task_bag.task_id, PlayerStatus),
            {TaskData#task.id, TaskData#task.level, TaskData#task.type, TaskData#task.name, TaskData#task.desc, TipList, TaskData#task.coin, TaskData#task.exp, TaskData#task.spt, TaskData#task.binding_coin, TaskData#task.attainment, TaskData#task.guild_exp, TaskData#task.contrib, TaskData#task.award_select_item_num, lib_task:get_task_award(TaskData, PlayerStatus), TaskData#task.award_select_item}
        end,
        TriggerBag
    ),
    {ok, BinData} = pt_30:write(30000, [ActiveList, TriggerList]),
    lib_send:send_one(PlayerStatus#player_status.socket, BinData);

%% 触发任务
handle(30003, PlayerStatus, [TaskId]) ->
    case lib_task:trigger_task(TaskId, PlayerStatus) of
        {true, NewRS} ->    
            lib_scene:refresh_npc_ico(NewRS),
            {ok, BinData} = pt_30:write(30006, []),
            lib_send:send_one(NewRS#player_status.socket, BinData),
            {ok, BinData1} = pt_30:write(30003, [1, <<>>]),
            lib_send:send_one(NewRS#player_status.socket, BinData1),
            lib_task:preact_finish(TaskId, NewRS),
            {ok, NewRS};
        {false, Reason} -> {ok, [0, Reason], PlayerStatus}
    end;

%% 阵营任务特殊处理 
handle(30004, PlayerStatus, [10012, _])->
    ?DEBUG("这里要弹出大地图窗口", []),
    {ok, PlayerStatus};

%% 完成任务
handle(30004, PlayerStatus, [TaskId, SelectItemList])->
    case lib_task:finish_task(TaskId, SelectItemList, PlayerStatus) of
        {true, NewRS} ->
            {ok, BinData} = pt_30:write(30004, [1, <<>>]),
            lib_send:send_one(NewRS#player_status.socket, BinData),
            lib_scene:refresh_npc_ico(NewRS),           %% 刷新npc图标
            {ok, BinData1} = pt_30:write(30006, []),
            lib_send:send_one(NewRS#player_status.socket, BinData1),
            next_task_cue(TaskId, NewRS),       %% 显示npc的默认对话
            {ok, NewRS};
        {false, _Reason} ->
            ok
    end;

%% 放弃任务
handle(30005, PlayerStatus, [TaskId])->
    BinData1 = case lib_task:abnegate_task(TaskId, PlayerStatus) of
        true -> 
            lib_scene:refresh_npc_ico(PlayerStatus),
            {ok, BinData} = pt_30:write(30006, []),
            BinData;
        false -> 
            {ok, BinData} = pt_30:write(30005, [0]),
            BinData
    end,
    lib_send:send_one(PlayerStatus#player_status.socket, BinData1);

%% 任务对话事件
handle(30007, PlayerStatus, [TaskId, Id])->
    case lib_npc:get_npc_id(Id) of
        0 ->  0;
        NpcId -> lib_task:task_event(talk, {TaskId, NpcId}, PlayerStatus#player_status.id)
    end;


%% 触发并完成任务
handle(30008, PlayerStatus, [TaskId, SelectItemList])->
    case lib_task:trigger_and_finish(TaskId, SelectItemList, PlayerStatus) of
        {true, NewRS} ->    
            lib_scene:refresh_npc_ico(NewRS),           %% 刷新npc图标
            next_task_cue(TaskId, NewRS),       %% 显示npc的默认对话
            lib_task:after_finish(TaskId, NewRS),  %% 完成后的特殊操作
            {ok, NewRS};
        {false, Reason} -> 
            lib_task:abnegate_task(TaskId, PlayerStatus),
            {ok, [0, Reason], PlayerStatus}
    end;

%% 获取任务奖励信息
handle(30009, PlayerStatus, [TaskId]) ->
    case data_task:get(TaskId, PlayerStatus) of
        null -> {ok, PlayerStatus};
        TaskData ->
            {ok, [TaskData#task.id, TaskData#task.coin, TaskData#task.exp, TaskData#task.spt, TaskData#task.binding_coin, TaskData#task.attainment, TaskData#task.guild_exp, TaskData#task.contrib, TaskData#task.award_select_item_num, lib_task:get_task_award(TaskData, PlayerStatus), TaskData#task.award_select_item], PlayerStatus}
    end;

%% 学习技能
handle(30030, PlayerStatus, SkillId) ->
    lib_task:task_event(learn_skill, {SkillId}, PlayerStatus#player_status.id);

handle(_Cmd, _PlayerStatus, _Data) ->
    {error, bad_request}.

%% 完成任务后是否弹结束npc的默认对话
next_task_cue(TaskId, PlayerStatus) ->
    case data_task:get(TaskId, PlayerStatus) of
        null -> false;
        TaskData ->
            case TaskData#task.next_cue of
                0 -> false;
                _ -> 
                   Id = lib_npc:get_id(TaskData#task.end_npc, PlayerStatus#player_status.scene),
                   Npc = lib_npc:get_data(TaskData#task.end_npc),
                   TalkList = data_talk:get(Npc#ets_npc.talk),
                   TaskList = lib_task:get_npc_task_list(TaskData#task.end_npc, PlayerStatus),
                   {ok, BinData} = pt_32:write(32000, [Id, TaskList, TalkList]),
                    lib_send:send_one(PlayerStatus#player_status.socket, BinData),
                   true
            end
    end.
