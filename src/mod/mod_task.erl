%%%------------------------------------
%%% @Module  : mod_scene
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2010.06.13
%%% @Description: 任务数据回写
%%%------------------------------------
-module(mod_task).
-behaviour(gen_server).
-export(
    [
        start_link/0
        ,stop/0
        ,add_finish_log/4
        ,del_finish_log/2
        ,add_trigger_log/6
        ,upd_trigger_task_state/4
        ,del_trigger_task/2
        ,compress/2
        ,write_back/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").

-record(state, {id = 1, interval = 0, limit = 0, cache = []}).

%% 添加完成日志
add_finish_log(Rid, TaskId, TriggerTime, FinishTime) ->
    gen_server:cast(?MODULE, {add_finish_log, Rid, TaskId, [Rid, TaskId, TriggerTime, FinishTime]}).

%% 添加完成日志
del_finish_log(Rid, TaskId) ->
    gen_server:cast(?MODULE, {del_finish_log, Rid, TaskId, [Rid, TaskId]}).

%% 添加触发
add_trigger_log(Rid, TaskId, TriggerTime, TaskState, TaskEndState, TaskMark) ->
    gen_server:cast(?MODULE, {add_trigger_log, Rid, TaskId, [Rid, TaskId, TriggerTime, TaskState, TaskEndState, TaskMark]}).

%% 更新任务记录器
upd_trigger_task_state(Rid, TaskId, TaskState, TaskMark) ->
    gen_server:cast(?MODULE, {upd_trigger_task_state, Rid, TaskId, [TaskState, TaskMark, Rid, TaskId]}).

%% 删除触发的任务
del_trigger_task(Rid, TaskId) ->
    gen_server:cast(?MODULE, {del_trigger_task, Rid, TaskId, [Rid, TaskId]}).

%% 立即回写所有缓存
write_back() ->
    gen_server:call(?MODULE, write_back).

start_link()->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 关闭服务器时回调
stop() ->
    ok.

init([])->
    erlang:send_after(10000, self(), syn_db),
    Timeout = 60000, %60000 * 1,
    Limit = 100,
    {ok, #state{id = 1, interval = Timeout, limit = Limit, cache = []}}.

%% 同步任务数据
syn_db([]) ->
    ok;

syn_db([{add_finish_log, _, _, Data} | List]) ->
    db_sql:execute(<<"insert into `task_log`(`role_id`, `task_id`, `trigger_time`, `finish_time`) values(?,?,?,?)">>, Data),
    syn_db(List);

syn_db([{add_trigger_log, _, _, Data} | List]) ->
    db_sql:execute(<<"insert into `task_bag`(`role_id`, `task_id`, `trigger_time`, `state`, `end_state`, `mark`) values(?,?,?,?,?,?)">>, Data),
    syn_db(List);

syn_db([{upd_trigger_task_state, _, _, Data} | List]) ->
    db_sql:execute(<<"update `task_bag` set state=?,mark=? where role_id=? and task_id=?">>, Data),
    syn_db(List);

syn_db([{del_trigger_task, _, _, Data} | List]) ->
    db_sql:execute(<<"delete from `task_bag` where role_id=? and task_id=?">>, Data),
    syn_db(List);

syn_db([{del_finish_log, Rid, TaskId, _} | List]) ->
    db_sql:execute(<<"delete from `task_log` where role_id=? and task_id=?">>, [Rid, TaskId]),
    syn_db(List).

%% 数据压缩
compress([], Result) ->
    Result; %% 旧 -> 新

compress([{FirType, FirRid, FirTaskId, FirData} | T ], Result) ->
    R = lists:foldl(fun(X, R)-> compress(X, R) end, {FirType, FirRid, FirTaskId, FirData, []}, T),
    {_, _, _, _, Cache} = R,
    compress(lists:reverse(Cache), [{FirType, FirRid, FirTaskId, FirData} | Result]);

compress({XType, XRid, XTaskId, XData}, {add_finish_log, Rid, TaskId, Data, Cache}) ->
    case  XRid =:= Rid andalso XTaskId =:= TaskId andalso XType =:= upd of
        false -> {add, Rid, TaskId, Data, [{XType, XRid, XTaskId, XData} | Cache]};
        true -> {add, Rid, TaskId, Data, Cache}
    end;

compress({XType, XRid, XTaskId, XData}, {upd_trigger_task_state, Rid, TaskId, Data, Cache}) ->
    case  XRid =:= Rid andalso XTaskId =:= TaskId andalso XType =:= upd_trigger_task_state of
        false -> {upd_trigger_task_state, Rid, TaskId, Data, [{XType, XRid, XTaskId, XData} | Cache]};
        true -> {upd_trigger_task_state, Rid, TaskId, Data, Cache}
    end;

compress({XType, XRid, XTaskId, XData}, {del_trigger_task, Rid, TaskId, Data, Cache}) ->
    case  XRid =:= Rid andalso XTaskId =:= TaskId andalso (XType =:= upd_trigger_task_state orelse XType =:= add) of
        false -> {del_trigger_task, Rid, TaskId, Data, [{XType, XRid, XTaskId, XData} | Cache]};
        true -> {del_trigger_task, Rid, TaskId, Data, Cache}
    end;

%% 测试用
compress({XType, XRid, XTaskId, XData}, {del_finish_log, Rid, TaskId, Data, Cache}) ->
    case  XRid =:= Rid andalso XTaskId =:= TaskId andalso XType =:= log of
        false -> {del_finish_log, Rid, TaskId, Data, [{XType, XRid, XTaskId, XData} | Cache]};
        true -> {del_finish_log, Rid, TaskId, Data, Cache}
    end;

compress(Elem, {Type, Rid, TaskId, Data, Cache}) ->
    {Type, Rid, TaskId, Data, [Elem | Cache]}.

%% 将要更新的数据加入到缓存中
handle_cast(Elem, State) ->
    {noreply, State#state{cache = [Elem | State#state.cache]}}.

%% 回写数据到数据库
handle_call(write_back, _From, State) ->
    NewCache = compress(State#state.cache, []), 
    syn_db(NewCache),
    {reply, ok, State#state{cache = []}};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_info(syn_db, State) ->
    %% 开始异步回写
    spawn(
        fun() -> 
            NewCache = compress(State#state.cache, []), 
            syn_db(NewCache)
            %% ?DEBUG("需回写任务数据[~w]，压缩并回写[~w]", [length(State#state.cache), length(NewCache)]) 
        end
    ),
    %% 再次启动闹钟
    erlang:send_after(State#state.interval, self(), syn_db),
    {noreply, State#state{cache = []}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
