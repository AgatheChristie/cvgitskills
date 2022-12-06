%%%--------------------------------------
%%% @Module  : lib_goods
%%% @Author  : xhg
%%% @Email   : xuhuguang@jieyou.com
%%% @Created : 2010.05.24
%%% @Description : 物品信息
%%%--------------------------------------
-module(lib_goods).
-include("common.hrl").
-include("record.hrl").
-export(
    [
        add_goods/1,
        add_goods_attribute/4,
        modify_goods_attribute/2,
        del_goods_attribute/1,
        del_goods_attribute/3,
        buy_goods/4,
        sell_goods/4,
        equip_goods/5,
        unequip_goods/3,
        drag_goods/4,
        use_goods/4,
        delete_from_goods_list_by_num/3,
        delete_goods_by_num/3,
        delete_goods_by_type/2,
        movein_bag/4,
        moveout_bag/4,
        extend_bag/2,
        mend_goods/3,
        change_equip/3,
        clean_bag/2,
        mon_drop/2,
        rand_drop_num_rule/1,
        get_drop_rule_list/2,
        drop_choose/4,
        give_goods/2,
        give_goods_to_player/3,
        give_goods_to_player/4,
        give_goods_to_player/5,
        update_overlap_goods/2,
        add_overlap_goods/2,
        add_nonlap_goods/2,
        wear_down_equip/2,
        cost_money/3,
        add_money/3,
        delete_role/1
    ]
).


%% 保存物品信息
%% @spec set_goods_info(GoodsId, Field, Data) -> ok
%set_goods_info(GoodsId, Field, Data) ->
%    L = db_sql:make_conn_sql(Field, Data, []),
%    Sql = lists:concat(["update `goods` set ",L," where id = '",GoodsId,"'"]),
%    db_sql:execute(Sql),
%    ok.

%% 添加新物品信息
%% @spec add_goods(PlayerId, Cell, Num, Location, GoodsTypeInfo) -> ok
add_goods(GoodsInfo) ->
    Sql1 = lists:concat(["insert into `goods` set ",
                            "player_id = '",GoodsInfo#goods.player_id,"', ",
                            "goods_id = '",GoodsInfo#goods.goods_id,"', ",
                            "type = '",GoodsInfo#goods.type,"', ",
                            "subtype = '",GoodsInfo#goods.subtype,"', ",
                            "equip_type = '",GoodsInfo#goods.equip_type,"', ",
                            "price_type = '",GoodsInfo#goods.price_type,"', ",
                            "price = '",GoodsInfo#goods.price,"', ",
                            "sell_price = '",GoodsInfo#goods.sell_price,"', ",
                            "bind = '",GoodsInfo#goods.bind,"', ",
                            "trade = '",GoodsInfo#goods.trade,"', ",
                            "sell = '",GoodsInfo#goods.sell,"', ",
                            "isdrop = '",GoodsInfo#goods.isdrop,"', ",
                            "level = '",GoodsInfo#goods.level,"', ",
                            "vitality = '",GoodsInfo#goods.vitality,"', ",
                            "spirit = '",GoodsInfo#goods.spirit,"', ",
                            "hp = '",GoodsInfo#goods.hp,"', ",
                            "mp = '",GoodsInfo#goods.mp,"', ",
                            "forza = '",GoodsInfo#goods.forza,"', ",
                            "agile = '",GoodsInfo#goods.agile,"', ",
                            "wit = '",GoodsInfo#goods.wit,"', ",
                            "att = '",GoodsInfo#goods.att,"', ",
                            "def = '",GoodsInfo#goods.def,"', ",
                            "hit = '",GoodsInfo#goods.hit,"', ",
                            "dodge = '",GoodsInfo#goods.dodge,"', ",
                            "crit = '",GoodsInfo#goods.crit,"', ",
                            "ten = '",GoodsInfo#goods.ten,"', ",
                            "speed = '",GoodsInfo#goods.speed,"', ",
                            "attrition = '",GoodsInfo#goods.attrition,"', ",
                            "use_num = '",GoodsInfo#goods.use_num,"', ",
                            "suit_id = '",GoodsInfo#goods.suit_id,"', ",
                            "quality = '",GoodsInfo#goods.quality,"', ",
                            "quality_his = '",GoodsInfo#goods.quality_his,"', ",
                            "quality_fail = '",GoodsInfo#goods.quality_fail,"', ",
                            "stren = '",GoodsInfo#goods.stren,"', ",
                            "stren_his = '",GoodsInfo#goods.stren_his,"', ",
                            "stren_fail = '",GoodsInfo#goods.stren_fail,"', ",
                            "hole = '",GoodsInfo#goods.hole,"', ",
                            "hole1_goods = '",GoodsInfo#goods.hole1_goods,"', ",
                            "hole2_goods = '",GoodsInfo#goods.hole2_goods,"', ",
                            "hole3_goods = '",GoodsInfo#goods.hole3_goods,"', ",
                            "location = '",GoodsInfo#goods.location,"', ",
                            "cell = '",GoodsInfo#goods.cell,"', ",
                            "num = '",GoodsInfo#goods.num,"', ",
                            "color = '",GoodsInfo#goods.color,"', ",
                            "expire_time = '",GoodsInfo#goods.expire_time,"' "]),
    db_sql:execute(Sql1),
    NewGoodsInfo = goods_util:get_add_goods(GoodsInfo#goods.player_id, GoodsInfo#goods.goods_id, GoodsInfo#goods.location, GoodsInfo#goods.cell, GoodsInfo#goods.num),
    case is_record(NewGoodsInfo, goods) of
        true ->
            ets:insert(?ETS_GOODS_ONLINE, NewGoodsInfo),
            add_attribule_by_type(NewGoodsInfo);
        false -> skip
    end,
    NewGoodsInfo.

%% 添加附加属性
add_attribule_by_type(GoodsInfo) ->
    if GoodsInfo#goods.color > 0 ->
            Pattern = #ets_goods_add_attribute{ goods_id=GoodsInfo#goods.goods_id, _='_' },
            AttribuleList = goods_util:get_goods_list_from_ets(?ETS_GOODS_ADD_ATTRIBUTE, Pattern),
            [add_base_attribule(GoodsInfo, AttributeInfo) || AttributeInfo <- AttribuleList ];
        true ->
            skip
    end,
    ok.

%% 添加附加属性
add_base_attribule(GoodsInfo, BaseAttributeInfo) ->
    Effect = goods_util:get_add_attribute_by_type(BaseAttributeInfo),
    add_goods_attribute(GoodsInfo, 1, BaseAttributeInfo#ets_goods_add_attribute.attribute_id, Effect).

%% 添加装备属性
add_goods_attribute(GoodsInfo, AttributeType, AttributeId, Effect) ->
    [Hp, Mp, Att, Def, Hit, Dodge, Crit, Ten] = Effect,
    Sql = io_lib:format(<<"insert into `goods_attribute` set player_id = ~p, gid = ~p, attribute_type = ~p, attribute_id = ~p, hp=~p, mp=~p, att=~p, def=~p, hit=~p, dodge=~p, crit=~p, ten=~p ">>,
                                [GoodsInfo#goods.player_id, GoodsInfo#goods.id, AttributeType, AttributeId, Hp, Mp, Att, Def, Hit, Dodge, Crit, Ten]),
    db_sql:execute(Sql),
    AttributeInfo = goods_util:get_add_goods_attribute(GoodsInfo#goods.player_id, GoodsInfo#goods.id, AttributeType, AttributeId),
    if is_record(AttributeInfo, goods_attribute) ->
            ets:insert(?ETS_GOODS_ATTRIBUTE, AttributeInfo);
        true -> skip
    end,
    ok.

%% 更新装备属性
modify_goods_attribute(AttributeInfo, Effect) ->
    [Hp, Mp, Att, Def, Hit, Dodge, Crit, Ten] = Effect,
    Sql = io_lib:format(<<"update `goods_attribute` set hp=~p, mp=~p, att=~p, def=~p, hit=~p, dodge=~p, crit=~p, ten=~p where id = ~p ">>,
                                [Hp, Mp, Att, Def, Hit, Dodge, Crit, Ten, AttributeInfo#goods_attribute.id]),
    db_sql:execute(Sql),
    NewAttributeInfo = AttributeInfo#goods_attribute{ hp=Hp, mp=Mp, att=Att, def=Def, hit=Hit, dodge=Dodge, crit=Crit, ten=Ten },
    ets:insert(?ETS_GOODS_ATTRIBUTE, NewAttributeInfo),
    ok.

%% 删除装备属性
del_goods_attribute(PlayerId, GoodsId, AttributeType) ->
    Sql = io_lib:format(<<"delete from `goods_attribute` where player_id = ~p and gid = ~p and attribute_type = ~p ">>,
                                [PlayerId, GoodsId, AttributeType]),
    db_sql:execute(Sql),
    Pattern = #goods_attribute{ player_id=PlayerId, gid=GoodsId, attribute_type=AttributeType, _='_'},
    ets:match_delete(?ETS_GOODS_ATTRIBUTE, Pattern),
    ok.

%% 删除装备属性
del_goods_attribute(Id) ->
    Sql = io_lib:format(<<"delete from `goods_attribute` where id = ~p ">>, [Id]),
    db_sql:execute(Sql),
    ets:delete(?ETS_GOODS_ATTRIBUTE, Id),
    ok.

%% 购买物品
%% @spec buy_goods(GoodsStatus, GoodsTypeInfo, GoodsNum) -> ok | Error
buy_goods(GoodsStatus, GoodsTypeInfo, HasGoodsList, GoodsNum) ->
    GoodsInfo = goods_util:transform_goods_type_to_goods(GoodsTypeInfo),
    {ok, NewGoodsStatus} = give_goods_to_player(GoodsStatus, GoodsTypeInfo, GoodsNum, GoodsInfo, HasGoodsList),
    {ok, NewGoodsStatus}.

%% 出售物品
%% @spec sell_goods(PlayerStatus, Status, GoodsInfo, GoodsNum) -> {ok, NewPlayerStatus, NewStatus}
sell_goods(PlayerStatus, Status, GoodsInfo, GoodsNum) ->
    Amount = GoodsInfo#goods.sell_price * GoodsNum,
    NewCoin = PlayerStatus#player_status.coin + Amount,
    NewPlayerStatus = lib_goods:cost_money(PlayerStatus, 1, NewCoin),
    %% 删除物品
    {ok, NewStatus, _} = delete_goods_by_num(Status, GoodsInfo, GoodsNum),
    {ok, NewPlayerStatus, NewStatus}.

%%装备物品
%% @spec equip_goods(PlayerStatus, Status, GoodsInfo, Location, Cell) -> {ok, 1, Effect} | Error
equip_goods(PlayerStatus, Status, GoodsInfo, Location, Cell) ->
    BeforeGoodsInfo = goods_util:get_goods_by_cell(PlayerStatus#player_status.id, Location, Cell),
    case is_record(BeforeGoodsInfo, goods) of
        %% 存在已装备的物品，则替换
        true ->
            %% 腾空原来的位置
            NullCells = lists:sort([GoodsInfo#goods.cell | Status#goods_status.null_cells]),
            [OldGoodsCell | NullCells2] = NullCells,
            %% 旧的放入背包
            BeforeGoodsInfo2 = change_goods_cell(BeforeGoodsInfo, ?GOODS_LOCATION_BAG, OldGoodsCell),
            %% 新的放入该位置
            NewGoodsInfo = change_goods_cell(GoodsInfo, Location, Cell),
            EquipSuit = goods_util:update_equip_suit_list(Status#goods_status.equip_suit_list, BeforeGoodsInfo2#goods.suit_id, GoodsInfo#goods.suit_id),
            NewStatus = Status#goods_status{ null_cells=NullCells2, equip_suit_list=EquipSuit };
        %% 不存在
        false ->
            BeforeGoodsInfo2 = GoodsInfo,
            %% 腾空
            NullCells = lists:sort([GoodsInfo#goods.cell | Status#goods_status.null_cells]),
            %% 直接放入该位置
            NewGoodsInfo = change_goods_cell(GoodsInfo, Location, Cell),
            EquipSuit = goods_util:update_equip_suit_list(Status#goods_status.equip_suit_list, 0, GoodsInfo#goods.suit_id),
            NewStatus = Status#goods_status{ null_cells=NullCells, equip_suit_list=EquipSuit }
    end,
    case GoodsInfo#goods.bind =:= 1 of
        true -> bind_goods(GoodsInfo);
        false -> skip
    end,
    %% 人物属性重新计算
    {ok, NewPlayerStatus, NewStatus2} = goods_util:count_role_equip_attribute(PlayerStatus, NewStatus, NewGoodsInfo),
    %% 单件装备加成
    Effect2 = goods_util:get_goods_attribute(NewGoodsInfo),
    {ok, NewPlayerStatus, NewStatus2, BeforeGoodsInfo2, Effect2}.

%%卸下装备
%% @spec unequip_goods(PlayerStatus, Status, GoodsInfo) -> {ok, 1, [HP, MP, Attack, Defense, Strengh, Physique, Agility]}
unequip_goods(PlayerStatus, Status, GoodsInfo) ->
    [Cell | NullCells] = Status#goods_status.null_cells,
    NewGoodsInfo = change_goods_cell(GoodsInfo, ?GOODS_LOCATION_BAG, Cell),
    %% 检查是否是武器、衣服
    [Weapon, Clothes, Mount] = Status#goods_status.equip_current,
    if  NewGoodsInfo#goods.subtype =:= 10 ->
            CurrentEquip = [0, Clothes, Mount];
        NewGoodsInfo#goods.subtype =:= 21 ->
            CurrentEquip = [Weapon, 0, Mount];
        true ->
            CurrentEquip = [Weapon, Clothes, Mount]
    end,
    %% 更新套装列表
    EquipSuitList = goods_util:update_equip_suit_list(Status#goods_status.equip_suit_list, GoodsInfo#goods.suit_id, 0),
    NewStatus = Status#goods_status{ null_cells=NullCells, equip_current=CurrentEquip, equip_suit_list=EquipSuitList },
    %% 人物属性重新计算
    {ok, NewPlayerStatus, NewStatus2} = goods_util:count_role_equip_attribute(PlayerStatus, NewStatus, NewGoodsInfo),
    {ok, NewPlayerStatus, NewStatus2, NewGoodsInfo}.

%% 背包拖动物品
%% @spec drag_goods(GoodsInfo, OldCell, NewCell) -> {ok, NewStatus, [OldCellId, OldTypeId, NewCellId, NewTypeId]}
drag_goods(Status, GoodsInfo, OldCell, NewCell) ->
    OldGoodsInfo = goods_util:get_goods_by_cell(Status#goods_status.player_id, ?GOODS_LOCATION_BAG, NewCell),
    case is_record(OldGoodsInfo, goods) of
        false ->
            %% 新位置没有物品
            change_goods_cell(GoodsInfo, ?GOODS_LOCATION_BAG, NewCell),
            %% 整理空余位置
            NullCells = lists:delete(NewCell, Status#goods_status.null_cells),
            NullCells1 = lists:sort([OldCell|NullCells]),
            NewStatus = Status#goods_status{ null_cells=NullCells1 },
            OldCellId = 0,
            OldTypeId = 0,
            NewCellId = GoodsInfo#goods.id,
            NewTypeId = GoodsInfo#goods.goods_id;
        true ->
            %% 新位置有物品，交换位置
            change_goods_cell(GoodsInfo, ?GOODS_LOCATION_BAG, NewCell),
            change_goods_cell(OldGoodsInfo, ?GOODS_LOCATION_BAG, OldCell),
            NewStatus = Status,
            OldCellId = OldGoodsInfo#goods.id,
            OldTypeId = OldGoodsInfo#goods.goods_id,
            NewCellId = GoodsInfo#goods.id,
            NewTypeId = GoodsInfo#goods.goods_id
    end,
    {ok, NewStatus, [OldCellId, OldTypeId, NewCellId, NewTypeId]}.

%% 使用物品
%% @spec use_goods(PlayerStatus, Status, GoodsInfo, GoodsNum) -> {ok, NewPlayerStatus, NewStatus1, NewNum}
use_goods(PlayerStatus, Status, GoodsInfo, GoodsNum) ->
    %% 使用药品，有cd
    case GoodsInfo#goods.type =:= 20 andalso GoodsInfo#goods.expire_time > 0 of
        true -> 
            Ct_time = util:unixtime() + GoodsInfo#goods.expire_time,
            NewStatus = Status#goods_status{ ct_time=Ct_time };
        false ->
            NewStatus = Status
    end,
    {ok, NewStatus1, NewNum} = delete_goods_by_num(NewStatus, GoodsInfo, GoodsNum),
    %% 使用改变玩家状态的物品，需要修改player_status
    case GoodsInfo#goods.type of
        20 ->
            {ok, NewPlayerStatus} = use20(PlayerStatus, GoodsInfo, GoodsNum);
        22 ->
            {ok, NewPlayerStatus} = use22(PlayerStatus, GoodsInfo, GoodsNum);
        _ ->
            NewPlayerStatus = PlayerStatus
    end,
    {ok, NewPlayerStatus, NewStatus1, NewNum}.

%% 增加气血或者内力
use20(PlayerStatus, GoodsInfo, GoodsNum) ->
    %% 计算气血或者内力
    Hp = GoodsInfo#goods.hp * GoodsNum,
    Mp = GoodsInfo#goods.mp * GoodsNum,
    NewHp = case PlayerStatus#player_status.hp + Hp > PlayerStatus#player_status.hp_lim of
                true -> PlayerStatus#player_status.hp_lim;
                false -> PlayerStatus#player_status.hp + Hp
           end,
    NewMp = case PlayerStatus#player_status.mp + Mp > PlayerStatus#player_status.mp_lim of
                true -> PlayerStatus#player_status.mp_lim;
                false -> PlayerStatus#player_status.mp + Mp
           end,
    %% 更新人物属性
    %ok = gen_server:cast(PlayerStatus#player_status.pid, {'USE_GOODS', [NewHp, NewMp]}),
    NewPlayerStatus = PlayerStatus#player_status{ hp = NewHp, mp = NewMp },
    %% 气血改变广播
    if Hp > 0 ->
            {ok, BinData1} = pt_12:write(12014, [NewPlayerStatus#player_status.id, GoodsInfo#goods.goods_id, NewPlayerStatus#player_status.hp, NewPlayerStatus#player_status.hp_lim]),
            lib_send:send_to_area_scene(NewPlayerStatus#player_status.scene, NewPlayerStatus#player_status.x, NewPlayerStatus#player_status.y, BinData1);
        true -> skip
    end,
    {ok, NewPlayerStatus}.

%% 增加经验
use22(PlayerStatus, GoodsInfo, GoodsNum) ->
    case GoodsInfo#goods.subtype of
        %% 经验卡
        10 ->
            Exp = goods_util:get_goods_exp(GoodsInfo#goods.color),
            NewExp = Exp * GoodsNum,
            NewPlayerStatus = lib_player:add_exp(PlayerStatus, NewExp),
            {ok, NewPlayerStatus};
        _ ->
            {ok, PlayerStatus}
    end.

%% 从给定的列表中删除指定数量的物品
%% 有点迷的方法
%% @spec delete_from_goods_list_by_num(Status, GoodsList, GoodsNum) -> {ok, NewStatus}
delete_from_goods_list_by_num(Status, GoodsList, GoodsNum) ->
    GoodsList1 = goods_util:sort(GoodsList, cell),
    F1 = fun(GoodsInfo, [Num, Status1]) ->
            case Num > 0 of
                true ->
                    {ok, NewStatus1, Num1} = delete_goods_by_num(Status1, GoodsInfo, Num),
                    NewNum = case Num1 > 0 of
                                 true -> 0;
                                 false -> Num - GoodsInfo#goods.num
                             end,
                    [NewNum, NewStatus1];
                false ->
                    [Num, Status1]
            end
         end,
    [_, NewStatus] = lists:foldl(F1, [GoodsNum, Status], GoodsList1),
    {ok, NewStatus}.

%% 根据数量删除物品
%% @spec delete_goods_by_num(Status, GoodsInfo, GoodsNum) -> {ok, NewStatus, NewNum}
delete_goods_by_num(Status, GoodsInfo, GoodsNum) ->
    case GoodsInfo#goods.num > GoodsNum of
        true when GoodsInfo#goods.id >0 ->
            %% 删除部分
            NewNum = GoodsInfo#goods.num - GoodsNum,
            change_goods_num(GoodsInfo, NewNum),
            NewStatus = Status;
        false when GoodsInfo#goods.id >0 ->
            %% 删除全部
            NewNum = 0,
            delete_goods_by_id(GoodsInfo#goods.id),
            if  GoodsInfo#goods.location =/= 5 ->
                    ets:delete(?ETS_GOODS_ONLINE, GoodsInfo#goods.id),
                    NullCells = lists:sort([GoodsInfo#goods.cell|Status#goods_status.null_cells]),
                    NewStatus = Status#goods_status{ null_cells=NullCells };
                true ->
                    NewStatus = Status
            end;
        _ ->
            NewNum = GoodsNum,
            NewStatus = Status
    end,
    {ok, NewStatus, NewNum}.


%% 删除一类物品
%% @spec delete_goods_by_type(GoodsTypeId, GoodsStatus) -> {ok, NewStatus} | Error
delete_goods_by_type(GoodsTypeId, GoodsStatus) ->
    GoodsList = goods_util:get_goods_list_by_type_id(GoodsStatus#goods_status.player_id, GoodsTypeId, 4),
    if length(GoodsList) > 0 ->
            TotalNum = goods_util:get_goods_total_num(GoodsList),
            case (catch delete_from_goods_list_by_num(GoodsStatus, GoodsList, TotalNum)) of
                {ok, NewStatus} -> {ok, NewStatus};
                 Error -> {fail, Error, GoodsStatus}
            end;
        true ->
            {ok, GoodsStatus}
    end.

%%物品存入仓库
movein_bag(Status, GoodsInfo, GoodsNum, GoodsTypeInfo) ->
    GoodsList = goods_util:get_goods_list_by_type_id(GoodsInfo#goods.player_id, GoodsInfo#goods.goods_id, GoodsInfo#goods.bind, ?GOODS_LOCATION_STORAGE),
    GoodsList1 = goods_util:sort(GoodsList, id),
    case GoodsNum =:= GoodsInfo#goods.num of
        %% 全部放入
        true ->
            case length(GoodsList1) > 0 of
                true when GoodsTypeInfo#ets_goods_type.max_overlap > 1 ->
                    %% 更新原有的可叠加物品
                    [GoodsNum2,_] = lists:foldl(fun update_overlap_goods/2, [GoodsNum, GoodsTypeInfo#ets_goods_type.max_overlap], GoodsList1),
                    case GoodsNum2 > 0 of
                        true ->
                            change_goods_cell_and_num(GoodsInfo, ?GOODS_LOCATION_STORAGE, 0, GoodsNum2);
                        false ->
                            delete_goods_by_id(GoodsInfo#goods.id)
                    end;
                _ ->
                    %% 不存在或者不可叠加，且直接移入仓库
                    change_goods_cell(GoodsInfo, ?GOODS_LOCATION_STORAGE, 0)
            end,
            ets:delete(?ETS_GOODS_ONLINE, GoodsInfo#goods.id),
            NullCells = lists:sort([GoodsInfo#goods.cell | Status#goods_status.null_cells]),
            NewStatus = Status#goods_status{ null_cells=NullCells };
        %% 部份
        false ->
            GoodsNum20 = case length(GoodsList1) > 0 of
                            %% 存在
                            true ->
                                %% 更新原有的可叠加物品
                                [GoodsNum2,_] = lists:foldl(fun update_overlap_goods/2, [GoodsNum, GoodsTypeInfo#ets_goods_type.max_overlap], GoodsList1),
                                GoodsNum2;
                            false ->
                                GoodsNum
                        end,
            %% 添加新的记录
            case GoodsNum20 > 0 of
                true -> 
                    NewGoodsInfo1 = GoodsInfo#goods{ location=?GOODS_LOCATION_STORAGE, cell=0, num=GoodsNum20 },
                    add_goods(NewGoodsInfo1);
                false -> void
            end,
            %% 更改原数量
            NewNum = GoodsInfo#goods.num - GoodsNum,
            change_goods_num(GoodsInfo, NewNum),
            NewStatus = Status
    end,
    {ok, NewStatus}.

%%从仓库取出物品
moveout_bag(Status, GoodsInfo, GoodsNum, GoodsTypeInfo) ->
    GoodsList = goods_util:get_goods_list_by_type_id(GoodsInfo#goods.player_id, GoodsInfo#goods.goods_id, GoodsInfo#goods.bind, 4),
    GoodsList1 = goods_util:sort(GoodsList, cell),
    NewStatus = case GoodsNum =:= GoodsInfo#goods.num of
                    %% 全部
                    true ->
                        case length(GoodsList1) > 0 of
                            %% 存在且可叠加
                            true when GoodsTypeInfo#ets_goods_type.max_overlap > 1 ->
                                %% 更新原有的可叠加物品
                                [GoodsNum2,_] = lists:foldl(fun update_overlap_goods/2, [GoodsNum, GoodsTypeInfo#ets_goods_type.max_overlap], GoodsList1),
                                case GoodsNum2 > 0 of
                                    true ->
                                        [NewCell|NullCells] =Status#goods_status.null_cells,
                                        change_goods_cell_and_num(GoodsInfo, 4, NewCell, GoodsNum2),
                                        Status#goods_status{ null_cells=NullCells };
                                    false ->
                                        delete_goods_by_id(GoodsInfo#goods.id),
                                        Status
                                end;
                            %% 不存在或者不可叠加，且直接移入背包
                            _ ->
                                [NewCell|NullCells] =Status#goods_status.null_cells,
                                change_goods_cell(GoodsInfo, 4, NewCell),
                                Status#goods_status{ null_cells=NullCells }
                        end;
                    %% 部份
                    false ->
                        case length(GoodsList1) > 0 of
                            %% 存在
                            true ->
                                %% 更新原有的可叠加物品
                                [GoodsNum2,_] = lists:foldl(fun update_overlap_goods/2, [GoodsNum, GoodsTypeInfo#ets_goods_type.max_overlap], GoodsList1);
                            false ->
                                GoodsNum2 = GoodsNum
                        end,
                        case GoodsNum2 > 0 of
                            true ->
                                [NewCell|NullCells] =Status#goods_status.null_cells,
                                NewGoodsInfo2 = GoodsInfo#goods{ num=GoodsNum2, cell=NewCell, location=4 },
                                add_goods(NewGoodsInfo2),
                                ets:insert(?ETS_GOODS_ONLINE, NewGoodsInfo2),
                                Status#goods_status{ null_cells=NullCells };
                            false ->
                                Status
                        end,
                        %% 更改原数量
                        NewNum = GoodsInfo#goods.num - GoodsNum,
                        change_goods_num(GoodsInfo, NewNum)
                end,
    {ok, NewStatus}.

%% 扩展背包
extend_bag(PlayerStatus, Cost) ->
    CellNum = 49,
    NewCellNum = PlayerStatus#player_status.cell_num + CellNum,
    PlayerStatus1 = goods_util:get_cost(PlayerStatus, Cost, coin),
    NewPlayerStatus = PlayerStatus1#player_status{ cell_num = NewCellNum },
    Sql = io_lib:format(<<"update player set coin=~p, bcoin=~p, cell_num=~p where id=~p">>, 
                [NewPlayerStatus#player_status.coin, NewPlayerStatus#player_status.bcoin, NewCellNum, NewPlayerStatus#player_status.id]),
    db_sql:execute(Sql),
    {ok, NewPlayerStatus}.

%%修理装备
%% @spec mend_goods(PlayerId, GoodsInfo) -> {ok, NewCoin, Cost, [HP, MP, Attack, Defense]} | {error, Res}
mend_goods(PlayerStatus, GoodsStatus, GoodsInfo) ->
    MaxUseNum = goods_util:get_goods_max_use_num(GoodsInfo#goods.attrition),
    change_goods_use(GoodsInfo, MaxUseNum),
    %% 扣费
    Cost = goods_util:get_mend_cost(GoodsInfo#goods.attrition, GoodsInfo#goods.use_num),
    NewPlayerStatus = cost_money(PlayerStatus, Cost, coin),
    case GoodsInfo#goods.use_num =< 0 of
        %% 之前磨损为0
        true ->
            %% 人物属性重新计算
            EquipSuitList = goods_util:update_equip_suit_list(GoodsStatus#goods_status.equip_suit_list, 0, GoodsInfo#goods.suit_id),
            Status = GoodsStatus#goods_status{ equip_suit_list=EquipSuitList },
            {ok, NewPlayerStatus2, NewStatus} = goods_util:count_role_equip_attribute(NewPlayerStatus, Status, GoodsInfo);
        false ->
            NewPlayerStatus2 = NewPlayerStatus,
            NewStatus = GoodsStatus
     end,
     {ok, NewPlayerStatus2, NewStatus}.

%% 切换装备
change_equip(PlayerStatus, Status, EquipLocal) ->
    %% 切换装备
    Sql = io_lib:format(<<"update player set equip=~p where id=~p">>, [EquipLocal, PlayerStatus#player_status.id]),
    db_sql:execute(Sql),
    NewPlayerStatus = PlayerStatus#player_status{ equip=EquipLocal },
    %% 查询装备列表
    EquipList = goods_util:get_goods_list_by_type_and_location(PlayerStatus#player_status.id, 10, EquipLocal),
    %% 检查武器、衣服
    [NewStatus, _] = goods_util:count_current_equip_by_list(EquipList, [Status, on]),
    EquipSuit = goods_util:count_equip_suit_list(PlayerStatus#player_status.id, PlayerStatus#player_status.equip),
    NewStatus1 = NewStatus#goods_status{ equip_suit_list=EquipSuit },
    %% 人物属性重新计算
    {ok, NewPlayerStatus2, NewStatus2} = goods_util:count_role_equip_attribute(NewPlayerStatus, NewStatus1, {}),
    {ok, NewPlayerStatus2, NewStatus2, EquipList}.

%% 整理背包
clean_bag(GoodsInfo, [Num, OldGoodsInfo]) ->
    case is_record(OldGoodsInfo, goods) of
        %% 与上一格子物品类型相同
        true when GoodsInfo#goods.goods_id =:= OldGoodsInfo#goods.goods_id andalso GoodsInfo#goods.bind =:= OldGoodsInfo#goods.bind ->
            GoodsTypeInfo = goods_util:get_goods_type_by_id(GoodsInfo#goods.goods_id),
            case GoodsTypeInfo#ets_goods_type.max_overlap > 1 of
                %% 可叠加
                true ->
                    [NewGoodsNum, _] = update_overlap_goods(OldGoodsInfo, [GoodsInfo#goods.num, GoodsTypeInfo#ets_goods_type.max_overlap]),
                    case NewGoodsNum > 0 of
                        %% 还有剩余
                        true ->
                            NewGoodsInfo = change_goods_cell_and_num(GoodsInfo, ?GOODS_LOCATION_BAG, Num, NewGoodsNum),
                            [Num+1, NewGoodsInfo];
                        %% 没有剩余
                        false ->
                            delete_goods_by_id(GoodsInfo#goods.id),
                            ets:delete(?ETS_GOODS_ONLINE, GoodsInfo#goods.id),
                            NewGoodsNum1 = OldGoodsInfo#goods.num + GoodsInfo#goods.num,
                            NewOldGoodsInfo = OldGoodsInfo#goods{ num=NewGoodsNum1 },
                            [Num, NewOldGoodsInfo]
                    end;
                %% 不可叠加
                false ->
                    NewGoodsInfo = change_goods_cell(GoodsInfo, ?GOODS_LOCATION_BAG, Num),
                    [Num+1, NewGoodsInfo]
            end;
        %% 与上一格子类型不同
        true ->
            NewGoodsInfo = change_goods_cell(GoodsInfo, ?GOODS_LOCATION_BAG, Num),
            [Num+1, NewGoodsInfo];
        false ->
            NewGoodsInfo = change_goods_cell(GoodsInfo, ?GOODS_LOCATION_BAG, Num),
            [Num+1, NewGoodsInfo]
    end.

%% 掉落数随机
rand_drop_num_rule(MonId) ->
    Pattern1 = #ets_goods_drop_num{ mon_id=MonId, _='_' },
    NumRuleList = goods_util:get_goods_list_from_ets(?ETS_GOODS_DROP_NUM, Pattern1),
    case length(NumRuleList) > 0 of
        true ->
            TotalRatio1 = lists:foldl(fun(R, Sum) -> R#ets_goods_drop_num.ratio + Sum end, 0, NumRuleList),
            Ratio1 = util:rand(1, TotalRatio1),
            F1 = fun(Rule, [Ratio, First, Result]) ->
                End = First + Rule#ets_goods_drop_num.ratio,
                case Ratio > First andalso Ratio =< End of
                    true -> [Ratio, End, Rule];
                    false -> [Ratio, End, Result]
                end
                 end,
            [Ratio1, _, NumRule] = lists:foldl(F1, [Ratio1, 0, {}], NumRuleList);
       false ->
            NumRule = {}
    end,
    NumRule.

%% 掉落物品规则列表
get_drop_rule_list(PlayerId, MonId) ->
    Pattern1 = #ets_goods_drop_rule{ mon_id=MonId, _='_' },
    RuleList = goods_util:get_goods_list_from_ets(?ETS_GOODS_DROP_RULE, Pattern1),
    F1 = fun(Rule, [TotalRatio1, RuleList1]) ->
            case Rule#ets_goods_drop_rule.type =:= 50 of
                true ->
                    %% 查询任务物品是否可掉落
                    case lib_task:can_gain_item(PlayerId, Rule#ets_goods_drop_rule.goods_id) of
                        true -> [TotalRatio1+Rule#ets_goods_drop_rule.ratio, [Rule|RuleList1]];
                        false -> [TotalRatio1, RuleList1]
                    end;
                false ->
                    [TotalRatio1+Rule#ets_goods_drop_rule.ratio, [Rule|RuleList1]]
            end
         end,
    [TotalRatio2, RuleList2] = lists:foldl(F1, [0, []], RuleList),
    [TotalRatio2, RuleList2].

%% 循环掉落物品
mon_drop(_Num, [GoodsStatus, RuleList, AccGoodsList, TaskResult]) ->
    %% 随机取一个掉落规则
    Rule = util:rand_list_by_ratio(RuleList, #ets_goods_drop_rule.ratio),
    case Rule#ets_goods_drop_rule.goods_id > 0 of
        %% 有掉落
        true ->
            NewRuleList = lists:delete(Rule, RuleList),
            %% 掉落物品分析
            [NewGoodsStatus, AccGoodsList1, NewTaskResult] = handle_drop_rule(GoodsStatus, Rule, AccGoodsList, TaskResult),
            [NewGoodsStatus, NewRuleList, AccGoodsList1, NewTaskResult];
        %% 无掉落
        false ->
            [GoodsStatus, RuleList, AccGoodsList, TaskResult]
    end.

%% 掉落物品分析处理
handle_drop_rule(GoodsStatus, Rule, RuleResult, TaskResult) ->
    GoodsTypeId = Rule#ets_goods_drop_rule.goods_id,
    GoodsType = Rule#ets_goods_drop_rule.type,
    GoodsNum = Rule#ets_goods_drop_rule.goods_num,
    case GoodsType of
        10 -> %% 掉落装备
            Rand = util:rand(1, 10),
            Quality = case Rand > 3 of true -> 0; false -> Rand end,
            NewRuleResult = [{GoodsTypeId, GoodsType, GoodsNum, Quality} | RuleResult],
            NewTaskResult = TaskResult,
            NewGoodsStatus = GoodsStatus;
        50 -> %% 任务物品
            Pattern = #goods{ player_id=GoodsStatus#goods_status.player_id, goods_id=GoodsTypeId, location=4, _='_'},
            GoodsInfo = goods_util:get_ets_info(?ETS_GOODS_ONLINE, Pattern),
            case is_record(GoodsInfo, goods) of
                true ->
                    NewNum = GoodsInfo#goods.num + GoodsNum,
                    change_goods_num(GoodsInfo, NewNum),
                    NewGoodsStatus = GoodsStatus;
                false ->
                    GoodsTypeInfo = goods_util:get_ets_info(?ETS_GOODS_TYPE, GoodsTypeId),
                    GoodsInfo1 = goods_util:transform_goods_type_to_goods(GoodsTypeInfo),
                    [Cell|NullCells] = GoodsStatus#goods_status.null_cells,
                    NewGoodsStatus = GoodsStatus#goods_status{ null_cells=NullCells },
                    GoodsInfo2 = GoodsInfo1#goods{ player_id=GoodsStatus#goods_status.player_id, location=4, cell=Cell, num=GoodsNum },
                    add_goods(GoodsInfo2),
                    NewNum = GoodsNum
            end,
            NewRuleResult = RuleResult,
            NewTaskResult = [{GoodsTypeId, NewNum} | TaskResult];
        _ ->
            NewGoodsStatus = GoodsStatus,
            NewRuleResult = [{GoodsTypeId, GoodsType, GoodsNum, 0} | RuleResult],
            NewTaskResult = TaskResult
    end,
    [NewGoodsStatus, NewRuleResult, NewTaskResult].


%% 拣取地上掉落包的物品
drop_choose(PlayerStatus, Status, DropInfo, GoodsInfo) ->
    {GoodsTypeId, _Type, GoodsNum, GoodsQuality} = GoodsInfo,
    %% 添加物品
    GoodsTypeInfo = goods_util:get_goods_type_by_id(GoodsTypeId),
    NewInfo = goods_util:transform_goods_type_to_goods(GoodsTypeInfo),
    NewGoodsInfo = NewInfo#goods{ quality=GoodsQuality },
    {ok, NewStatus} = give_goods_to_player(Status, GoodsTypeInfo, GoodsNum, NewGoodsInfo),
    %% 有品质的装备，添加属性
    if GoodsTypeInfo#ets_goods_type.type =:= 10 andalso GoodsQuality > 0 ->
            AttributeId = goods_util:get_goods_attribute_id(NewGoodsInfo#goods.subtype),
            Effect = goods_util:count_quality_addition(NewGoodsInfo, GoodsQuality),
            add_goods_attribute(NewGoodsInfo, 3, AttributeId, Effect);
        true ->
            skip
    end,
    %% 更新掉落包，从ETS中删除该物品
    NewDropGoods = lists:delete(GoodsInfo, DropInfo#ets_goods_drop.drop_goods),
    case length(NewDropGoods) > 0 of
        true ->
            NewDropInfo = DropInfo#ets_goods_drop{ drop_goods=NewDropGoods },
            ets:insert(?ETS_GOODS_DROP, NewDropInfo);
        false ->
            ets:delete(?ETS_GOODS_DROP, DropInfo#ets_goods_drop.id),
            {ok, BinData} = pt_12:write(12019, DropInfo#ets_goods_drop.id),
            lib_send:send_to_team(PlayerStatus#player_status.sid, PlayerStatus#player_status.pid_team, BinData)
    end,
    {ok, NewStatus}.

%% 赠送物品
%% @spec give_goods(GoodsStatus, GoodsTypeId, GoodsNum) -> {ok, NewGoodsStatus} | {fail, Error, GoodsStatus}
give_goods({GoodsTypeId, GoodsNum}, GoodsStatus) ->
    GoodsTypeInfo = goods_util:get_goods_type_by_id(GoodsTypeId),
    case is_record(GoodsTypeInfo, ets_goods_type) of
        %% 物品不存在
        false ->
            {fail, {GoodsTypeId, not_found}, GoodsStatus};
        true ->
            HasGoodsList = goods_util:get_goods_list_by_type_id(GoodsStatus#goods_status.player_id, GoodsTypeId, GoodsTypeInfo#ets_goods_type.bind, ?GOODS_LOCATION_BAG),
            CellNum = goods_util:get_null_cell_num(HasGoodsList, GoodsTypeInfo#ets_goods_type.max_overlap, GoodsNum),
            case length(GoodsStatus#goods_status.null_cells) < CellNum of
                %% 背包格子不足
                true ->
                    {fail, cell_num, not_enough};
                false ->
                    GoodsInfo = goods_util:transform_goods_type_to_goods(GoodsTypeInfo),
                    case (catch give_goods_to_player(GoodsStatus, GoodsTypeInfo, GoodsNum, GoodsInfo, HasGoodsList)) of
                        {ok, NewStatus} ->
                            {ok, NewStatus};
                        Error ->
                            %?DEBUG("mod_goods give_goods:~p", [Error]),
                            {fail, Error, GoodsStatus}
                    end
            end
    end.


%% 更新原有的可叠加物品
%% 返回叠加现有物品后剩余的物品数量
update_overlap_goods(GoodsInfo, [Num, MaxOverlap]) ->
    case Num > 0 of
        true when GoodsInfo#goods.num =/= MaxOverlap andalso MaxOverlap > 0 ->
            {LapHasNum, RestNum} = case Num + GoodsInfo#goods.num > MaxOverlap of
                                   %% 总数超出可叠加数
                                   true ->
                                       {MaxOverlap, Num + GoodsInfo#goods.num - MaxOverlap};
                                   false ->
                                       {Num + GoodsInfo#goods.num, 0}
                               end,
            change_goods_num(GoodsInfo, LapHasNum);
        true ->
            RestNum = Num;
        false ->
            RestNum = 0
    end,
    [RestNum, MaxOverlap].

%% 添加物品
%% @spec give_goods_to_player(GoodsStatus, GoodsTypeInfo, GoodsInfo, GoodsNum) -> {ok, NewGoodsStatus}
give_goods_to_player(GoodsStatus, GoodsTypeInfo, GoodsNum) ->
    GoodsInfo = goods_util:transform_goods_type_to_goods(GoodsTypeInfo),
    give_goods_to_player(GoodsStatus, GoodsTypeInfo, GoodsNum, GoodsInfo).

give_goods_to_player(GoodsStatus, GoodsTypeInfo, GoodsNum, GoodsInfo) ->
    case GoodsTypeInfo#ets_goods_type.max_overlap > 1 of
        true ->
            List = goods_util:get_goods_list_by_type_id(GoodsStatus#goods_status.player_id, GoodsTypeInfo#ets_goods_type.goods_id, GoodsTypeInfo#ets_goods_type.bind, 4),
            GoodsList = goods_util:sort(List, cell);
        false ->
            GoodsList = []
    end,
    give_goods_to_player(GoodsStatus, GoodsTypeInfo, GoodsNum, GoodsInfo, GoodsList).

give_goods_to_player(GoodsStatus, GoodsTypeInfo, AddGoodsNum, GoodsInfo, HasGoodsList) ->
    %% 插入物品记录
    case GoodsTypeInfo#ets_goods_type.max_overlap > 1 of
        true ->
            %% 更新原有的可叠加物品
            [RestGoodsNum,_] = lists:foldl(fun update_overlap_goods/2, [AddGoodsNum, GoodsTypeInfo#ets_goods_type.max_overlap], HasGoodsList),
            %% 添加新的可叠加物品
            SplitGoodsNum = RestGoodsNum div GoodsTypeInfo#ets_goods_type.max_overlap + 1,
            [NewGoodsStatus,_,_,_] = lists:foldl(fun add_overlap_goods/2, [RestGoodsNum, GoodsStatus, 4, GoodsInfo, GoodsTypeInfo#ets_goods_type.max_overlap], lists:seq(1, SplitGoodsNum));
        false ->
            %% 添加新的不可叠加物品
            AllNum = lists:seq(1, AddGoodsNum),
            [NewGoodsStatus,_,_] = lists:foldl(fun add_nonlap_goods/2, [GoodsStatus, GoodsInfo, 4], AllNum)
    end,
    {ok, NewGoodsStatus}.

%% 添加新的可叠加物品
add_overlap_goods([RestGoodsNum, GoodsStatus, Location, GoodsInfo, MaxOverlap], _) ->
    {NewNum, OldNum} = case RestGoodsNum > MaxOverlap of
                           true ->
                               {RestGoodsNum - MaxOverlap, MaxOverlap};
                           false ->
                               {0, RestGoodsNum}
                       end,
    case OldNum > 0 of
        true when length(GoodsStatus#goods_status.null_cells) > 0 ->
            [Cell|NullCells] = GoodsStatus#goods_status.null_cells,
            NewGoodsStatus = GoodsStatus#goods_status{ null_cells=NullCells },
            NewGoodsInfo = GoodsInfo#goods{ player_id=GoodsStatus#goods_status.player_id, location=Location, cell=Cell, num=OldNum },
            add_goods(NewGoodsInfo);
        _ ->
            NewGoodsStatus = GoodsStatus
    end,
    [NewNum, NewGoodsStatus, Location, GoodsInfo, MaxOverlap].

%% 添加新的不可叠加物品
add_nonlap_goods(_, [GoodsStatus, GoodsInfo, Location]) ->
    case length(GoodsStatus#goods_status.null_cells) > 0 of
        true ->
            [Cell|NullCells] = GoodsStatus#goods_status.null_cells,
            NewGoodsStatus = GoodsStatus#goods_status{ null_cells=NullCells },
            NewGoodsInfo3 = GoodsInfo#goods{ player_id=GoodsStatus#goods_status.player_id, location=Location, cell=Cell, num=1 },
            add_goods(NewGoodsInfo3);
        false ->
            NewGoodsStatus = GoodsStatus
    end,
    [NewGoodsStatus, GoodsInfo, Location].

%% 装备磨损
wear_down_equip(GoodsInfo, [UseNum, ZeroEquipList]) ->
    case GoodsInfo#goods.attrition > 0 andalso GoodsInfo#goods.use_num > 0 of
        %% 耐久度降为0
        true when GoodsInfo#goods.use_num =< UseNum ->
            NewGoodsInfo = GoodsInfo#goods{ use_num=0 },
            ets:insert(?ETS_GOODS_ONLINE, NewGoodsInfo),
            [UseNum, [NewGoodsInfo|ZeroEquipList]];
        true ->
            NewUseNum = GoodsInfo#goods.use_num - UseNum,
            NewGoodsInfo = GoodsInfo#goods{ use_num=NewUseNum },
            ets:insert(?ETS_GOODS_ONLINE, NewGoodsInfo),
            [UseNum, ZeroEquipList];
        false ->
            [UseNum, ZeroEquipList]
    end.

%% 更改物品格子位置
change_goods_cell(GoodsInfo, Location, Cell) ->
    Sql = io_lib:format(<<"update `goods` set location = ~p, cell = ~p where id = ~p ">>, [Location, Cell, GoodsInfo#goods.id]),
    db_sql:execute(Sql),
    NewGoodsInfo = GoodsInfo#goods{ location=Location, cell=Cell },
    if Location =/= 5 ->
            ets:insert(?ETS_GOODS_ONLINE, NewGoodsInfo);
        true -> skip
    end,
    NewGoodsInfo.

%% 更改物品数量
change_goods_num(GoodsInfo, Num) ->
    Sql = io_lib:format(<<"update `goods` set num = ~p where id = ~p ">>, [Num, GoodsInfo#goods.id]),
    db_sql:execute(Sql),
    NewGoodsInfo = GoodsInfo#goods{ num=Num },
    if NewGoodsInfo#goods.location =/= 5 ->
            ets:insert(?ETS_GOODS_ONLINE, NewGoodsInfo);
        true -> skip
    end,
    NewGoodsInfo.

%% 更改物品格子位置和数量
change_goods_cell_and_num(GoodsInfo, Location, Cell, Num) ->
    Sql = io_lib:format(<<"update `goods` set location = ~p, cell = ~p, num = ~p where id = ~p ">>, [Location, Cell, Num, GoodsInfo#goods.id]),
    db_sql:execute(Sql),
    NewGoodsInfo = GoodsInfo#goods{ location=Location, cell=Cell, num=Num },
    if Location =/= 5 ->
            ets:insert(?ETS_GOODS_ONLINE, NewGoodsInfo);
        true -> skip
    end,
    NewGoodsInfo.

%% 更改物品耐久度
change_goods_use(GoodsInfo, UseNum) ->
    Sql = io_lib:format(<<"update `goods` set use_num = ~p where id = ~p ">>, [UseNum, GoodsInfo#goods.id]),
    db_sql:execute(Sql),
    NewGoodsInfo = GoodsInfo#goods{ use_num=UseNum },
    ets:insert(?ETS_GOODS_ONLINE, NewGoodsInfo),
    NewGoodsInfo.

%% 删除物品
%% 从数据库和ETS中删除
delete_goods_by_id(GoodsId) ->
    Sql1 = io_lib:format(<<"delete from `goods` where id = ~p ">>, [GoodsId]),
    db_sql:execute(Sql1),
    Sql2 = io_lib:format(<<"delete from `goods_attribute` where gid = ~p ">>, [GoodsId]),
    db_sql:execute(Sql2),
    Pattern = #goods_attribute{ gid=GoodsId, _='_'},
    ets:match_delete(?ETS_GOODS_ATTRIBUTE, Pattern),
    ok.

%% 扣除角色金钱
cost_money(PlayerStatus, Cost, Type) ->
    NewPlayerStatus = goods_util:get_cost(PlayerStatus, Cost, Type),
    Sql = io_lib:format(<<"update player set coin=~p, silver=~p, gold=~p, bcoin=~p where id=~p">>,
                        [NewPlayerStatus#player_status.coin, NewPlayerStatus#player_status.silver, NewPlayerStatus#player_status.gold, NewPlayerStatus#player_status.bcoin, NewPlayerStatus#player_status.id]),
    db_sql:execute(Sql),
    NewPlayerStatus.

%% 角色加钱
add_money(PlayerStatus, AddMoney, Type) ->
    NewPlayerStatus = goods_util:get_cost(PlayerStatus, AddMoney, Type),
    Sql = io_lib:format(<<"update player set coin=~p, silver=~p, gold=~p, bcoin=~p where id=~p">>,
        [NewPlayerStatus#player_status.coin, NewPlayerStatus#player_status.silver, NewPlayerStatus#player_status.gold, NewPlayerStatus#player_status.bcoin, NewPlayerStatus#player_status.id]),
    db_sql:execute(Sql),
    NewPlayerStatus.


%% 物品绑定
bind_goods(GoodsInfo) ->
    Sql = io_lib:format(<<"update `goods` set bind = 2, trade = 1 where id = ~p ">>, [GoodsInfo#goods.id]),
    db_sql:execute(Sql),
    NewGoodsInfo = GoodsInfo#goods{ bind=2, trade=1 },
    if GoodsInfo#goods.location =/= 5 ->
            ets:insert(?ETS_GOODS_ONLINE, NewGoodsInfo);
        true -> skip
    end,
    NewGoodsInfo.

%%当玩家删除角色时，删除有关于这角色的数据
delete_role(PlayerId) ->
    Sql1 = io_lib:format(<<"delete from `goods_attribute` where player_id = ~p">>, [PlayerId]),
    db_sql:execute(Sql1),
    Sql2 = io_lib:format(<<"delete from `goods` where player_id = ~p">>, [PlayerId]),
    db_sql:execute(Sql2),
    ok.
