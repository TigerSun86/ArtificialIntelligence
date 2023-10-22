local sdk = sdk;
local pcall = pcall;
local log = log;


local enemy_character_base_type_def = sdk.find_type_definition("snow.enemy.EnemyCharacterBase");
local enemy_character_base_after_calc_damage_damage_side_method = enemy_character_base_type_def:get_method("afterCalcDamage_DamageSide");
local check_die_method = enemy_character_base_type_def:get_method("checkDie");
local enemy_get_pos_field = enemy_character_base_type_def:get_method("get_Pos");

local enemy_calc_damage_info_type_def = sdk.find_type_definition("snow.hit.EnemyCalcDamageInfo.AfterCalcInfo_DamageSide");
local get_total_damage_method = enemy_calc_damage_info_type_def:get_method("get_TotalDamage");

local player_character_base_type_def = sdk.find_type_definition("snow.player.PlayerQuestBase");
local player_character_base_after_calc_damage_damage_side_method = player_character_base_type_def:get_method("afterCalcDamage_DamageSide");

local player_base_type_def = sdk.find_type_definition("snow.player.PlayerBase");
local player_get_pos_field = player_base_type_def:get_method("get_Pos");

--snow.enemy.EnemyActionParam
local action_param_field = sdk.find_type_definition("snow.enemy.EnemyCharacterBase"):get_field("<ActionParam>k__BackingField")

--Uint
local get_action_no_method = action_param_field:get_type():get_method("get_ActionNo")

--snow.enemy.EnemyActionParamData.ActionInfo
local get_action_param_action_data_method = action_param_field:get_type():get_method("get_ActionDataParam")

-- Quest manager/actual functionality toggles
local questManager = nil
local enemyManager = nil
local playerManager = nil
local syncedTime = nil
local file = nil
local lastAnim = nil
local isInQuest = false
local frameCount = 0
local lastDistance = 0

local function reset_file(current_time)
    if file then
        file:close()
    end

    file = io.open("shared_memory.txt", "w")
    file:write(string.format("%d time_synced\n", current_time))
    file:flush()
    log.info("[ReinforcementLearning] Reset file")
end 

local function write_to_file(str)
    if not file then
        log.warn("[ReinforcementLearning] Cannot write to file before sync time")
        return
    end
    local timeDifference = os.clock() - syncedTime
    local str = string.format("%f %s\n", timeDifference, str)
    log.info(string.format("[ReinforcementLearning] Writing info: %s", str))
    file:write(str)
    file:flush()
end

local function write_data_to_file(data_category, number)
    write_to_file(string.format("%s %d", data_category, number))
end

local function write_end_to_file()
    write_to_file("end")
end

local function has_written_end()
    local lastLine
    local file2 = io.open("shared_memory.txt", "r")
    for line in file2:lines() do
        if line ~= nil then
            lastLine = line
        end
    end
    file2:close()
    return lastLine ~= nil and string.find(lastLine, "end") ~= nil
end

-- snow.hit.EnemyCalcDamageInfo.AfterCalcInfo_DamageSide
local function update_enemy_damage(enemy, enemy_calc_damage_info)
    local dead_or_captured = check_die_method:call(enemy);
    if dead_or_captured == nil then
        return;
    end

    if dead_or_captured then
        return;
    end

    local damage = get_total_damage_method:call(enemy_calc_damage_info);
    log.info(string.format("[ReinforcementLearning] Dealed damage: %d", damage))
    write_data_to_file("enemy", damage)
end

local function update_player_damage(owner_type, player_calc_damage_info)
    local count = enemyManager:call("getBossEnemyCount")
    local action_no = 0
    for i = 0, count - 1 do
        local enemy = enemyManager:call("getBossEnemy", i)
        local action_param = action_param_field:get_data(enemy)
        if action_param then 
            local action_no_temp = get_action_no_method:call(action_param)
            log.info(string.format("[ReinforcementLearning] Took damage: %d", action_no_temp))
            action_no = action_no + action_no_temp
        end
    end

    write_data_to_file("player", action_no)
end

local function update_distance(masterPlayer)
    frameCount = frameCount + 1
    if frameCount % 60 ~= 0 then 
        -- Update only once per 60 frames.
        return
    end

    local enemy = enemyManager:call("getBossEnemy", 0)
    if enemy == nil then
        log.info("[ReinforcementLearning] Could not find the emeny")
        return 
    end

    local enemy_position = enemy_get_pos_field:call(enemy);
    local player_position = player_get_pos_field:call(masterPlayer);
    local distance = math.floor((enemy_position - player_position):length());
    -- Write to file only when distance is changed.
    -- This is to skip writing when pausing the game.
    if lastDistance == distance then
        -- Skip the updating when distance is not changed,
        -- this happens a lot when pausing the game.
        return
    end

    lastDistance = distance
    write_data_to_file("distance", distance)
end

local function init_module()
    -- grabbing the quest manager
    if not questManager then
        questManager = sdk.get_managed_singleton("snow.QuestManager")
    end

    if not enemyManager then
        enemyManager = sdk.get_managed_singleton('snow.enemy.EnemyManager')
    end

    if not playerManager then
        playerManager = sdk.get_managed_singleton("snow.player.PlayerManager")
    end

    -- Player dealed damage.
    sdk.hook(enemy_character_base_after_calc_damage_damage_side_method, function(args)
        pcall(update_enemy_damage, sdk.to_managed_object(args[2]), sdk.to_managed_object(args[3]));
    end, function(retval)
        return retval;    
    end);

    -- Player received damage.
    sdk.hook(player_character_base_after_calc_damage_damage_side_method, function(args)
        pcall(update_player_damage, sdk.to_managed_object(args[2]), sdk.to_managed_object(args[3]));
    end, function(retval)
        return retval;    
    end);
end


-- Event callback hook for behaviour updates
re.on_pre_application_entry("UpdateBehavior", function() -- unnamed/inline function definition
    if questManager == nil then
        return
    end
    -- getting Quest End state
    -- 0: still in quest, 1: ending countdown, 8: ending animation, 16: quest over
    local endFlow = questManager:get_field("_EndFlow")
    if endFlow > 0 then
        if isInQuest then
            write_end_to_file()
            isInQuest = false
            log.info("[ReinforcementLearning] Quest end")
        end
    end
end)

re.on_frame(function()
    if questManager == nil then
        return
    end
    if playerManager == nil then
        return
    end

    local masterPlayer = playerManager:call("findMasterPlayer");
    if masterPlayer == nil then
        return
    end

    if isInQuest then
        update_distance(masterPlayer)
    end

    local mBehaviortree = masterPlayer:call("get_GameObject"):call("getComponent(System.Type)",sdk.typeof("via.behaviortree.BehaviorTree"));
    local curNodeID = mBehaviortree:call("getCurrentNodeID", 0);
    if curNodeID == lastAnim then
        return
    end

    -- log.info("[ReinforcementLearning] anim: " .. curNodeID)
    lastAnim = curNodeID
    local status = questManager:get_field("_QuestStatus")
    if status ~= 2 then
        return
    end

    -- Anim of "Good Work"
    if curNodeID == 2440417892 then
        syncedTime = os.clock()
        currentTime = os.time()
        reset_file(currentTime)
        isInQuest = true
        log.info("[ReinforcementLearning] synced time at: " .. currentTime)
    end
end);

init_module();
