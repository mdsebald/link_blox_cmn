%%% @doc
%%% 
%%% Common utilities for LinkBlox app.
%%%
%%% @end

-module(lb_utils).

-author("Mark Sebald").

-export([
          create_config/0,
          set_ssh_port/1,
          get_ssh_port/0,
          set_lang_mod/1,
          get_lang_mod/0,
          get_ui_string/1,
          get_log_string/1,
          get_block_type_strings/1,
          get_attrib_string/1,
          get_attrib_id/1,
          get_calendar_locale/0,
          get_map_string/2,
          get_ui_cmds/0,
          is_string/1
]).


%%
%% Create a configuration values store
%%
-spec create_config() -> ok.

create_config() ->
  % Create an ets table named 'config'
  % Delete current table if it already exists
  case ets:info(config) of
    undefined -> ok;

    _AlreadyExists -> ets:delete(config)
  end,
  lb_logger:debug("Creating config ets"),
  ets:new(config, [set, named_table, public]).


%%
%% Set the port number used by the SSH Daemon
%%
-spec set_ssh_port(SshPort :: pos_integer()) -> true.

set_ssh_port(SshPort) ->
  % Create an ets table named 'config'
  % if it doesn't already exist
  case ets:info(config) of
    undefined -> create_config();

    _AlreadyExists -> ok
  end,
  
  % PortNum is an entry of the config table
  ets:insert(config, {ssh_port, SshPort}).


%%
%% Get the port number used by the SSH daemon
%%
-spec get_ssh_port() -> pos_integer() | undefined.

get_ssh_port() ->
  case ets:lookup(config, ssh_port) of
    [{ssh_port, SshPort}] -> SshPort;

    _NotFound -> undefined
  end.

%%
%% Set the language module used 
%% LangMod is the name of the module containing the string maps
%% LangMod will change depending on the language used
%%
-spec set_lang_mod(LangMod :: atom()) -> true.

set_lang_mod(LangMod) ->
  % Create an ets table named 'config'
  % if it doesn't already exist
  case ets:info(config) of
    undefined -> create_config();

    _AlreadyExists -> ok
  end,
  
  % Language Module is an entry of the config table
  ets:insert(config, {lang_mod, LangMod}).


%%
%% Get the language module currently loaded
%%
-spec get_lang_mod() -> atom() | undefined.

-ifndef(TEST).

get_lang_mod() ->
  case ets:lookup(config, lang_mod) of
    [{lang_mod, LangMod}] -> LangMod;

    _NotFound -> undefined
  end.

-else.

get_lang_mod() -> lang_en_us.
 
-endif.

%%
%% Get the UI strings map
%%
-spec get_ui_string(UiStringId :: atom()) -> string().

get_ui_string(UiStringId) ->
  case get_lang_mod() of
    undefined ->
      ErrorStr = "Error: Language module not found",
      lb_logger:error(ErrorStr),
      ErrorStr;
  
    LangMod ->
      get_map_string(UiStringId, LangMod:ui_strings())
  end.


%%
%% Get the logging strings map
%%
-spec get_log_string(LogStringId :: atom()) -> string().

get_log_string(LogStringId) ->
  case get_lang_mod() of
    undefined ->
      ErrorStr = "Error: Language module not found",
      lb_logger:error(ErrorStr),
      ErrorStr;
  
    LangMod ->
      get_map_string(LogStringId, LangMod:log_strings())
  end.


%%
%% Get the block type name and description strings, 
%% corresponding to the given block module or block type string
%%
-spec get_block_type_strings(BlockModule :: module() | string()) -> 
                              {module(), string(), string()} | false.

get_block_type_strings(BlockModule) ->
  case get_lang_mod() of
    undefined ->
      lb_logger:error("Error: Language module not found"),
      false;

    LangMod ->
      % Search the list of block type strings in the language module
      % If BlockModule is an atom, search via the 1st element
      % If BlockModule is a string, search via the 2nd element
      Key = case is_atom(BlockModule) of
        true  -> 1;
        false -> 2
      end,
      lists:keyfind(BlockModule, Key, LangMod:block_type_strings())
  end.


%%
%% Get an attribute name string from attribute ID
%%
-spec get_attrib_string(AttribId :: atom()) -> string().

get_attrib_string(AttribId) ->
  case get_lang_mod() of
    undefined -> 
      lb_logger:error("Language module not found"),
      "NotFound";
  
    LangMod ->
      case lists:keyfind(AttribId, 1, LangMod:attrib_strings()) of
        {AttribId, AttribStr, _AttribDescr} -> AttribStr;

        % if AttribId not found in attrib strings, in the language module
        % Just return the AttribId as a string
        false -> 
          lb_logger:warning("~p not in attrib_strings() list", [AttribId]),
          atom_to_list(AttribId)
      end
  end.


%%
%% Find attribute ID for the given attribute string
%% or return not found
%% 
-spec get_attrib_id(AttribStr :: string()) -> lb_types:value_name() | {error, not_found}.

get_attrib_id(AttribStr) ->
  case get_lang_mod() of
    undefined -> 
      lb_logger:error("Error: Language module not found"),
      {error, not_found};

    LangMod ->
      case lists:keyfind(AttribStr, 2, LangMod:attrib_strings()) of
        {AttribId, AttribStr, _AttribDescr} -> AttribId;
      
        false -> {error, not_found}
      end
  end.


%%
%% Get the month and day name strings for the current Language module
%% 
-spec get_calendar_locale() -> [{atom(), [string()] | [tuple()]}] | {error, not_found}.

get_calendar_locale() ->
  case get_lang_mod() of
    undefined -> 
      lb_logger:error("Error: Language module not found"),
      {error, not_found};

    LangMod ->  LangMod:calendar_locale()
  end.


%%
%% Get the string specified by the string ID, from the given strings map
%%
-spec get_map_string(StringId :: atom(),
                     StringsMap :: map()) -> string().

get_map_string(StringId, StringsMap) ->
  try maps:get(StringId, StringsMap) of
    String -> String
  catch
    error:{badmap, StringsMap} -> io_lib:format("Error: bad strings map: ~p~n", [StringsMap]);
    error:{badkey, StringId} -> io_lib:format("Error: string ID: ~p not found~n", [StringId])
  end.


%%
%% Get the UI commands list
%%
-spec get_ui_cmds() -> term() | undefined.

get_ui_cmds() ->
  case get_lang_mod() of
    undefined -> undefined;

    LangMod -> LangMod:ui_cmds()
  end.


%%
%% Is list a printable string?
%%
-spec is_string(List :: list()) -> boolean().

is_string([]) -> true;
is_string(List) when is_list(List) -> lists:all(fun isprint/1, List);
is_string(_) -> false.

isprint(X) when X >= 32, X < 127 -> true;
isprint(_) -> false.


%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


-endif.