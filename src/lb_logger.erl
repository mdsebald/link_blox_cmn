%%% @doc
%%% Common logging server module.
%%% All LinkBlox log messages are routed through here.
%%%
%%% @end

-module(lb_logger).

-author("Mark Sebald").

%% ====================================================================
%% API functions
%% ====================================================================

-export([
          error/1,
          error/2,
          warning/1,
          warning/2,
          info/1,
          info/2,
          debug/1,
          debug/2
]).

% TODO: Add calls to configure logging, i.e. to file, turn off tty, etc

%%
%% Log error messages
%%
-spec error(LogMsg :: string() | atom()) -> ok.

error(LogMsg) ->
  log_message(error, LogMsg).


-spec error(LogMsg :: string() | atom(),
            Args :: list(term())) -> ok.

error(LogMsg, Args) -> 
  log_message(error, LogMsg, Args).


%%
%% Log warning messages
%%
-spec warning(LogMsg :: string() | atom()) -> ok.

warning(LogMsg) -> 
  log_message(warning, LogMsg).


-spec warning(LogMsg :: string() | atom(),
              Args :: list(term())) -> ok.

warning(LogMsg, Args) ->
  log_message(warning, LogMsg, Args).


%%
%% Log info messages
%%
-spec info(LogMsg :: string() | atom()) -> ok.

info(LogMsg) ->
  log_message(info, LogMsg).


-spec info(LogMsg :: string() | atom(),
           Args :: list(term())) -> ok.

info(LogMsg, Args) -> 
  log_message(info, LogMsg, Args).

%%
%% Log debug messages
%%
-spec debug(LogMsg :: string() | atom()) -> ok.

debug(LogMsg) ->
  log_message(debug, LogMsg).


-spec debug(LogMsg :: string() | atom(),
            Args :: list(term())) -> ok.

debug(LogMsg, Args) ->
  log_message(debug, LogMsg, Args).


%% ====================================================================
%% Internal functions
%% ====================================================================

%%
%% If LogMsgId is an atom, look up the string in the log_string Map
%% Otherwise assume LogMsg is already a string
%%
log_message(LogLevel, LogMsgId) when is_atom(LogMsgId) ->
  LogMsg = lb_utils:get_log_string(LogMsgId),
  logger:log(LogLevel, LogMsg);

log_message(LogLevel, LogMsg) ->
  logger:log(LogLevel, LogMsg, []).
    

log_message(LogLevel, LogMsgId, Args) when is_atom(LogMsgId) ->
  LogMsg = lb_utils:get_log_string(LogMsgId),
  logger:log(LogLevel, LogMsg, Args);

log_message(LogLevel, LogMsg, Args) ->
  logger:log(LogLevel, LogMsg, Args).
