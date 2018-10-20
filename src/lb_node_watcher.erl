%%% @doc 
%%% Node Watcher, Indicate when other LinkBlox nodes connect and disconnect
%%%
%%% @end

-module(lb_node_watcher).

-author("Mark Sebald").

-export([start/0, listen_start/0]).


-spec start() -> pid().

start() ->
  spawn(lb_node_watcher, listen_start, []).


%
% Listen for nodeup and nodedown messages
%
listen_start() ->
  lb_logger:info(starting_node_watcher),
  net_kernel:monitor_nodes(true),  
  listen_loop().


listen_loop() ->
  receive 
    {nodeup, Node} ->
      lb_logger:info(node_has_connected, [Node]);

    {nodedown, Node} ->
      lb_logger:info(node_has_disconnected, [Node]);

    Unexpected ->
      lb_logger:warning(node_watcher_received_unexpected_msg, [Unexpected])

  end,
  listen_loop().