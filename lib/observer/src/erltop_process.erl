%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% File    : erltop_process.erl
%% Author  : Olle Mattsson <olle@erix.ericsson.se>
%% Description : Trace and display of process info
%%
%% Created :  7 Jul 2008 by Olle Mattsson <olle@erix.ericsson.se>
%%-------------------------------------------------------------------

-module(erltop_process).
-author('olle@erix.ericsson.se').

-export([start/5, init/5, window_loop/1,
	module_info_window2/2, module_info_window/2]).

%-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include("erltop_defs.hrl").

-record(state, {frame,
		text_ctrl,
		process,
		process_info,
		trace_options,
		boxes,
		toggle_button,
		node,
		parent,
		io_device,
		traced_procs}).

-record(traced, {node,
		 pid,
		 flags}).

-define(OPTIONS, 1).
-define(SAVE_BUFFER, 2).
-define(CLOSE, 3).
-define(CLEAR, 4).
-define(MODULE_INFO, 5).
-define(ALL_LINKED, 6).
-define(LINKED_PROCESS, 7).
-define(KILL, 8).
-define(OK, 10).
-define(CANCEL, 11).
-define(LINKS, 20).
-define(TRAP_EXIT, 21).
-define(ERROR_HANDLER, 22).
-define(PRIORITY, 23).
-define(GROUP_LEADER, 24).
-define(GARBAGE_COLLECT, 25).
-define(HEAP_SIZE, 26).
-define(BINARY, 27).

-define(BACKTRACE, 28).
-define(CATCHLEVEL, 29).
-define(CURRENT_FUNCTION, 30).
-define(DICTIONARY, 31).
-define(LAST_CALL, 32).
-define(MEMORY, 33).
-define(MESSAGE_QUEUE_LEN, 35).
-define(MESSAGES, 36).
-define(MONITORED_BY, 37).
-define(MONITORS, 38).
-define(REDUCTIONS, 40).
-define(REGISTERED_NAME, 41).
-define(SEQ_TRACE_TOKEN, 42).
-define(STACK_SIZE, 43).
-define(STATUS, 44).
-define(SUSPENDING, 45).
-define(TOTAL_HEAP_SIZE, 46).
-define(TRACE, 47).

start(Process, Options, ProcessInfo, Node, Parent) ->
    spawn_link(?MODULE, init, [Process, Options, ProcessInfo, Node, Parent]).


init(Process, Options, ProcessInfo, Node, Parent) ->
    process_flag(trap_exit, true),
    Wx = wx:new(),
    State = 
	wx:batch(fun() ->
			 create_window(Wx, Process, Options, ProcessInfo, Node)
		 end),
    window_loop(State#state{parent = Parent,
			    node = Node}).


create_window(Wx, Process, Options, ProcessInfo, Node) ->
    %% Create the window
    Frame = wxFrame:new(Wx, ?wxID_ANY, title(Process)),
    wxFrame:connect(Frame, close_window,[{skip,true}]),
    Panel = wxPanel:new(Frame, []),
    Sizer = wxBoxSizer:new(?wxVERTICAL),

    %% Menues
    MenuBar = wxMenuBar:new(),
    create_menues(MenuBar),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxMenu:connect(Frame, command_menu_selected, []),

    %% Buttons
    ToggleButton = wxToggleButton:new(Panel, ?wxID_ANY, "Start Trace", []),
    wxSizer:add(Sizer, ToggleButton, [{proportion, 0},
				      {flag, ?wxEXPAND bor ?wxALL},
				      {border, 5}]),
    wxMenu:connect(ToggleButton, command_togglebutton_clicked, []),

    %% Text control
    TextCtrl = wxTextCtrl:new(Panel, ?wxID_ANY,
			      [{style,?wxTE_READONLY bor
				?wxTE_MULTILINE},
			       {size, {400, 300}}]),

    wxSizer:add(Sizer, TextCtrl, [{proportion, 1},
 				  {flag, ?wxEXPAND}]),
    wxTextCtrl:appendText(TextCtrl, info(Node, Process, ProcessInfo)),

    %% Display window
    wxWindow:setSizer(Panel, Sizer),
    wxSizer:fit(Sizer, Frame),
    wxFrame:show(Frame),
    #state{frame = Frame,
	   text_ctrl = TextCtrl, 
	   process = Process,
	   process_info =  ProcessInfo,
	   toggle_button = ToggleButton,
	   traced_procs = [],
	   trace_options = Options#trace_options{main_window = false}}.

create_menues(MenuBar) ->
    observer_wx:create_menu(
      [
       {"File", [#create_menu{id = ?OPTIONS, text = "&Options"},
		 #create_menu{id = ?SAVE_BUFFER, text = "&Save buffer..."},
		 #create_menu{id = ?CLOSE, text = "&Close"}
		]},
       {"View", [#create_menu{id = ?CLEAR, text = "&Clear buffer..."},
		 #create_menu{id = ?MODULE_INFO, text = "&Module info"}
		]},
       {"Trace", [#create_menu{id = ?ALL_LINKED, text = "&All linked processes"},
		  #create_menu{id = ?LINKED_PROCESS, text = "&Linked process..."},
		  #create_menu{id = ?KILL, text = "&Kill"}
		 ]}
      ],
      MenuBar).

info(Node, Process, Info) ->
    io:format("Info~p~n", [Info]),
    ProcessInfo = format_info(rpc:call(Node, erlang, process_info, [Process, Info])),
    lists:flatten([io_lib:format("--------------------------------------------------\n"
				 "Process info:\n"
				 "--------------------------------------------------\n"
				 "~s\n"
				 "---------------------------------------------------\n\n",
				 [ProcessInfo])]).


format_info(ProcessInfo) ->
    lists:map(fun({Text, Value}) ->
		      io_lib:format("~p:   ~p\n", [Text, Value])
	      end,
	      ProcessInfo).
		      







title({module, Mod}) ->
    lists:flatten([io_lib:format("Pman: Module info ~p", [Mod])]);
title({shell,  Sh} ) ->
    lists:flatten([io_lib:format("Pman: Shell process ~p on ~p",
				 [Sh,node(Sh)])]);
title(Sh) ->
    lists:flatten([io_lib:format("Pman: Process ~p on ~p",
				 [Sh, node(Sh)]),name(Sh)]).
name(Pid) ->
    case pman_process:pinfo(Pid, registered_name) of
	undefined -> 
	    "";
	Name ->
	    lists:flatten([io_lib:format("[~p]", [Name])])
    end.


window_loop(State) ->
    io:format("windowloop~n"),
    receive
	{checked, Opt, ProcessInfo, _Interval} -> %% Trace to file checked. When pressed OK erltop:otptions
	    State2 = State#state{trace_options = Opt,
				 process_info = ProcessInfo},
	    ?MODULE:window_loop(State2);
%%%-----------------------------------------------------------------
	#wx{id = ?CLOSE, event = #wxCommand{type = command_menu_selected}} ->
	    %% io:format("Shutdown. ~p\n", [self()]),
	    exit(shutdown);
%%%-----------------------------------------------------------------
	#wx{id = ?OPTIONS, event = #wxCommand{type = command_menu_selected}} ->
	    Self = self(),
	    erltop_options:start(State#state.trace_options,
				 State#state.process_info, 
				 5000,
				 Self),
	    ?MODULE:window_loop(State);
%%%-----------------------------------------------------------------
	#wx{id = ?MODULE_INFO, event = #wxCommand{type = command_menu_selected}} ->
	    module_info_window(State#state.node, State#state.process),
	    ?MODULE:window_loop(State);
%%%-----------------------------------------------------------------
	#wx{id = ?CLEAR, event = #wxCommand{type = command_menu_selected}} ->
	    wxTextCtrl:clear(State#state.text_ctrl),
	    wxTextCtrl:appendText(State#state.text_ctrl, info(State#state.node,
							      State#state.process,
							      State#state.process_info)),
	    ?MODULE:window_loop(State);
%%%-----------------------------------------------------------------
	#wx{id = ?SAVE_BUFFER, event = #wxCommand{type = command_menu_selected}} ->
	    Dialog = wxFileDialog:new(State#state.frame),
	    wxFileDialog:show(Dialog),	
	    case wxFileDialog:showModal(Dialog) of
		?wxID_OK ->
		    Path = wxFileDialog:getPath(Dialog),
		    case filelib:is_file(Path) of
			false ->
			    wxTextCtrl:saveFile(State#state.text_ctrl, [{file, Path}]),
			    wxTextCtrl:appendText(State#state.text_ctrl, "Saved to: " ++  Path ++ "\n");
			true ->
			    wxTextCtrl:appendText(State#state.text_ctrl, "File already exists " ++  Path ++ "\n")
		    end;
		_ -> ok
	    end,
	    ?MODULE:window_loop(State);
%%%-----------------------------------------------------------------
	#wx{event = #wxClose{type = close_window}} ->
	    %% io:format("Shutdown. ~p\n", [self()]),
	    exit(shutdown);
%%%-----------------------------------------------------------------
	#wx{id = ?ALL_LINKED, event = #wxCommand{type = command_menu_selected}} ->  %Ofärdig?
	    ?MODULE:window_loop(State);
	#wx{id = ?LINKED_PROCESS, event = #wxCommand{type = command_menu_selected}} -> %Ofärdig?
	    ?MODULE:window_loop(State);
	#wx{id = ?KILL, event = #wxCommand{type = command_menu_selected}} ->
	    exit(State#state.process, kill),
	    exit(shutdown);
	#wx{id = Id, event = #wxCommand{type = command_menu_selected, commandInt = Int}} -> %Onödig?
	    io:format("Menu selected med commandint: ~p~n", [Int]),
	    State2 = show_info(Id, Int, State),
	    ?MODULE:window_loop(State2);
%%%-----------------------------------------------------------------
	#wx{event = #wxCommand{type = command_togglebutton_clicked, commandInt = 1}} ->
	    Checked = State#state.trace_options,
	    Node = State#state.node,
	    TracedProcs = State#state.traced_procs,
	    State2 = State,
		%% case Checked#trace_options.to_file of
		%%     false ->
		%% 	State;
		%%     true ->
		%% 	{ok, IODevice} = file:open(Checked#trace_options.file, [append]),
		%% 	State#state{io_device = IODevice}
		%% end,
	    TracedProcs2 = set_trace(Node, State2#state.trace_options, true, State2#state.process, TracedProcs),
	    wxTextCtrl:appendText(State2#state.text_ctrl, "Start Trace:\n"),
	    wxToggleButton:setLabel(State2#state.toggle_button, "Stop Trace"),
	    ?MODULE:window_loop(State2#state{traced_procs = TracedProcs2});
%%%-----------------------------------------------------------------
	#wx{event = #wxCommand{type = command_togglebutton_clicked, commandInt = 0}} ->
	    Checked = State#state.trace_options,
	    Node = State#state.node,
	    TracedProcs = State#state.traced_procs,
	    %% case Checked#trace_options.to_file of
	    %% 	false ->
	    %% 	    ignore;
	    %% 	true ->
	    %% 	    file:close(State#state.io_device)
	    %% end,
	    TracedProcs2 = set_trace(Node, State#state.trace_options, false, State#state.process, TracedProcs),
	    wxTextCtrl:appendText(State#state.text_ctrl, "Stop Trace.\n"),
	    wxToggleButton:setLabel(State#state.toggle_button, "Start Trace"),
	    ?MODULE:window_loop(State#state{traced_procs = TracedProcs2});
%%%-----------------------------------------------------------------
	#wx{event = What} ->
	    io:format("~p~p: WX: ~p ~n", [?MODULE, self(), What]),
	    ?MODULE:window_loop(State);
%%%-----------------------------------------------------------------
	{'EXIT', Pid, _} when Pid =:= State#state.parent ->
	    exit(shutdown);
	{'EXIT', _, _} ->
	    ?MODULE:window_loop(State);
%%%-----------------------------------------------------------------
	Tuple when is_tuple(Tuple) ->
	    io:format("incoming~n"),
	    Checked = State#state.trace_options,
	    Text = textformat(Tuple),
	    %% case Checked#trace_options.to_file of
	    %% 	false ->
	    %% 	    wxTextCtrl:appendText(State#state.text_ctrl, lists:flatten(Text));
	    %% 	true ->
	    %% 	    file:write(State#state.io_device, list_to_binary(lists:flatten(Text)))
	    %% end,
	    ?MODULE:window_loop(State);
%%%-----------------------------------------------------------------
	Any ->
	    io:format("~p~p: received unexpected message: ~p\n", [?MODULE, self(), Any]),
	    ?MODULE:window_loop(State)
%%%-----------------------------------------------------------------
    end.

module_info_window(Node, Process) ->
    spawn_link(?MODULE, module_info_window2, [Node, Process]).

module_info_window2(Node, Process) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, lists:flatten([io_lib:format("Module info: ~p", [Process])])),
    Panel = wxPanel:new(Frame, []),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    TextCtrl = wxTextCtrl:new(Panel, ?wxID_ANY,
			      [{style,?wxTE_READONLY bor
				?wxTE_MULTILINE},
			       {size, {400, 300}}]),
    wxSizer:add(Sizer, TextCtrl, [{proportion, 1},
 				  {flag, ?wxEXPAND}]),
    {initial_call, {Module, _, _}} = rpc:call(Node, erlang, process_info, [Process, initial_call]),
    ModuleInfo = Module:module_info(),
    wxTextCtrl:appendText(TextCtrl, lists:flatten([io_lib:format("~p", [ModuleInfo])])),
    

    wxWindow:setSizer(Panel, Sizer),
    wxSizer:fit(Sizer, Frame),
    wxFrame:show(Frame),
    window_loop(#state{}).


show_info(Id, Int, State) ->
    ProcInfo = State#state.process_info,
    ProcInfo2 = 
	case Int of
	    1 -> lists:sort([info_id_to_item(Id) | ProcInfo]);
	    0 -> lists:delete(info_id_to_item(Id), ProcInfo)
	end,

    wxTextCtrl:clear(State#state.text_ctrl),
    wxTextCtrl:appendText(State#state.text_ctrl, info(State#state.node,
						      State#state.process,
						      ProcInfo2)),
    State#state{process_info = ProcInfo2}.

info_id_to_item(Id) ->	    
    case Id of
	?LINKS              -> 'links';
	?TRAP_EXIT          -> 'trap_exit';
	?ERROR_HANDLER      -> 'error_handler';
	?PRIORITY           -> 'priority';
	?GROUP_LEADER       -> 'group_leader';
	?GARBAGE_COLLECT    -> 'garbage_collection';
	?HEAP_SIZE          -> 'heap_size';
	?BINARY             -> 'binary';
	?BACKTRACE          -> 'backtrace';
	?CATCHLEVEL         -> 'catchlevel';
	?CURRENT_FUNCTION   -> 'current_function';
	?DICTIONARY         -> 'dictionary';
	?LAST_CALL          -> 'last_calls';
	?MEMORY             -> 'memory';
	?MESSAGES           -> 'messages';
	?MESSAGE_QUEUE_LEN  -> 'message_queue_len';
	?MONITORS           -> 'monitors';
	?MONITORED_BY       -> 'monitored_by';
	?REDUCTIONS         -> 'reductions';
	?REGISTERED_NAME    -> 'registered_name';
	?SEQ_TRACE_TOKEN    -> 'sequential_trace_token';
	?STACK_SIZE         -> 'stack_size';
	?STATUS             -> 'status';
	?SUSPENDING         -> 'suspending';
	?TOTAL_HEAP_SIZE    -> 'total_heap_size';
	?TRACE              -> 'trace'	    
    end.

set_trace(_, _, false, Process, TracedProcs) ->
    dbg:stop_clear(),
    {traced, ToRemove} = get_traced_pid(Process, TracedProcs),
    UpdTracedProcs = remove_traced_pid(ToRemove, TracedProcs),
    io:format("~p~n", [UpdTracedProcs]),
    
    case UpdTracedProcs of
	[] ->
	    [];
	_ ->
	    lists:foreach(fun(TraceRecord) ->
				  start_tracing(TraceRecord)
			  end,
			  UpdTracedProcs),
	    UpdTracedProcs
    end;
set_trace(Node, 
	  #trace_options{send = Send, treceive = Receive, functions = Functions,
			 events = Events, on_1st_spawn = On1Spawn,
			 on_all_spawn = AllSpawn, on_1st_link = On1Link,
			 on_all_link = AllLink}, 
	  true, Process, TracedProcs) ->

    case get_traced_pid(Process, TracedProcs) of
	{traced, _} ->
	    io:format("~p is already being traced.~n", [Process]),
	    TracedProcs;
	untraced ->
	    io:format("Starting trace of: ~p~n", [Process]),
	    Recs = [{Send, send},
		    {Receive, 'receive'},
		    {Functions, call}, 
		    {Events, procs},
		    {On1Spawn, set_on_first_spawn},
		    {AllSpawn, set_on_spawn},
		    {On1Link, set_on_first_link},
		    {AllLink, set_on_link}],
	    
	    Flags = [Assoc || {true, Assoc} <- Recs],
	    Traced = #traced{node = Node, pid = Process, flags = Flags},
	    start_tracing(Traced),
	    [Traced | TracedProcs]
    end.
%%    erlang:trace(Process, How, lists:flatten([{tracer, self()} | Flags])).



textformat({died, Pid}) ->
    io_lib:format("~w Process died.~n",[Pid]);
textformat({shell_died, Old, New}) ->
    io_lib:format("~w Shell Process died. Restarted as ~w~n~n",[Old,New]);
textformat({trace, From, 'receive', Msg}) ->
    io_lib:format("~w: rec   ~s~n", [From,
				     tuple_space(Msg)]);
textformat({trace, From, send, Msg, To}) ->
    io_lib:format("~w:  !    To: ~w Msg: ~s~n", [From,
						 To,
						 tuple_space(Msg)]);
textformat({trace, From, call, Func}) ->
    io_lib:format("~w: call  ~s~n",[From, ffunc(Func)]);
textformat({trace, From, spawn, Data}) ->
    io_lib:format("~w: spawn ~p~n", [From, Data]);
textformat({trace, From, link, Data}) ->
    io_lib:format("~w: link  ~p~n", [From,  Data]);
textformat({trace, From, unlink, Data}) ->
    io_lib:format("~w: U-lnk ~p~n", [From,  Data]);

textformat({trace, From, Op, Data}) ->
    io_lib:format("~w: ~w   ~p~n", [From, Op, Data]);

textformat({print, Format, Args}) ->
    io_lib:format(Format, Args);
textformat(Other) ->
    io_lib:format("~p~n",[Other]).


tuple_space(X) when is_tuple(X) -> print(size(X), X, "}");
tuple_space(X)                  -> io_lib:format("~p",[X]).


ffunc({M,F, Argl}) ->
    io_lib:format("~w:~w(~s)", [M, F, fargs(Argl)]);
ffunc(X) -> tuple_space(X).

fargs([]) -> [];
fargs([A]) -> tuple_space(A);  %% last arg
fargs([A|Args]) -> [tuple_space(A),", "|fargs(Args)].

print(0 , _X, Buff) -> ["{"|Buff];
print(1 , X, Buff) -> 
    Str =  tuple_space(element(1, X)),
    ["{",Str|Buff];
print(Num, X, Buff) ->
    Str =  tuple_space(element(Num, X)),
    print(Num-1, X, [", ",Str|Buff]).


get_traced_pid(_Pid, []) ->
    untraced;
get_traced_pid(Pid, [#traced{pid = TracedPid} = Traced | _Rest]) when Pid =:= TracedPid ->
    {traced, Traced};
get_traced_pid(Pid, [ _H | Rest]) ->
    get_traced_pid(Pid, Rest).


start_tracing(#traced{node = Node, pid = Pid, flags = Flags}) ->
    MyPid = self(),
    HandlerFun = fun(NewMsg, _) ->
			 io:format("NewMsg: ~p~n", [NewMsg]),
			 MyPid ! NewMsg
		 end,
    dbg:tracer(Node, process, {HandlerFun, []}),
    dbg:p(Pid, lists:flatten(Flags)).

remove_traced_pid(#traced{node = Node, pid = Pid, flags = Flags}, ListOfTraced) ->
    lists:filter(fun(#traced{node = RecNode, pid = RecPid, flags = RecFlags}) ->
			 ((Node =/= RecNode) andalso (Pid =/= RecPid)) 
			     andalso (Flags =/= RecFlags)
		 end,
		 ListOfTraced).
