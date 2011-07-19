%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011. All Rights Reserved.
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
-module(observer_pro_wx).

-behaviour(wx_object).

-export([start_link/2]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2, to_str/1]).

-include_lib("wx/include/wx.hrl").
-include_lib("runtime_tools/include/observer_backend.hrl").

-include("observer_defs.hrl").

%% Defines
-define(OBS_PRO, etop).

-define(COL_PID,  0).
-define(COL_NAME, 1).
-define(COL_MEM, 2).
-define(COL_TIME,  3).
-define(COL_REDS, 4).
-define(COL_FUN,  5).
-define(COL_MSG,  6).

-define(ID_KILL, 200).
-define(ID_VIEW, 201).
-define(ID_PROC, 202).
-define(ID_REFRESH, 203).
-define(ID_DUMP_TO_FILE, 204).
-define(ID_TRACEMENU, 208).
-define(ID_TRACE_ALL_MENU, 209).
-define(ID_TRACE_NEW_MENU, 210).
-define(ID_OPTIONS, 211).
-define(ID_SAVE_OPT, 212).
-define(ID_MODULE_INFO, 213).
-define(OK, 214).
-define(ID_SYSHIDE, 215).
-define(ID_HIDENEW, 216).

-define(FIRST_NODES_MENU_ID, 1000).
-define(LAST_NODES_MENU_ID,  2000).

%% Records
-record(attrs, {even, odd, deleted, changed, searched}).

-record(holder, {parent,
		 procinfo,
		 attrs,
		 sort_order}).

-record(pro_wx_state, {parent,
		       popup_menu, 
		       grid, 
		       frame,
		       parent_notebook,
		       trace_options = #trace_options{},
		       match_specs = [], %%fixa
		       options_pid, %%onödig?
		       opened = [], %%onödig?
		       process_info_settings, %%onödig
		       interval,
		       hide_system, %%onödig?
		       hide_modules, %%onödig?
		       hide_pids,%%onödig?
		       tracemenu_opened,
		       procinfo_menu_pids = [],
		       node = node(),
		       sort_order = {#etop_proc_info.reds, decr},
		       holder}).

start_link(Notebook, Parent) ->
    wx_object:start_link(?MODULE, [Notebook, Parent], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Notebook, Parent]) ->
    process_flag(trap_exit, true),
    Config = etop:start(self()),
    Info = etop:update(Config),
    Attrs = create_attrs(),
    Self = self(),
    SortOrder = {?COL_REDS, decr},
    Holder = spawn_link(fun() ->
				init_table_holder(Self, 
						  Info#etop_info.procinfo, 
						  Attrs,
						  SortOrder
						 )
			end),
    Count = length(Info#etop_info.procinfo),
    {ProPanel, State} = setup(Notebook, Parent, Holder, Count),
    refresh_grid(Holder),
    erlang:send_after(State#pro_wx_state.interval, self(), time_to_update),
    {ProPanel, State}.

setup(Notebook, Parent, Holder, Count) ->
    ProPanel = wxPanel:new(Notebook, []),
    Menu = create_popup_menu(),

    Grid = create_list_box(ProPanel, Holder, Count),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Grid, [{flag, ?wxEXPAND bor ?wxALL},
			      {proportion, 1},
			      {border,4}]),
    
    wxWindow:setSizer(ProPanel, Sizer),
    
    Interval = 5000, %erltop:getopt(intv, Config),
    
    ProcessInfo = [ % sorted
		    error_handler,
		    garbage_collection,
		    group_leader,
		    heap_size,
		    links,
		    priority,
		    trap_exit,
		    message_queue_len],
    
    State =  #pro_wx_state{parent = Parent,
			   popup_menu  = Menu,
			   grid        = Grid,
			   frame       = ProPanel,
			   parent_notebook = Notebook,
			   process_info_settings = ProcessInfo,
			   interval = Interval,
			   tracemenu_opened = false,
			   holder = Holder}, 
    {ProPanel, State}.
%% UI-creation

create_pro_menu(Parent) ->
    MenuEntries = [{"View",
		    [#create_menu{id = ?ID_REFRESH, text = "Refresh"},
		     #create_menu{id = ?ID_SYSHIDE, text = "Hide system processes"},
		     #create_menu{id = ?ID_HIDENEW, text = "Auto-hide new"}]},
		   {"Options",
		    [#create_menu{id = ?ID_DUMP_TO_FILE, text = "Dump to file"},
		     #create_menu{id = ?ID_OPTIONS, text = "Options"}]},
		   {"Trace",
		    [#create_menu{id = ?ID_TRACEMENU, text = "Trace selected processes"},
		     #create_menu{id = ?ID_TRACE_ALL_MENU, text = "Trace all processes"},
		     #create_menu{id = ?ID_TRACE_NEW_MENU, text = "Trace new processes"}]}
		  ],
    observer_wx:create_menus(Parent, MenuEntries).

create_popup_menu() ->
    PopupMenu = wxMenu:new(),
    wxMenu:append(PopupMenu, ?ID_VIEW,  "View code"),
    wxMenu:append(PopupMenu, ?ID_PROC,  "View process information"),
    wxMenu:append(PopupMenu, ?ID_MODULE_INFO,  "Module info"),
    wxMenu:appendSeparator(PopupMenu),
    wxMenu:append(PopupMenu, ?ID_KILL,  "Kill"),

    wxMenu:connect(PopupMenu, command_menu_selected),

    PopupMenu.


create_list_box(Panel, Holder, Count) ->
    Style = ?wxLC_REPORT bor ?wxLC_VIRTUAL,
    ListCtrl = wxListCtrl:new(Panel, [{style, Style},
				      {onGetItemText,
				       fun(_, Item, Col) -> get_row(Holder, Item, Col) end},
				      {onGetItemAttr,
				       fun(_, Item) -> get_attr(Holder, Item) end}
				     ]),
    %%{size    , {600, 500}}]),
    Li = wxListItem:new(),
    AddListEntry = fun({Name, Align, DefSize}, Col) ->
    			   wxListItem:setText(Li, Name),
    			   wxListItem:setAlign(Li, Align),
    			   wxListCtrl:insertColumn(ListCtrl, Col, Li),
    			   wxListCtrl:setColumnWidth(ListCtrl, Col, DefSize),
    			   Col + 1
    		   end,
    ListItems = [{"Pid", ?wxLIST_FORMAT_CENTRE,  120},
    		 {"Name or initial fun", ?wxLIST_FORMAT_LEFT, 200},
		 {"Memory", ?wxLIST_FORMAT_LEFT, 50},
		 {"Time", ?wxLIST_FORMAT_CENTRE, 50},
		 {"Reds", ?wxLIST_FORMAT_CENTRE, 50},
		 {"Current function", ?wxLIST_FORMAT_LEFT,  200},
		 {"Msgs",  ?wxLIST_FORMAT_LEFT, 50}
		],
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),
    
    wxListCtrl:connect(ListCtrl, size, [{skip, true}]),
    wxListCtrl:connect(ListCtrl, command_list_item_activated),
    wxListCtrl:connect(ListCtrl, command_list_item_right_click),
    wxListCtrl:connect(ListCtrl, command_list_col_click),
    wxListCtrl:connect(ListCtrl, command_list_item_selected),
    wxListCtrl:setItemCount(ListCtrl, Count),
    ListCtrl.


change_node(Node) ->
    etop_server ! {config, {node, Node}}.
    
refresh_grid(Holder) ->
    etop_server ! {update, Holder}.

lookup_pid(ProcInfo, Pid) ->
    [Ret] = lists:filter(fun(#etop_proc_info{pid = Pid2}) ->
				 Pid =:= Pid2
			 end,
			 ProcInfo),
    Ret.

module_code(Module) ->
    case filename:find_src(Module) of
        {error, _} ->
            no_code;
        {CodeSrc, _} ->
            case file:read_file_info(CodeSrc ++ ".erl") of
                {error, _} -> 
                    no_code;
                _ ->
                    {src, CodeSrc ++ ".erl"}
            end
    end.

create_code_frame(Parent) ->
    Frame = wxFrame:new(Parent, ?wxID_ANY, "View"),
    TxtCtrl = wxTextCtrl:new(Frame, ?wxID_ANY, 
			     [{style,?wxTE_READONLY bor ?wxTE_MULTILINE}]),
    {Frame, TxtCtrl}.

update_sort_order(Col, 
		  #pro_wx_state{sort_order = {Sort,Dir}} = State) ->
    case map_sort_order(Col) of
	Sort when Dir =:= incr -> State#pro_wx_state{sort_order = {Sort,decr}};
	Sort when Dir =:= decr -> State#pro_wx_state{sort_order = {Sort,incr}};
	New ->  State#pro_wx_state{sort_order = {New,Dir}}
    end.

map_sort_order(Col) ->    
    case Col of
	?COL_PID  -> #etop_proc_info.pid;
	?COL_NAME -> #etop_proc_info.name;
	?COL_TIME -> #etop_proc_info.runtime;
	?COL_REDS -> #etop_proc_info.reds;
	?COL_MEM  -> #etop_proc_info.mem;
	?COL_MSG  -> #etop_proc_info.mq;
	?COL_FUN  -> #etop_proc_info.cf
    end.

opt_warning(Pid, Reason) ->
    case Reason of
	normal ->
	    ok;
	shutdown ->
	    ok;
	Bad ->
	    error_logger:format("~p~p: received: ~p\n",
				[?MODULE, self(), {'EXIT', Pid, Bad}])
    end.



to_str(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_str({A, B}) ->
    lists:concat([A, ":", B]);
to_str({M,F,A}) ->
    lists:concat([M, ":", F, "/", A]);
to_str(Value) when is_list(Value) ->
    case lists:all(fun(X) -> is_integer(X) end, Value) of
	true -> Value;
	false ->
	    lists:foldl(fun(X, Acc) -> 
				to_str(X) ++ " " ++ Acc end, 
			"", Value)
    end;
to_str(Port) when is_port(Port) ->
    erlang:port_to_list(Port);
to_str(Pid) when is_pid(Pid) ->
    pid_to_list(Pid);
to_str(No) when is_integer(No) ->
    integer_to_list(No);
to_str(ShouldNotGetHere) ->
    erlang:error({?MODULE, to_str, ShouldNotGetHere}).
    
get_selected_items(Grid) ->
    get_selected_items(Grid, -1, []).
    
get_selected_items(Grid, Index, ItemAcc) ->
    Item = wxListCtrl:getNextItem(Grid, Index, 
				  [{geometry, ?wxLIST_NEXT_ALL}, {state, ?wxLIST_STATE_SELECTED}]),
    case Item of
	-1 ->
	    lists:reverse(ItemAcc);
	_ ->
	    get_selected_items(Grid, Item, [Item+1 | ItemAcc])
    end.

%%%%%%%%%%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({holder_updated, Count}, #pro_wx_state{grid = Grid} = State) ->
    wxListCtrl:setItemCount(Grid, Count),
    wxListCtrl:refreshItems(Grid, 0, wxListCtrl:getItemCount(Grid)),
    {noreply, State};

handle_info(time_to_update, #pro_wx_state{holder = Holder} = State) ->
    refresh_grid(Holder),
    erlang:send_after(State#pro_wx_state.interval, self(), time_to_update),
    {noreply, State};

handle_info({updated_traceopt, TraceOpts}, State) -> %When user updates traceoptions from tracemenu..
    {noreply, State#pro_wx_state{trace_options = TraceOpts}};

handle_info(tracemenu_closed, State) -> %When tracemenu terminates..
    {noreply, State#pro_wx_state{tracemenu_opened = false}};

handle_info({procinfo_menu_closed, Pid}, 
	    #pro_wx_state{procinfo_menu_pids = MenuPids} = State) ->
    NewPids = lists:delete(Pid, MenuPids),
    {noreply, State#pro_wx_state{procinfo_menu_pids = NewPids}};

handle_info({checked, Opt, ProcessInfo, Interval}, State) -> %från opts
    State2 = State#pro_wx_state{trace_options = Opt,
				process_info_settings  = ProcessInfo,
				interval = Interval},
    {noreply, State2};

handle_info({save, File}, State) -> %från options
    State#pro_wx_state.parent ! {statusbar, "Options saved: " ++ File},
    {noreply, State};

handle_info({active, Node}, #pro_wx_state{holder = Holder,
					  parent = Parent} = State) ->
    create_pro_menu(Parent),
    change_node(Node),
    refresh_grid(Holder),
    {noreply, State#pro_wx_state{node = Node}};

handle_info({'EXIT', Pid, Reason}, State) when Pid =:= State#pro_wx_state.options_pid ->
    opt_warning(Pid, Reason),
    {noreply, State#pro_wx_state{options_pid = undefined}};

handle_info({'EXIT', Pid, Reason}, State) ->
    opt_warning(Pid, Reason),
    case lists:keysearch(Pid, #pid.window, State#pro_wx_state.opened) of
	{value, Tuple} ->
	    Opened = lists:delete(Tuple, State#pro_wx_state.opened),
	    {noreply, State#pro_wx_state{opened = Opened}};
	false ->
	    {noreply, State}
    end;

handle_info({node, Node}, #pro_wx_state{holder = Holder} = State) ->
    change_node(Node),
    refresh_grid(Holder),
    {noreply, State#pro_wx_state{node = Node}};

handle_info(Info, State) ->
    io:format("~p, ~p, Handled unexpected info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    etop:stop(),
    io:format("~p terminating. Reason: ~p~n", [?MODULE, Reason]),
    ok.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.


handle_call(Msg, _From, State) ->
    io:format("~p~p: Got Call ~p~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.


handle_cast(Msg, State) ->
    io:format("~p ~p: Unhandled cast ~p~n", [?MODULE, ?LINE, Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%LOOP%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% handle_event(#wx{id = ?ID_DUMP_TO_FILE}, State) ->  %%Fixa
%%     io:format("~p:~p, Klickade på dump to file~n", [?MODULE, ?LINE]),
%%     FD  =  wxFileDialog:new(State#pro_wx_state.frame, [{style,?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT}]),
%%     case wxFileDialog:showModal(FD) of
%% 	?wxID_OK ->
%% 	    Path = wxFileDialog:getPath(FD),
%% 	    io:format("It should Dump to file ~p~n", [Path]);
%% 	_ -> ok
%%     end,
%%     {noreply, State};

%% handle_event(#wx{id = ?ID_OPTIONS}, #pro_wx_state{trace_options = Options} = State) -> %%Fixa
%%     io:format("~p:~p, Klickade options~n", [?MODULE, ?LINE]),
%%     io:format("Options_pid: ~p~n", [State#pro_wx_state.options_pid]),
%%     case State#pro_wx_state.options_pid of
%% 	undefined ->
%% 	    Pid = erltop_options:start(Options,
%% 				       State#pro_wx_state.process_info_settings,
%% 				       State#pro_wx_state.interval,
%% 				       self()),
%% 	    io:format("Options: ~p~n, Process_info: ~p~n, Interval: ~p~n",
%% 		      [Options, State#pro_wx_state.process_info_settings, State#pro_wx_state.interval]),
%% 	    {noreply, State#pro_wx_state{options_pid = Pid}};
%% 	_ ->
%% 	    {noreply, State}
%%     end;

handle_event(#wx{id = ?ID_REFRESH, event = #wxCommand{type = command_menu_selected}}, 
	     #pro_wx_state{holder = Holder} = State) ->
    io:format("~p:~p, Klickade på refresh~n", [?MODULE, ?LINE]),
    refresh_grid(Holder),
    {noreply, State};

%% handle_event(#wx{id = ?ID_KILL}, #pro_wx_state{selected = Sel} = State) ->
%%     case Sel of
%% 	undefined -> ignore;
%%  	_ -> exit(State#pro_wx_state.selected, kill)
%%     end,
%%     {noreply, State};

%% handle_event(#wx{id = ?ID_MODULE_INFO, 
%% 		 event = #wxCommand{type = command_menu_selected}}, 
%% 	     #pro_wx_state{node = Node, selected = Sel} = State)  ->
%%     erltop_process:module_info_window(Node, Sel), %%%%%%% erltop process byt namn
%%     {noreply, State};

%% handle_event(#wx{id = ?ID_VIEW} = Event,
%% 	     #pro_wx_state{holder = Holder,
%% 			   selected = Sel} = State) ->
%%     Holder ! {get_info, self()},
%%     Info = receive
%% 	       I ->
%% 		   I
%% 	   end,
%%     io:format("Event: ~p~n", [Event]),
%%     #etop_proc_info{cf = {M,_,_}} = lookup_pid(Info, Sel),
%%     case module_code(M) of
%% 	no_code ->     ok;
%% 	{src, File} ->
%% 	    {CodeFrame, CodeView} = create_code_frame(State#pro_wx_state.frame),
%% 	    wxTextCtrl:loadFile(CodeView, File),
%% 	    FileName = filename:basename(File),
%% 	    wxFrame:setTitle(CodeFrame, "Viewing: " ++ FileName),
%% 	    wxFrame:show(CodeFrame)
%%     end,
%%     {noreply, State};

%% handle_event(#wx{id = ?ID_PROC}, 
%% 	     #pro_wx_state{node = Node,
%% 			   opened = Opened,
%% 			   selected = Sel,
%% 			   process_info_settings = PI,
%% 			   trace_options = Opts} = State) ->
    
%%     case lists:keysearch(Sel, #pid.traced, Opened) of
%% 	false ->
%% 	    Pid = erltop_process:start(Sel, Opts,
%% 				       PI, Node,
%% 				       self()),
%% 	    {noreply, 
%% 	     State#pro_wx_state{opened = [#pid{traced = State#pro_wx_state.selected,window = Pid} 
%% 					  | State#pro_wx_state.opened]}};
%% 	{value, _Tuple} ->
%% 	    io:format("Already open\n", []),
%% 	    {noreply, State}
%%     end;

handle_event(#wx{id = ?ID_TRACEMENU, event = #wxCommand{type = command_menu_selected}}, 
	     #pro_wx_state{node = Node,
			   trace_options = Options,
			   holder = Holder,
			   grid = Grid,
			   tracemenu_opened = false, 
			   frame = Frame} = State) ->
    IndexList = get_selected_items(Grid),
    PidList = get_selected_pids(Holder, IndexList),
    observer_trace_wx:start(Node,
     			    PidList,
     			    Options,
     			    Frame,
     			    self()),
    {noreply,  State#pro_wx_state{tracemenu_opened = true}};

handle_event(#wx{id = ?ID_TRACE_ALL_MENU, event = #wxCommand{type = command_menu_selected}},
	     #pro_wx_state{node = Node,
			   trace_options = Options,
			   tracemenu_opened = false,
			   frame = Frame} = State) ->

        observer_trace_wx:start(Node,
			    all,
			    Options,
			    Frame,
			    self()),
    {noreply, State#pro_wx_state{tracemenu_opened = true}};


handle_event(#wx{id = ?ID_TRACE_NEW_MENU, event = #wxCommand{type = command_menu_selected}},
	     #pro_wx_state{node = Node,
			   trace_options = Options,
			   tracemenu_opened = false,
			   frame = Frame} = State) ->
    
    observer_trace_wx:start(Node,
			    new,
			    Options,
			    Frame,
			    self()),
    {noreply,  State#pro_wx_state{tracemenu_opened = true}};


handle_event(#wx{event=#wxSize{size={W,_}}}, 
	     #pro_wx_state{grid=Grid} = State) ->
    wx:batch(fun() ->
		     Cols = wxListCtrl:getColumnCount(Grid),
		     Last = lists:foldl(fun(I, Last) ->
						Last - wxListCtrl:getColumnWidth(Grid, I)
					end, W-2, lists:seq(0, Cols - 2)),
		     Size = max(200, Last),
		     wxListCtrl:setColumnWidth(Grid, Cols-1, Size)
	     end),
    {noreply, State};

%% handle_event(#wx{event = #wxList{type = command_list_item_right_click, 
%% 				 itemIndex = Row}}, 
%% 	     #pro_wx_state{grid = Grid,
%% 			   popup_menu = PopupMenu} = State) -> %Row selected
%%     Pid = wxListCtrl:getItemText(Grid, Row),
%%     wxWindow:popupMenu(Grid, PopupMenu, []),
%%     {noreply, State#pro_wx_state{selected = list_to_pid(Pid)}};

handle_event(#wx{event = #wxList{type = command_list_item_selected,
				 itemIndex = Row}}, 
	     #pro_wx_state{holder = Holder} = State) ->
    ProcInfo = get_row(Holder, Row, all),
    Str = lists:flatten(io_lib:format("~p", [ProcInfo])),
    io:format("List item selected ~p~n", [Str]),
    %%{noreply, State#pro_wx_state{selected = list_to_pid(Pid)}};
    {noreply, State};

handle_event(#wx{event = #wxList{type = command_list_col_click, col = Col}}, 
	     #pro_wx_state{holder = Holder} = State) ->
    NewState = update_sort_order(Col, State),
    Holder ! {change_sort_order, NewState#pro_wx_state.sort_order},
    refresh_grid(Holder),
    {noreply, NewState};

handle_event(#wx{event = #wxList{type = command_list_item_activated,
				 itemIndex = Row}}, 
	     #pro_wx_state{frame = Frame,
			   holder = Holder,
			   node = Node,
			   procinfo_menu_pids= MenuPids} = State) ->
    SelectedPid = get_row(Holder, Row, pid),
    case lists:member(SelectedPid, MenuPids) of
	true ->
	    {noreply, State};
	false ->
	    observer_procinfo:start(Node, SelectedPid, Frame, self()),
	    {noreply, State#pro_wx_state{procinfo_menu_pids = [SelectedPid | MenuPids]}}
    end;

%% case lists:keysearch(State#pro_wx_state.selected, #pid.traced, State#pro_wx_state.opened) of
%% 	false ->
%% 	    Pid = erltop_process:start(State#pro_wx_state.selected,
%% 		   		       Options,
%% 		  		       State#pro_wx_state.process_info_settings, %%bort
%% 		   		       Node,
%% 		   		       self()),

%% 	    {noreply, {Config, Info, State#pro_wx_state{opened = [#pid{traced = State#pro_wx_state.selected,
%% 								window = Pid} | State#pro_wx_state.opened]}}};
%% 	{value, _Tuple} ->
%% 	    io:format("Already open\n", []),
%%{noreply, {ProcInfo,State}};
%% end;

handle_event(Event, State) ->
    io:format("~p~p, handle event ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.
























get_selected_pids(Holder, Indices) ->
    Ref = erlang:monitor(process, Holder),
    Holder ! {get_pids, self(), Indices},
    receive
	{'DOWN', Ref, _, _, _} -> [];
	{Holder, Res} ->
	    erlang:demonitor(Ref),
	    Res
    end.

get_row(Holder, Row, Column) ->
    Ref = erlang:monitor(process, Holder),
    Holder ! {get_row, self(), Row, Column},
    receive
    	{'DOWN', Ref, _, _, _} -> "";
    	{Holder, Res} ->
    	    erlang:demonitor(Ref),
    	    Res
    end.


get_attr(Holder, Item) ->
    Ref = erlang:monitor(process, Holder),
    Holder ! {get_attr, self(), Item},
    receive
	{'DOWN', Ref, _, _, _} -> "";
	{Holder, Res} ->
	    erlang:demonitor(Ref),
	    Res
    end.

init_table_holder(Parent, Info, Attrs, SortOrder) ->
    table_holder(#holder{parent = Parent,
			 procinfo = Info,
			 attrs = Attrs,
			 sort_order = SortOrder}).

table_holder(#holder{parent = Parent,
		     procinfo = Info,
		     attrs = Attrs,
		     sort_order = SortOrder} = S0) ->
    receive
	{get_row, From, Row, Col} ->
	    get_row(From, Row, Col, Info, SortOrder),
	    table_holder(S0);
	{get_attr, From, Row} ->
	    get_attr(From, Row, Attrs),
	    table_holder(S0);
	{get_info, From} ->
	    From ! Info,
	    table_holder(S0);
	{get_pids, From, Indices} ->
	    get_pids(From, Indices, Info, SortOrder),
	    table_holder(S0);
	{change_sort_order, SortOrder2} ->
	    table_holder(S0#holder{sort_order = SortOrder2});
	#etop_info{procinfo = ProcInfo} ->
	    Parent ! {holder_updated, length(ProcInfo)},
	    table_holder(S0#holder{procinfo = ProcInfo});
	What ->
	    io:format("Table holder got ~p~n",[What]),
	    table_holder(S0)
    end.


get_procinfo_data(?COL_PID, #etop_proc_info{pid = Pid}) ->
    Pid;
get_procinfo_data(?COL_NAME, #etop_proc_info{name = Name})  ->
    Name;
get_procinfo_data(?COL_MEM, #etop_proc_info{mem = Mem}) ->
    Mem;
get_procinfo_data(?COL_TIME, #etop_proc_info{runtime = RT}) ->
    RT;
get_procinfo_data(?COL_REDS, #etop_proc_info{reds = Reds}) ->
    Reds;
get_procinfo_data(?COL_FUN, #etop_proc_info{cf = CF}) ->
    CF;
get_procinfo_data(?COL_MSG, #etop_proc_info{mq = MQ}) ->
    MQ.



get_pids(From, Indices, ProcInfo, {Sort, Dir}) ->
    SortedInfo = sort_procinfo(ProcInfo, Sort, Dir),
    From ! {self(),
	    [X#etop_proc_info.pid || X <- 
					 [lists:nth(I, SortedInfo) || I <- Indices]]}.

get_row(From, Row, all, Info, {Sort, Dir}) ->
    SortedInfo = sort_procinfo(Info, Sort, Dir),
    ProcInfo = lists:nth(Row+1, SortedInfo),
    From ! {self(), ProcInfo};
get_row(From, Row, pid, Info, {Sort, Dir}) ->
    SortedInfo = sort_procinfo(Info, Sort, Dir),
    Pid = get_procinfo_data(?COL_PID, lists:nth(Row+1, SortedInfo)),
    From ! {self(), Pid};
get_row(From, Row, Col, Info, {Sort, Dir}) ->
    SortedInfo = sort_procinfo(Info, Sort, Dir),
    ProcInfo = lists:nth(Row+1, SortedInfo),
    Data = get_procinfo_data(Col, ProcInfo),
    From ! {self(), io_lib:format("~p", [Data])}.

get_attr(From, Row, Attrs) ->
    Attribute = case Row rem 2 =:= 0 of
		    true ->
			Attrs#attrs.even;
		    false ->
			Attrs#attrs.odd
		end,
    From ! {self(), Attribute}.

create_attrs() ->
    Font = wxSystemSettings:getFont(?wxSYS_DEFAULT_GUI_FONT),
    Text = wxSystemSettings:getColour(?wxSYS_COLOUR_LISTBOXTEXT),
    #attrs{even = wx:typeCast(wx:null(), wxListItemAttr),
	   odd  = wxListItemAttr:new(Text, {240,240,255}, Font),
	   searched = wxListItemAttr:new(Text, {235,215,90}, Font)
	  }.

sort_procinfo(Info, Key, incr) ->
    lists:keysort(Key, Info);
sort_procinfo(Info, Key, decr) ->
    lists:reverse(lists:keysort(Key, Info)).

