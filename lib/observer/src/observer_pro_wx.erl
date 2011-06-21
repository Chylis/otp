-module(observer_pro_wx).

-behaviour(wx_object).

-export([start_link/4]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).



-include_lib("wx/include/wx.hrl").
-include_lib("runtime_tools/include/observer_backend.hrl").

%% Defines
-define(OBS_PRO, erltop).
-define(OBS, observer_wx).

-define(COL_PID,  0).
-define(COL_NAME, 1).
-define(COL_TIME, 2).
-define(COL_REDS, 3).
-define(COL_MEM,  4).
-define(COL_MSG,  5).
-define(COL_FUN,  6).

-define(ID_KILL, 200).
-define(ID_VIEW, 201).
-define(ID_PROC, 202).
-define(ID_REFRESH, 203).
-define(ID_DUMP_TO_FILE, 204).
%%%-define(ID_ACCUMULATE, 206).
-define(ID_TRACE, 208).
-define(ID_OPTIONS, 209).
-define(ID_SAVE_OPT, 210).
-define(ID_MODULE_INFO, 212).
-define(OK, 211).
-define(ID_SYSHIDE, 214).
-define(ID_HIDENEW, 215).

%%%-define(MAX_PROCINFO_TEXT_LENGTH, 35).
%-define(NOTEBOOK, 20).

-define(FIRST_NODES_MENU_ID, 1000).
-define(LAST_NODES_MENU_ID,  2000).

%% Records
-record(pid, {window, traced}).

-record(trace_options, {send         = true,
			treceive     = true,
			functions    = true,
			events       = true,
			on_1st_spawn = true,
			on_all_spawn = false,
			on_1st_link  = true,
			on_all_link  = false,
			in_window    = true,
			to_file      = false,
			file         = "",
			main_window  = true}).


-record(pro_wx_state, {code_frame, 
		       code_view, 
		       proc_frame, 
		       popup_menu, 
		       grid, 
		       selected, 
		       frame,
		       menubar,
		       parent_notebook,
		       sort_order = {#etop_proc_info.reds, decr},
		       panel, trace_options = #trace_options{},
		       options_pid,
		       opened = [],
		       process_info,
		       interval,
		       statusbar,
		       hide_system,
		       hide_modules,
		       hide_pids,
		       nodes}).

-record(opts, {node=node(), port = 8415, accum = false, intv = 5000, lines = 30, 
	       width = 700, height = 340, sort = runtime, tracing = on,
	       %% Other state information
	       out_mod=erltop:output(graphical), out_proc, server, host, tracer, store, 
	       accum_tab, remote}).

-record(create_menu, 
	{id,
	 text,
	 type = append,
	 check = true
	}).


start_link(Notebook, Env, MenuBar, StatusBar) ->
    wx:set_env(Env),
    wx_object:start_link(?MODULE, [Notebook, MenuBar, StatusBar], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Notebook, MenuBar, StatusBar]) ->
    process_flag(trap_exit, true),
    Config = ?OBS_PRO:start(self()),
    {ProPanel, State} = setup(Notebook, MenuBar, StatusBar),
    Info = ?OBS_PRO:update(Config),
    refresh(Info, State),
    erlang:send_after(State#pro_wx_state.interval, self(), time_to_update),
    {ProPanel, {Config, Info, State}}.

setup(Notebook, MenuBar, StatusBar) ->
    ProPanel = wxPanel:new(Notebook, []),
    Menu = create_popup_menu(),
    Grid = create_list_box(ProPanel),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Grid, [{flag, ?wxEXPAND bor ?wxALL},
			      {proportion, 1},
			      {border,4}]),
    
    wxWindow:setSizer(ProPanel, Sizer),
    
    Interval = 5000, %erltop:getopt(intv, Config),

    
    DefaultProcessInfo = [ % sorted
			   error_handler,
			   garbage_collection,
			   group_leader,
			   heap_size,
			   links,
			   priority,
			   trap_exit,
			   message_queue_len],
    File = filename:join(os:getenv("HOME"), ".erlang_tools/pman.opts"),
    {Opt, ProcessInfo} =
	case filelib:is_file(File) of
	    true ->
		case file:consult(File) of
		    {ok, Terms} ->
			parse_options(Terms, #trace_options{}, []);
		    {error, Reason} ->
			io:format("Error:  ~p\n", [Reason]),
			{#trace_options{}, []}
		end;
	    false ->
		{#trace_options{}, []}
	end,
    ProcessInfo2 =
	case ProcessInfo of
	    [] ->
		DefaultProcessInfo;
	    Other ->
		Other
	end,
    
    State =  #pro_wx_state{popup_menu  = Menu,
			   grid        = Grid,
			   frame       = ProPanel,
			   parent_notebook = Notebook,
			   trace_options = Opt,
			   process_info = ProcessInfo2,
			   menubar = MenuBar,
			   statusbar = StatusBar,
			   interval = Interval,
			   nodes    = [node() | nodes()]}, %temporary nodes...!
    {ProPanel, State}.
%% UI-creation

create_pro_menu(MenuBar) ->
    clear_menu_bar(MenuBar),
    create_menu([#create_menu{id = ?ID_DUMP_TO_FILE, text = "&Dump to file"},
		 #create_menu{id = ?ID_OPTIONS, text = "&Options"},
		 #create_menu{id = ?ID_SAVE_OPT, text = "&Save options..."}],
		"&Options", MenuBar),
    create_menu([#create_menu{id = ?ID_REFRESH, text = "&Refresh"}, 
		 #create_menu{id = ?ID_SYSHIDE, text = "&Hide system processes"}, %TA HAND OM EVENTS
		 #create_menu{id = ?ID_HIDENEW, text = "&Auto-hide new"}],
		"View", MenuBar).

clear_menu_bar(MenuBar) ->
    Count = wxMenuBar:getMenuCount(MenuBar),
    remove_menu_item(MenuBar, Count).

remove_menu_item(_MenuBar, 2) ->
    ok;
remove_menu_item(MenuBar, Item) ->
    wxMenuBar:remove(MenuBar, Item),
    remove_menu_item(MenuBar, Item-1).


%%%%% create_node_info_ui(Panel) ->
%%%%%     Sizer  = wxBoxSizer:new(?wxVERTICAL),
%%%%%     create_check_box(Panel, Sizer, ?wxID_ANY, "Hide system processes"),
%%%%%     create_check_box(Panel, Sizer, ?wxID_ANY, "Auto-hide new"),
%%%%%     Text1 = wxStaticText:new(Panel, ?wxID_ANY, "# Hidden: ", []),
%%%%%     wxSizer:addSpacer(Sizer, 10),
%%%%%     wxSizer:add(Sizer, Text1, [{flag, ?wxLEFT}, {border, 10}]),
%%%%%     Sizer.


create_popup_menu() ->
    PopupMenu = wxMenu:new(),
    wxMenu:append(PopupMenu, ?ID_VIEW,  "View code"),
    wxMenu:append(PopupMenu, ?ID_PROC,  "View process information"),
    wxMenu:append(PopupMenu, ?ID_MODULE_INFO,  "Module info"),
    wxMenu:appendSeparator(PopupMenu),
    wxMenu:append(PopupMenu, ?ID_KILL,  "Kill"),

    wxMenu:connect(PopupMenu, command_menu_selected),

    PopupMenu.


create_list_box(Frame) ->
    ListCtrl = wxListCtrl:new(Frame, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL 
					   bor ?wxLC_HRULES },
				      {size, {600, 500}}]),
    LI = wxListItem:new(),
    wxListItem:setText(LI, "Pid"), 
    wxListItem:setAlign(LI, ?wxLIST_FORMAT_CENTRE),
    wxListCtrl:insertColumn(ListCtrl, 0, LI),
    wxListItem:setText(LI, "Name or initial fun"),
    wxListItem:setAlign(LI, ?wxLIST_FORMAT_LEFT),
    wxListCtrl:insertColumn(ListCtrl, 1, LI),
    wxListItem:setText(LI, "Time"), 
    wxListCtrl:insertColumn(ListCtrl, 2, LI),
    wxListItem:setAlign(LI, ?wxLIST_FORMAT_CENTRE),
    wxListItem:setText(LI, "Reds"), 
    wxListCtrl:insertColumn(ListCtrl, 3, LI),
    wxListItem:setText(LI, "Memory"), 
    wxListItem:setAlign(LI, ?wxLIST_FORMAT_LEFT),
    wxListCtrl:insertColumn(ListCtrl, 4, LI),
    wxListItem:setText(LI, "Msgs"), 
    wxListItem:setAlign(LI, ?wxLIST_FORMAT_LEFT),
    wxListCtrl:insertColumn(ListCtrl, 5, LI),
    wxListItem:setText(LI, "Current function"), 
    wxListItem:setAlign(LI, ?wxLIST_FORMAT_LEFT),
    wxListCtrl:insertColumn(ListCtrl, 6, LI),
    wxListItem:destroy(LI),

    wxListCtrl:setColumnWidth(ListCtrl, 0, 80),
    wxListCtrl:setColumnWidth(ListCtrl, 1, 200),
    wxListCtrl:setColumnWidth(ListCtrl, 2, 50),
    wxListCtrl:setColumnWidth(ListCtrl, 3, 50),
    wxListCtrl:setColumnWidth(ListCtrl, 4, 70),
    wxListCtrl:setColumnWidth(ListCtrl, 5, 60),
    wxListCtrl:setColumnWidth(ListCtrl, 6, 200),

    %%wxListCtrl:connect(ListCtrl, size, [{skip, true}]),
    wxListCtrl:connect(ListCtrl, command_list_item_activated),
    wxListCtrl:connect(ListCtrl, command_list_item_right_click),
    wxListCtrl:connect(ListCtrl, command_list_col_click),
    wxListCtrl:connect(ListCtrl, command_list_item_selected),

    ListCtrl.
%%    wxListCtrl:connect(ListCtrl, grid_cell_left_click),


%% create_check_box(Frame, Sizer, Id, Text) ->
%%     ChkBox = wxCheckBox:new(Frame, Id, Text, []),
%%     wxSizer:add(Sizer, ChkBox, [{flag, ?wxLEFT}, {border, 10}]),
%%     ChkBox.


%% create_box(Frame, BoxTitle, Values, NoCols) ->
%%     Box   = wxStaticBoxSizer:new(?wxHORIZONTAL, Frame, [{label, BoxTitle}]),
%%     Sizer = wxFlexGridSizer:new(3 * NoCols, [{vgap, 3}, {hgap, 3}]),
%%     AllV  = lists:map(fun({Title, Value}) ->
%% 			      TitleLabel = wxStaticText:new(Frame, ?wxID_ANY, Title ++ " :"),
%% 			      wxSizer:add(Sizer, TitleLabel, [{flag, ?wxALIGN_RIGHT}]),
%% 			      ValueLabel = wxStaticText:new(Frame, ?wxID_ANY, Value),
%% 			      wxSizer:add(Sizer, ValueLabel, [{flag, ?wxALIGN_RIGHT}]),
%% 			      wxSizer:addSpacer(Sizer, 10),
%% 			      ValueLabel
%% 		      end, Values),
%%     wxSizer:add(Box, Sizer),
%%     wxStaticBox:fit(wxStaticBoxSizer:getStaticBox(Box)),
%%     {AllV, Box}.

create_menu(MenuItems, Name, MenuBar) ->
    Menu = wxMenu:new(),
    lists:foreach(fun(Record) ->
			  create_menu_item(Record, Menu)
		  end,
		  MenuItems),
    wxMenuBar:append(MenuBar, Menu, Name),
    Menu.

create_menu_item(#create_menu{id = Id, text = Text, type = Type, check = Check}, Menu) ->
    case Type of
	append ->
	    wxMenu:append(Menu, Id, Text);
	appendCheck ->
	    wxMenu:appendCheckItem(Menu, Id, Text),
	    wxMenu:check(Menu, Id, Check);
	separator ->
	    wxMenu:appendSeparator(Menu)
    end.


refresh(Info,#pro_wx_state{grid = Grid, sort_order = Sort}) ->
    wx:batch(fun() -> update_grid(Grid, Sort, Info#etop_info.procinfo) end).


update_grid(Grid, {Sort,Dir}, ProcInfo0) ->
    wxListCtrl:deleteAllItems(Grid),

    Update =
	fun(#etop_proc_info{pid = Pid,mem = Mem, reds = Reds,name = Name,
			    runtime = RunTime,cf = CF,mq = MQ}, Row) ->
		wxListCtrl:insertItem(Grid, Row, ""),
		if (Row rem 2) =:= 0 ->
			wxListCtrl:setItemBackgroundColour(Grid, Row, {240,240,255});
		   true -> ignore
		end,
		lists:foreach(fun({Col, Val}) -> 
				      wxListCtrl:setItem(Grid, Row, Col, to_str(Val))
			      end,
			      [{?COL_PID,Pid},  {?COL_NAME,Name},{?COL_TIME,RunTime},
			       {?COL_REDS,Reds},{?COL_MEM,Mem},
			       {?COL_MSG, MQ},  {?COL_FUN,CF}
			      ]),
		Row + 1
	end,
    ProcInfo = case Dir of 
		   decr -> lists:reverse(lists:keysort(Sort, ProcInfo0));
		   incr -> lists:keysort(Sort, ProcInfo0)
	       end,
    wx:foldl(Update, 0, ProcInfo),
    ok.

save_options(#trace_options{send = Send,
			    treceive = Receive,
			    functions = Functions,
			    events = Events,
			    on_1st_spawn = On1stSpawn,
			    on_all_spawn = OnAllSpawn,
			    on_1st_link = On1stLink,
			    on_all_link = OnAllLink,
			    in_window = InWindow},
	     ProcessInfoList) ->
    io:format("ProcessInfoList: ~p~n", [ProcessInfoList]),
    ProcessInfo = 
	lists:foldl(fun(Atom , Acc) ->
			    ["{" ++ atom_to_list(Atom) ++ ", procinfo}.\n" | Acc]
		    end, [], ProcessInfoList),
    io:format("ProcessInfo: ~p~n", [ProcessInfo]),
    Dir = filename:join(os:getenv("HOME"), ".erlang_tools"),
    file:make_dir(Dir), 
    File = filename:join(Dir, "etop.opts"),
    
    Binary =
	list_to_binary("%%%\n%%% This file is generated by Etop\n"
		       "%%%\n%%% DO NOT EDIT!\n%%%\n"
		       "{send, " ++ atom_to_list(Send) ++ "}.\n"
		       "{treceive, " ++ atom_to_list(Receive) ++ "}.\n"
		       "{functions, " ++ atom_to_list(Functions) ++ "}.\n"
		       "{events, " ++ atom_to_list(Events) ++ "}.\n"
		       "{on_1st_spawn, " ++ atom_to_list(On1stSpawn) ++ "}.\n"
		       "{on_all_spawn, " ++ atom_to_list(OnAllSpawn) ++ "}.\n"
		       "{on_1st_link, " ++ atom_to_list(On1stLink) ++ "}.\n"
		       "{on_all_link, " ++ atom_to_list(OnAllLink) ++ "}.\n"
		       "{in_window, " ++ atom_to_list(InWindow) ++ "}.\n" ++ ProcessInfo),

    case file:write_file(File, Binary) of
	ok ->
	    io:format("File saved: ~p\n", [File]);
	{error, Reason} ->
	    io:format("{error, ~p}\n", [Reason])
    end,
    File.


%% get_node_name(Config) ->
%%     atom_to_list(erltop:getopt(node, Config)).

lookup_pid(#etop_info{procinfo = ProcInfo}, Pid) ->
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

update_sort_order(Col,State = #pro_wx_state{sort_order = {Sort,Dir}}) ->
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


parse_options([H | T], Rec, Acc) ->
    {Rec2, Acc2} =
	case H of
	    {send, Bool}         -> {Rec#trace_options{send = Bool}, Acc};
	    {treceive, Bool}     -> {Rec#trace_options{treceive = Bool}, Acc};
	    {functions, Bool}    -> {Rec#trace_options{functions = Bool}, Acc};
	    {events, Bool}       -> {Rec#trace_options{events = Bool}, Acc};
	    {on_1st_spawn, Bool} -> {Rec#trace_options{on_1st_spawn = Bool}, Acc};
	    {on_all_spawn, Bool} -> {Rec#trace_options{on_all_spawn = Bool}, Acc};
	    {on_1st_link, Bool}  -> {Rec#trace_options{on_1st_link = Bool}, Acc};
	    {on_all_link, Bool}  -> {Rec#trace_options{on_all_link = Bool}, Acc};
	    {in_window, Bool}    -> {Rec#trace_options{in_window = Bool}, Acc};
	    {ProcInfo, procinfo} -> {Rec, [ProcInfo | Acc]};

	    Other ->
		io:format("Not a valid option.\n~p\n", [Other]),
		{Rec, Acc}
	end,
    parse_options(T, Rec2, Acc2);

parse_options([], Rec, Acc) ->
    {Rec, Acc};
parse_options(_Other, _Rec, _Acc) ->
    io:format("Default.\n", []),
    {#trace_options{}, []} .

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

update_propage(Config, State) ->
    NewInfo = erltop:update(Config),
    refresh(NewInfo, State),
    NewInfo.	

%% get_nodes() ->
%%     Nodes = [node()| nodes()],
%%     {_, Menues} =
%% 	lists:foldl(fun(Node, {Id, Acc}) when Id < ?LAST_NODES_MENU_ID ->
%% 			    {Id + 1, [#create_menu{id = Id + ?FIRST_NODES_MENU_ID, 
%% 						   text =  atom_to_list(Node)} | Acc]} 
%% 		    end,
%% 		    {1, []},
%% 		    Nodes),
%%     {Nodes, lists:reverse(Menues)}.


%%%%%%%%%%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info(time_to_update, {Config, _Info, State}) ->
    NewInfo = update_propage(Config, State),
    erlang:send_after(State#pro_wx_state.interval, self(), time_to_update),
    {noreply, {Config, NewInfo, State}};

handle_info({checked, Opt, ProcessInfo, Interval}, {Config, Info, State}) ->
    State2 = State#pro_wx_state{trace_options = Opt,
				process_info  = ProcessInfo,
				interval = Interval},
    {noreply, {Config, Info, State2}};

handle_info({save, File}, State) ->
    wxFrame:setStatusText(State#pro_wx_state.frame, "Options saved: " ++ File),
    {noreply, State};

handle_info({config, _Changed, NewConfig}, {_Config, Info, State}) ->
    {noreply, {NewConfig, Info, State}};

handle_info({'EXIT', Pid, Reason}, {Config, Info, State}) when Pid =:= State#pro_wx_state.options_pid ->
    opt_warning(Pid, Reason),
    {noreply, {Config, Info, State#pro_wx_state{options_pid = undefined}}};

handle_info({'EXIT', Pid, Reason}, {Config, Info, State}) ->
    opt_warning(Pid, Reason),
    case lists:keysearch(Pid, #pid.window, State#pro_wx_state.opened) of
	{value, Tuple} ->
	    Opened = lists:delete(Tuple, State#pro_wx_state.opened),
	    {noreply, {Config, Info, State#pro_wx_state{opened = Opened}}};
	false ->
	    {noreply, {Config, Info, State}}
    end;

handle_info({node, Node}, {Config, _Info, State}) ->
    Config2 = Config#opts{node = Node},
    NewInfo = update_propage(Config2, State),
    %%wxFrame:setTitle(State#state.frame, "etop: " ++ get_node_name(Config2)),
    {noreply, {Config2, NewInfo, State}};

handle_info(Info, State) ->
    io:format("~p, ~p, Handled unexpected info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p terminating. Reason: ~p~n", [?MODULE, Reason]),
    erltop:stop(),
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

handle_event(#wx{id = ?ID_SAVE_OPT, event = #wxCommand{type = command_menu_selected}}, 
	     {Config, Info, #pro_wx_state{trace_options = Options} = State}) -> 
    io:format("~p:~p, Klickade på save options~n", [?MODULE, ?LINE]),
    %% File = 
    save_options(Options, State#pro_wx_state.process_info),
    %%wxFrame:setStatusText(State#pro_wx_state.frame, "Options saved: " ++ File),
    {noreply, {Config, Info, State}};

handle_event(#wx{id = ?ID_DUMP_TO_FILE}, {Config, Info, State}) ->
    io:format("~p:~p, Klickade på dump to file~n", [?MODULE, ?LINE]),
    FD  =  wxFileDialog:new(State#pro_wx_state.frame, [{style,?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT}]),
    case wxFileDialog:showModal(FD) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(FD),
	    io:format("It should Dump to file ~p~n", [Path]);
	_ -> ok
    end,
    {noreply, {Config, Info, State}};

handle_event(#wx{id = ?ID_OPTIONS}, {Config, Info, #pro_wx_state{trace_options = Options} = State}) ->
    io:format("~p:~p, Klickade på trace options~n", [?MODULE, ?LINE]),
    case State#pro_wx_state.options_pid of
	undefined ->
	    Pid = erltop_options:start(Options,
				       State#pro_wx_state.process_info,
				       State#pro_wx_state.interval,
				       self()),
	    {noreply, {Config, Info, State#pro_wx_state{options_pid = Pid}}};
	_ ->
	    {noreply, {Config, Info, State}}
    end;

handle_event(#wx{id = ?ID_REFRESH, event = #wxCommand{type = command_menu_selected}}, {Config, _Info, State}) ->
    io:format("~p:~p, Klickade på refresh~n", [?MODULE, ?LINE]),
    NewInfo = update_propage(Config, State),
    {noreply, {Config, NewInfo, State}};

handle_event(#wx{id = ?ID_KILL}, {Config, Info, State}) ->
    case State#pro_wx_state.selected of
	undefined -> ignore;
 	_ -> exit(State#pro_wx_state.selected, kill)
     end,
    {noreply, {Config, Info, State}};

handle_event(#wx{id = ?ID_MODULE_INFO, 
		 event = #wxCommand{type = command_menu_selected}}, {Config, Info, State})  ->
    erltop_process:module_info_window(Config#opts.node, State#pro_wx_state.selected), %%%%%%% erltop process byt namn
    {noreply, {Config, Info, State}};

handle_event(#wx{id = ?ID_VIEW} = Event, {Config, Info, State}) ->
    io:format("Event: ~p~n", [Event]),
    #etop_proc_info{cf = {M,_,_}} = lookup_pid(Info, State#pro_wx_state.selected),
    case module_code(M) of
	no_code ->     ok;
	{src, File} ->
	    {CodeFrame, CodeView} = create_code_frame(State#pro_wx_state.frame),
	    wxTextCtrl:loadFile(CodeView, File),
	    FileName = filename:basename(File),
	    wxFrame:setTitle(CodeFrame, "Viewing: " ++ FileName),
	    wxFrame:show(CodeFrame)
    end,
    {noreply, {Config, Info, State}};

handle_event(#wx{id = ?ID_PROC}, {#opts{node = Node} = Config, Info, #pro_wx_state{trace_options = Options} = State}) ->
    case lists:keysearch(State#pro_wx_state.selected, #pid.traced, State#pro_wx_state.opened) of
	false ->
	    Pid = erltop_process:start(State#pro_wx_state.selected,
				       Options,
				       State#pro_wx_state.process_info,
				       Node,
				       self()),
	    {noreply, {Config, Info, State#pro_wx_state{opened = [#pid{traced = State#pro_wx_state.selected,
								window = Pid} | State#pro_wx_state.opened]}}};
	{value, _Tuple} ->
	    io:format("Already open\n", []),
	    {noreply, {Config, Info, State}}
    end;
 
handle_event(#wx{id = ?ID_TRACE}, {#opts{node = Node} = Config, Info, #pro_wx_state{trace_options = Options} = State}) ->
    case lists:keysearch(State#pro_wx_state.selected, #pid.traced, State#pro_wx_state.opened) of
	false ->
	    Pid = erltop_process:start(State#pro_wx_state.selected,
				       Options,
				       State#pro_wx_state.process_info,
				       Node,
				       self()) ,
	    {noreply, {Config, Info, State#pro_wx_state{opened = [#pid{traced = State#pro_wx_state.selected,
								window = Pid} | State#pro_wx_state.opened]}}};
	{value, _Tuple} ->
	    io:format("Already open\n", []),
	    {noreply, {Config ,Info, State}}
    end;

handle_event(#wx{event = #wxList{type = command_list_item_right_click, 
				 itemIndex = Row}}, {Config, Info, State}) -> %Row selected
    #pro_wx_state{grid = Grid, popup_menu = PopupMenu} = State,
    Pid = wxListCtrl:getItemText(Grid, Row),
    wxWindow:popupMenu(Grid, PopupMenu, []),
    {noreply, {Config, Info, State#pro_wx_state{selected = list_to_pid(Pid)}}};

handle_event(#wx{event = #wxList{type = command_list_item_selected, 
				 itemIndex = Row}}, {Config, Info, State}) ->
    #pro_wx_state{grid = Grid} = State,
    Pid = wxListCtrl:getItemText(Grid, Row),
    {noreply, {Config, Info, State#pro_wx_state{selected = list_to_pid(Pid)}}};

handle_event(#wx{event = #wxList{type = command_list_col_click, col = Col}}, {Config, Info, State}) ->
    NewState = update_sort_order(Col,State),
    refresh(Info, NewState),
    {noreply, {Config, Info, NewState}};

handle_event(#wx{event = #wxList{type = command_list_item_activated}}, 
	     {#opts{node = Node} = Config, Info, #pro_wx_state{trace_options = Options} = State}) ->
    case lists:keysearch(State#pro_wx_state.selected, #pid.traced, State#pro_wx_state.opened) of
	false ->
	    Pid = erltop_process:start(State#pro_wx_state.selected,
				       Options,
				       State#pro_wx_state.process_info,
				       Node,
				       self()),
	    
	    {noreply, {Config, Info, State#pro_wx_state{opened = [#pid{traced = State#pro_wx_state.selected,
								window = Pid} | State#pro_wx_state.opened]}}};
	{value, _Tuple} ->
	    io:format("Already open\n", []),
	    {noreply, {Config,Info,State}}
    end;


handle_event(#wx{event = #wxNotebook{type = command_notebook_page_changed}}, {Config, _Info, State}) ->
    create_pro_menu(State#pro_wx_state.menubar),
    NewInfo = update_propage(Config, State),
    {noreply, {Config, NewInfo, State}};

handle_event(Event, State) ->
    io:format("~p~p, handle event ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.





