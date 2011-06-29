-module(observer_trace_wx).

-export([start/5]).
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include("erltop_defs.hrl").

-record(state, {frame,
		text_ctrl,
		trace_options,
		toggle_button,
		node,
		parent,
		traced_procs,
		mod_menus}).

-record(boxes, {send, 'receive', functions, events,
		on_spawn, on_link, all_spawn, all_link}).


-define(OPTIONS, 1).
-define(SAVE_BUFFER, 2).
-define(CLOSE, 3).
-define(CLEAR, 4).
-define(ALL_LINKED, 5).
-define(LINKED_PROCESS, 6).
-define(KILL, 7).
-define(VIEW_MODULE, 8).
-define(FIRST_MODULE, 9).


start(Node, TracedProcs, Options, ParentFrame, ParentPid) ->
    wx_object:start_link(?MODULE, [Options, Node, TracedProcs, ParentFrame, ParentPid], []).

init([Options, Node, TracedProcs, ParentFrame, ParentPid]) ->
    ModMenus = get_modmenus(Node, TracedProcs),
    
    State = 
	wx:batch(fun() ->
			 create_window(ParentFrame, Options, ModMenus)
		 end),
    Frame = State#state.frame,
    TraceOpts = State#state.trace_options,

    UpdTraceOpts = create_traceoption_dialog(Frame, TraceOpts, ParentPid),

    {Frame, State#state{parent = ParentPid,
			node = Node,
			trace_options = UpdTraceOpts, 
			traced_procs = TracedProcs,
			mod_menus = ModMenus}}.


create_window(ParentFrame, Options, ModMenus) ->
    %% Create the window
    Frame = wxFrame:new(ParentFrame, ?wxID_ANY, "Tracer", [{size, {900, 900}}]),
    wxFrame:connect(Frame, close_window,[{skip,true}]),
    Panel = wxPanel:new(Frame, []),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    
    %% Menues
    MenuBar = wxMenuBar:new(),
    create_menues(MenuBar, ModMenus),
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

    %% Display window
    wxWindow:setSizer(Panel, Sizer),
    wxFrame:show(Frame),
    #state{frame = Frame,
	   text_ctrl = TextCtrl, 
	   toggle_button = ToggleButton,
	   trace_options = Options#trace_options{main_window = false}}.
	   

create_menues(MenuBar, ModMenus) ->
    io:format("Modmenus: ~p~n", [ModMenus]),
    observer_wx:create_menu(
      [
       {"File", [#create_menu{id = ?OPTIONS, text = "&Options"},
		 #create_menu{id = ?SAVE_BUFFER, text = "&Save buffer"},
		 #create_menu{id = ?CLOSE, text = "&Close"}
		]},
       {"View", [#create_menu{id = ?CLEAR, text = "&Clear buffer"}
		]},
       {"Modules", ModMenus}
      ],
      MenuBar).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{id = ?CLOSE, event = #wxCommand{type = command_menu_selected}}, State) ->
    io:format("~p Shutdown. ~p\n", [?MODULE, self()]),
    {stop, shutdown, State};

handle_event(#wx{id = ?OPTIONS, event = #wxCommand{type = command_menu_selected}}, 
	     #state{frame = Frame, trace_options = TraceOpts, parent = Parent} = State) ->
    
    UpdTraceOpts = create_traceoption_dialog(Frame, TraceOpts, Parent),
    {noreply, State#state{trace_options = UpdTraceOpts}};

handle_event(#wx{id = ?CLEAR, event = #wxCommand{type = command_menu_selected}}, State) ->
    wxTextCtrl:clear(State#state.text_ctrl),
    {noreply, State};

handle_event(#wx{id = ?SAVE_BUFFER, event = #wxCommand{type = command_menu_selected}}, State) ->
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
    {noreply, State};

handle_event(#wx{id = Id, 
		 event = #wxCommand{type = command_menu_selected}}, 
	     #state{mod_menus = ModuleMenus} = State) 
  when Id >= ?FIRST_MODULE, Id =< ?FIRST_MODULE + length(ModuleMenus) ->
    ModLabel = find_module(Id, ModuleMenus),
    io:format("Du klickade på meny: ~p~n", [ModLabel]),
    {noreply, State};

handle_event(#wx{event = #wxClose{type = close_window}}, State) ->
    io:format("~p Shutdown. ~p\n", [?MODULE, self()]),
    {stop, shutdown, State};

handle_event(#wx{event = #wxCommand{type = command_togglebutton_clicked, commandInt = 1}}, 
	     #state{trace_options = TraceOpts, traced_procs = TracedProcs, 
		    node = Node, text_ctrl = TextCtrl, toggle_button = ToggleBtn} = State) ->
    
    start_trace(Node, TracedProcs, TraceOpts),
    wxTextCtrl:appendText(TextCtrl, "Start Trace:\n"),
    wxToggleButton:setLabel(ToggleBtn, "Stop Trace"),
    {noreply, State};

handle_event(#wx{event = #wxCommand{type = command_togglebutton_clicked, commandInt = 0}}, %%Stop tracing
	     State) ->
    dbg:stop_clear(),
    wxTextCtrl:appendText(State#state.text_ctrl, "Stop Trace.\n"),
    wxToggleButton:setLabel(State#state.toggle_button, "Start Trace"),
    {noreply, State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_event(#wx{event = #wxCommand{type = command_checkbox_clicked}}, State) ->
%%     io:format("~p ~p, Checkbox clicked~n", [?MODULE, ?LINE]),
%%     wx:batch(fun() -> enable(State#state.boxes#boxes.on_spawn, 
%% 			     State#state.boxes#boxes.on_link) end),
%%     {noreply, State};

%% handle_event(#wx{id = ?wxID_SAVE, event = #wxCommand{type = command_button_clicked}}, State) -> %Bör save finnas?
%%     File = ?MODULE:save_options(State#state.trace_options),
%%     State#state.parent ! {save, File},
%%     {noreply, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{event = What}, State) ->
    io:format("~p~p: WX: ~p ~n", [?MODULE, self(), What]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info(Tuple, State) when is_tuple(Tuple) ->
    io:format("Incoming tuple: ~p~n", [Tuple]),
    Text = textformat(Tuple),
    wxTextCtrl:appendText(State#state.text_ctrl, lists:flatten(Text)),
    {noreply, State};

handle_info(Any, State) ->
    io:format("~p~p: received unexpected message: ~p\n", [?MODULE, self(), Any]),
    {noreply, State}.


terminate(Reason, #state{parent = Parent, frame = Frame}) ->
    io:format("~p terminating. Reason: ~p~n", [?MODULE, Reason]),
    Parent ! tracemenu_closed,
    wxFrame:destroy(Frame),
    ok.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

handle_call(Msg, _From, State) ->
    io:format("~p~p: Got Call ~p~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("~p ~p: Unhandled cast ~p~n", [?MODULE, ?LINE, Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_trace(Node, TracedProcs, 
	    #trace_options{send = Send, treceive = Receive, functions = Functions,
			   events = Events, on_1st_spawn = On1Spawn,
			   on_all_spawn = AllSpawn, on_1st_link = On1Link,
			   on_all_link = AllLink}) ->
    
    MyPid = self(),
    HandlerFun = fun(NewMsg, _) ->
			 MyPid ! NewMsg
		 end,
    dbg:tracer(process, {HandlerFun, []}),
    dbg:n(Node),
    
    Recs = [{Send, send},
	    {Receive, 'receive'},
	    {Functions, call}, 
	    {Events, procs},
	    {On1Spawn, set_on_first_spawn},
	    {AllSpawn, set_on_spawn},
	    {On1Link, set_on_first_link},
	    {AllLink, set_on_link}],
    
    Flags = [Assoc || {true, Assoc} <- Recs],
    
    Trace = fun(Pid) ->
		    dbg:p(Pid, Flags)
	    end,
    lists:foreach(Trace, TracedProcs).


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


create_traceoption_dialog(Frame, Opt, Parent) ->
    Dialog = wxDialog:new(Frame, ?wxID_ANY, "Trace options"),
    Panel = wxPanel:new(Dialog, []),

    MainSz = wxBoxSizer:new(?wxVERTICAL),
    TopSz = wxBoxSizer:new(?wxHORIZONTAL),
    TopLeftSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label,"Tracing options:"}]),
    TopRightSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Inheritance options:"}]),
        
    SendBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace send", []),
    check_box(SendBox, Opt#trace_options.send),
    RecBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace receive", []),
    check_box(RecBox, Opt#trace_options.treceive),
    FuncBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace functions", []),
    check_box(FuncBox, Opt#trace_options.functions),
    EventBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace events", []),
    check_box(EventBox, Opt#trace_options.events),
    
    {SpawnBox, SpwnAllRadio, SpwnFirstRadio} = top_right(Panel, TopRightSz, [{flag, ?wxBOTTOM},{border, 5}], "spawn"),
    {LinkBox, LinkAllRadio, LinkFirstRadio} = top_right(Panel, TopRightSz, [{flag, ?wxBOTTOM},{border, 5}], "link"),
    wxRadioButton:setValue(SpwnAllRadio, Opt#trace_options.on_all_spawn),
    wxRadioButton:setValue(SpwnFirstRadio, Opt#trace_options.on_1st_spawn),
    wxRadioButton:setValue(LinkAllRadio, Opt#trace_options.on_all_link),
    wxRadioButton:setValue(LinkFirstRadio, Opt#trace_options.on_1st_link),


    OKBtn = wxButton:new(Panel, ?wxID_OK, []),
    CancelBtn = wxButton:new(Panel, ?wxID_CANCEL, []),
    BottomSz = wxBoxSizer:new(?wxHORIZONTAL),
    
    wxSizer:add(BottomSz, CancelBtn),
    wxSizer:add(BottomSz, OKBtn),
    wxSizer:add(TopLeftSz, SendBox, []),
    wxSizer:add(TopLeftSz, RecBox, []),
    wxSizer:add(TopLeftSz, FuncBox, []),
    wxSizer:add(TopLeftSz, EventBox, []),
    wxSizer:add(TopLeftSz, 150, -1),
   
    wxSizer:add(TopSz, TopLeftSz, [{flag, ?wxALL},{border, 5}]),
    wxSizer:add(TopSz, TopRightSz, [{flag, ?wxALL},{border, 5}]),
    wxSizer:add(MainSz, TopSz, []),   
    wxSizer:add(MainSz, BottomSz),
 

    wxWindow:setSizer(Panel, MainSz),
    wxSizer:fit(MainSz, Dialog),
    wxSizer:setSizeHints(MainSz, Dialog),
    
    Boxes = #boxes{send = SendBox,
		   'receive' = RecBox,
		   functions = FuncBox,
		   events = EventBox,
		   on_spawn = #on_spawn{checkbox = SpawnBox,
					all_spawn = SpwnAllRadio,
					first_spawn = SpwnFirstRadio},
		   all_spawn = SpwnAllRadio,
		   on_link = #on_link{checkbox = LinkBox,
				      all_link = LinkAllRadio,
				      first_link = LinkFirstRadio},
		   all_link = LinkAllRadio},
    
    
    case wxDialog:showModal(Dialog) of
	?wxID_OK ->
	    UpdTraceOpts2 = wx:batch(fun() ->
					     read_trace_boxes(Boxes, Opt)
				     end),
	    
	    Parent ! {updated_traceopt, UpdTraceOpts2},
	    UpdTraceOpts2;
	?wxID_CANCEL ->
	    Opt
    end.

    

top_right(Panel, TopRightSz, Options, Text) ->
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    ChkBox = wxCheckBox:new(Panel, ?wxID_ANY, "Inherit on " ++ Text, []),
    wxCheckBox:set3StateValue(ChkBox, ?wxCHK_CHECKED),
    RadioSz = wxBoxSizer:new(?wxVERTICAL),
    Radio1 = wxRadioButton:new(Panel, ?wxID_ANY, "All " ++ Text, [{style, ?wxRB_GROUP}]),
    Radio2 = wxRadioButton:new(Panel, ?wxID_ANY, "First " ++ Text ++ " only", []),
    wxSizer:add(Sizer, ChkBox, []),
    wxSizer:add(RadioSz, Radio1, []),
    wxSizer:add(RadioSz, Radio2, []),
    wxSizer:add(Sizer, RadioSz, [{flag, ?wxLEFT},{border, 20}]),
    wxSizer:add(TopRightSz, Sizer, Options),
    {ChkBox, Radio1, Radio2}.



read_trace_boxes(ChkBoxes = #boxes{on_spawn = OnSpawn, on_link = OnLink}, Options) ->
    {On1stSpawn2, OnAllSpawn2} =
	case wxCheckBox:isChecked(OnSpawn#on_spawn.checkbox) of
	    true ->
		OnAllSpawn = wxRadioButton:getValue(OnSpawn#on_spawn.all_spawn),
		On1stSpawn = wxRadioButton:getValue(OnSpawn#on_spawn.first_spawn),
		{On1stSpawn, OnAllSpawn};
	    false ->
		{false, false}
	end,
    {On1stLink2, OnAllLink2} =
	case wxCheckBox:isChecked(OnLink#on_link.checkbox) of
	    true ->
		OnAllLink = wxRadioButton:getValue(OnLink#on_link.all_link),
		On1stLink = wxRadioButton:getValue(OnLink#on_link.first_link),
		{On1stLink, OnAllLink};
	    false ->
		{false, false}
	end,
    Options#trace_options{send = wxCheckBox:isChecked(ChkBoxes#boxes.send),
			  treceive = wxCheckBox:isChecked(ChkBoxes#boxes.'receive'),
			  functions = wxCheckBox:isChecked(ChkBoxes#boxes.functions),
			  events = wxCheckBox:isChecked(ChkBoxes#boxes.events),
			  on_all_spawn = OnAllSpawn2,
			  on_1st_spawn = On1stSpawn2,
			  on_all_link = OnAllLink2,
			  on_1st_link = On1stLink2}.


check_box(ChkBox, Bool) ->
    case Bool of
	true ->
	    wxCheckBox:set3StateValue(ChkBox, ?wxCHK_CHECKED);
	false ->
	    ignore
    end.


get_modmenus(Node, PidList) ->
    ModuleList = get_modules(Node, PidList, []),
    create_modmenus(ModuleList).
    
get_modules(_, [], Modules) ->
    Modules;
get_modules(Node, [Pid|Pids], Modules) ->
    {initial_call, {Module, _, _}} = rpc:call(Node, erlang, process_info, [list_to_pid(Pid), initial_call]),
    case lists:member(Module, Modules) of
	true ->
	    get_modules(Node, Pids, Modules);
	false ->
	    get_modules(Node, Pids, [Module|Modules])
    end.

create_modmenus(ModuleList) ->
    create_modmenus(ModuleList, ?FIRST_MODULE, []).

create_modmenus([], _, Acc) ->
    Acc;
create_modmenus([Module|T], Id, Acc) ->
    create_modmenus(T, Id+1, [#create_menu{id = Id, text = atom_to_list(Module)} | Acc]).


find_module(Id, [#create_menu{id = SearchedId, text = Name} |T]) ->
    case Id =:= SearchedId of
	true ->
	    Name;
	false ->
	    find_module(Id, T)
    end.
	    
    
