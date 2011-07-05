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
		traceoptions_open,
		module_infobox_open,
		traced_procs,
		traced_funcs = dict:new(),
		checked_funcs = [],
		tree,
		modules}).

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
-define(SELECT, 10).
-define(SELECT_ALL, 11).


start(Node, TracedProcs, Options, ParentFrame, ParentPid) ->
    wx_object:start_link(?MODULE, [Options, Node, TracedProcs, ParentFrame, ParentPid], []).

init([Options, Node, TracedProcs, ParentFrame, ParentPid]) ->
    Modules = get_modules(Node),
    
    State = 
	wx:batch(fun() ->
			 create_window(ParentFrame, Options)
		 end),
    Frame = State#state.frame,
    TraceOpts = State#state.trace_options,
    TracedFuncs = State#state.traced_funcs,
    
    Tree = create_traceoption_dialog(Frame, Node, TraceOpts, Modules, TracedFuncs),
    
    {Frame, State#state{parent = ParentPid,
			node = Node,
			traced_procs = TracedProcs,
			traceoptions_open = true,
			module_infobox_open = false,
			tree = Tree,
			modules = Modules}}.


create_window(ParentFrame, Options) ->
    %% Create the window
    Frame = wxFrame:new(ParentFrame, ?wxID_ANY, "Tracer", [{size, {900, 900}}]),
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

    %% Display window
    wxWindow:setSizer(Panel, Sizer),
    wxFrame:show(Frame),
    #state{frame = Frame,
	   text_ctrl = TextCtrl, 
	   toggle_button = ToggleButton,
	   trace_options = Options#trace_options{main_window = false}}.
	   

create_menues(MenuBar) ->
    observer_wx:create_menu(
      [
       {"File", [#create_menu{id = ?OPTIONS, text = "&Trace Options"},
		 #create_menu{id = ?SAVE_BUFFER, text = "&Save buffer"},
		 #create_menu{id = ?CLOSE, text = "&Close"}
		]},
       {"View", [#create_menu{id = ?CLEAR, text = "&Clear buffer"}
		]}
      ],
      MenuBar).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{id = ?CLOSE, event = #wxCommand{type = command_menu_selected}}, State) ->
    io:format("~p Shutdown. ~p\n", [?MODULE, self()]),
    {stop, shutdown, State};

handle_event(#wx{id = ?OPTIONS, event = #wxCommand{type = command_menu_selected}}, 
	     #state{frame = Frame, trace_options = TraceOpts,
		    traced_funcs = TracedFuncs,
		    modules = Modules, node = Node,
		    traceoptions_open = false} = State) ->
    
    create_traceoption_dialog(Frame, Node, TraceOpts, Modules, TracedFuncs),
    {noreply, State#state{traceoptions_open = true}};

handle_event(#wx{id = ?CLEAR, event = #wxCommand{type = command_menu_selected}},
	     #state{text_ctrl = TxtCtrl} = State) ->
    wxTextCtrl:clear(TxtCtrl),
    {noreply, State};

handle_event(#wx{id = ?SAVE_BUFFER, event = #wxCommand{type = command_menu_selected}}, 
	     #state{frame = Frame, text_ctrl = TxtCtrl} = State) ->
    Dialog = wxFileDialog:new(Frame),
    wxFileDialog:show(Dialog),	
    case wxFileDialog:showModal(Dialog) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(Dialog),
	    case filelib:is_file(Path) of
		false ->
		    wxTextCtrl:saveFile(TxtCtrl, [{file, Path}]),
		    wxTextCtrl:appendText(TxtCtrl, "Saved to: " ++  Path ++ "\n");
		true ->
		    wxTextCtrl:appendText(TxtCtrl, "File already exists " ++  Path ++ "\n")
	    end;
	_ -> ok
    end,
    {noreply, State};


handle_event(#wx{obj = ListBox,
		 event = #wxCommand{type = command_listbox_doubleclicked}},
	     #state{frame = Frame, 
		    traced_funcs = TracedDict,
		    module_infobox_open = false} = State) ->
    
    ChosenModule = wxControlWithItems:getStringSelection(ListBox),
    create_module_infobox(Frame, ChosenModule, TracedDict),
    {noreply, State#state{module_infobox_open = true}};

handle_event(#wx{obj = TxtCtrl,
		 event = #wxCommand{type = command_text_updated},
		 userData = {ListBox, Data, Type}},
	     #state{checked_funcs = CheckedFuncs} = State) -> 
    
    Input = wxTextCtrl:getValue(TxtCtrl),
    FilteredData = lists:filter(fun(Row) ->
					lists:prefix(Input, Row)
				end,
				Data),
    wxListBox:clear(ListBox),
    wxListBox:appendStrings(ListBox, FilteredData),
    
    case Type of
	checklistbox ->
	    lists:foreach(fun(Index) -> wxCheckListBox:check(ListBox, Index, [{check, true}]) end,
			  [wxControlWithItems:findString(ListBox, X) || X <- CheckedFuncs, lists:member(X, FilteredData)]);
	listbox ->
	    ignore
    end,
    {noreply, State};

handle_event(#wx{id = ?wxID_OK, %Traceoptions OK
		 event = #wxCommand{type = command_button_clicked},
		 userData = {trace_options, Dialog, Boxes}}, 
	     #state{trace_options = TraceOpts, 
		    parent = Parent} = State) ->
    UpdTraceOpts = wx:batch(fun() ->
				    read_trace_boxes(Boxes, TraceOpts)
			    end),
    Parent ! {updated_traceopt, UpdTraceOpts},
    wxDialog:show(Dialog, [{show, false}]),
    {noreply, State#state{trace_options = UpdTraceOpts, traceoptions_open = false}};

handle_event(#wx{id = ?wxID_OK, %Module_infobox OK
		 event = #wxCommand{type = command_button_clicked},
		 userData = {module_infobox, Dialog, Module, CheckListBox, Choices}},
	     #state{traced_funcs = TracedDict, 
		    tree = Tree} = State) ->
    
    CheckedIndex1 = lists:filter(fun(Index) ->
					wxCheckListBox:isChecked(CheckListBox, Index)
				end,
				lists:seq(0, wxControlWithItems:getCount(CheckListBox))),
    
    CheckedIndex2 = lists:map(fun(Int) ->
				      Int+1
			      end,
			      CheckedIndex1),
    Selections = get_selections(CheckedIndex2, Choices),
    UpdTracedDict = case Selections of
			[] ->
			    dict:erase(Module, TracedDict);
			_ ->
			    dict:store(Module, Selections, TracedDict)
		    end,
    update_tree(Tree, UpdTracedDict),
    wxDialog:show(Dialog, [{show, false}]),
    {noreply, State#state{traced_funcs = UpdTracedDict,
			  checked_funcs = [],
			  module_infobox_open = false}};

handle_event(#wx{id = ?wxID_CANCEL, %Traceoptions cancel
		 event = #wxCommand{type = command_button_clicked},
		 userData = {trace_options, Dialog}},
	     State) ->
    wxDialog:show(Dialog, [{show, false}]),
    {noreply, State#state{traceoptions_open = false}};

handle_event(#wx{id = ?wxID_CANCEL, %Module_infobox cancel
		 event = #wxCommand{type = command_button_clicked},
		 userData = {module_infobox, Dialog}},
	     State) ->
    wxDialog:show(Dialog, [{show, false}]),
    {noreply, State#state{module_infobox_open = false, checked_funcs = []}};

handle_event(#wx{id = ?SELECT, %Module_infobox select/deselect
		 event = #wxCommand{type = command_button_clicked},
		 userData = {Bool, CheckListBox}},
	     #state{checked_funcs = CheckedFuncs} = State) ->
    {_, Selections} = wxListBox:getSelections(CheckListBox),
    lists:foreach(fun(Index) -> wxCheckListBox:check(CheckListBox, Index, [{check, Bool}]) end, Selections),
    StrSelections = [wxControlWithItems:getString(CheckListBox, N) || N <- Selections],
    UpdCheckedFuncs = case Bool of
			  true ->
			      [X || X <- StrSelections,
				    not(lists:member(X, CheckedFuncs))] ++ CheckedFuncs;
			  false ->
			      CheckedFuncs -- StrSelections
		      end,
    {noreply, State#state{checked_funcs = UpdCheckedFuncs}};

handle_event(#wx{id = ?SELECT_ALL, %Module_infobox select-/deselect-all
		 event = #wxCommand{type = command_button_clicked},
		 userData = {Bool, CheckListBox}},
	     State) ->
    lists:foreach(fun(Index) -> wxCheckListBox:check(CheckListBox, Index, [{check, Bool}]) end,
		  lists:seq(0, wxControlWithItems:getCount(CheckListBox))),
    CheckedFuncs = case Bool of
		       true ->
			   [wxControlWithItems:getString(CheckListBox, N) 
			    || N <- lists:seq(0, wxControlWithItems:getCount(CheckListBox))];
		       false ->
			   []
		   end,
    {noreply, State#state{checked_funcs = CheckedFuncs}};

handle_event(#wx{obj = CheckListBox,
		 event = #wxCommand{type = command_checklistbox_toggled,
				    commandInt = Index}},
	     #state{checked_funcs = CheckedFuncs} = State) ->
    
    UpdCheckedFuncs = case wxCheckListBox:isChecked(CheckListBox, Index) of
			  true ->
			      [wxControlWithItems:getString(CheckListBox, Index) | CheckedFuncs];
			  false ->
			      lists:delete(wxControlWithItems:getString(CheckListBox, Index), CheckedFuncs) 
		      end,
    {noreply, State#state{checked_funcs = UpdCheckedFuncs}};

handle_event(#wx{event = #wxCommand{type = command_checkbox_clicked}, userData = UserData},
	     State) ->
    enable(UserData),
    {noreply, State};

handle_event(#wx{event = #wxClose{type = close_window}}, State) ->
    io:format("~p Shutdown. ~p\n", [?MODULE, self()]),
    {stop, shutdown, State};

handle_event(#wx{event = #wxCommand{type = command_togglebutton_clicked, commandInt = 1}}, 
	     #state{trace_options = TraceOpts, 
		    traced_procs = TracedProcs,
		    node = Node, text_ctrl = TextCtrl, 
		    traced_funcs = TracedDict, 
		    toggle_button = ToggleBtn} = State) ->
    
    start_trace(Node, TracedProcs, TracedDict, TraceOpts),
    wxTextCtrl:appendText(TextCtrl, "Start Trace:\n"),
    wxToggleButton:setLabel(ToggleBtn, "Stop Trace"),
    {noreply, State};

handle_event(#wx{event = #wxCommand{type = command_togglebutton_clicked, commandInt = 0}}, %%Stop tracing
	     #state{text_ctrl = TxtCtrl, toggle_button = ToggleBtn} = State) ->
    dbg:stop_clear(),
    wxTextCtrl:appendText(TxtCtrl, "Stop Trace.\n"),
    wxToggleButton:setLabel(ToggleBtn, "Start Trace"),
    {noreply, State};


%% handle_event(#wx{id = ?wxID_SAVE, event = #wxCommand{type = command_button_clicked}}, State) -> %Bör save finnas?
%%     File = ?MODULE:save_options(State#state.trace_options),
%%     State#state.parent ! {save, File},
%%     {noreply, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{event = What}, State) ->
    io:format("~p~p: Unhandled event: ~p ~n", [?MODULE, self(), What]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 

handle_info(Tuple, #state{text_ctrl = TxtCtrl} = State) when is_tuple(Tuple) ->
    io:format("Incoming tuple: ~p~n", [Tuple]),
    Text = textformat(Tuple),
    wxTextCtrl:appendText(TxtCtrl, lists:flatten(Text)),
    {noreply, State};

handle_info(Any, State) ->
    io:format("~p~p: received unexpected message: ~p\n", [?MODULE, self(), Any]),
    {noreply, State}.


terminate(Reason, #state{parent = Parent, frame = Frame}) ->
    io:format("~p terminating tracemenu. Reason: ~p~n", [?MODULE, Reason]),
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

start_trace(Node, TracedProcs, TracedDict, 
	    #trace_options{send = Send, treceive = Receive, functions = Functions,
			   events = Events, on_1st_spawn = On1Spawn,
			   on_all_spawn = AllSpawn, on_1st_link = On1Link,
			   on_all_link = AllLink}) ->
    
    MyPid = self(),
    HandlerFun = fun(NewMsg, _) ->
			 MyPid ! NewMsg
		 end,
    dbg:tracer(process, {HandlerFun, []}),
    
    case Node =:= node() of 
	true -> 
	    ok;
	false ->
	    dbg:n(Node)
    end,
    
    Recs = [{Send, send},
	    {Receive, 'receive'},
	    {Functions, call}, 
	    {Events, procs},
	    {On1Spawn, set_on_first_spawn},
	    {AllSpawn, set_on_spawn},
	    {On1Link, set_on_first_link},
	    {AllLink, set_on_link}],
    Flags = [Assoc || {true, Assoc} <- Recs],
    
    case TracedProcs of
	all ->
	    dbg:p(all, Flags);
	_ ->
	    lists:foreach(fun(Pid) -> dbg:p(Pid, Flags) end, TracedProcs)
    end,
    
    case Functions of
	true ->
	    trace_functions(TracedDict);
	false ->
	    ok
    end.

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


create_traceoption_dialog(Frame, Node, Opt, Modules, TracedDict) ->
    %%Setup main window
    Dialog = wxDialog:new(Frame, ?wxID_ANY, "Trace options", [{size, {400, 500}}]),
    Panel = wxPanel:new(Dialog, []),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    Notebook = wxNotebook:new(Panel, ?wxID_ANY),
    
    %%Setup option page
    OptPanel = wxPanel:new(Notebook),
    OptMainSz = wxBoxSizer:new(?wxVERTICAL),
    TopSz = wxBoxSizer:new(?wxHORIZONTAL),
    TopLeftSz = wxStaticBoxSizer:new(?wxVERTICAL, OptPanel, [{label, "Tracing options"}]),
    TopRightSz = wxStaticBoxSizer:new(?wxVERTICAL, OptPanel, [{label, "Inheritance options:"}]),
        
    SendBox = wxCheckBox:new(OptPanel, ?wxID_ANY, "Trace send", []),
    check_box(SendBox, Opt#trace_options.send),
    RecBox = wxCheckBox:new(OptPanel, ?wxID_ANY, "Trace receive", []),
    check_box(RecBox, Opt#trace_options.treceive),
    FuncBox = wxCheckBox:new(OptPanel, ?wxID_ANY, "Trace functions", []),
    check_box(FuncBox, Opt#trace_options.functions),
    EventBox = wxCheckBox:new(OptPanel, ?wxID_ANY, "Trace events", []),
    check_box(EventBox, Opt#trace_options.events),
    
    {SpawnBox, SpwnAllRadio, SpwnFirstRadio} = top_right(OptPanel, TopRightSz, [{flag, ?wxBOTTOM},{border, 5}], "spawn"),
    {LinkBox, LinkAllRadio, LinkFirstRadio} = top_right(OptPanel, TopRightSz, [{flag, ?wxBOTTOM},{border, 5}], "link"),
    SpawnBool = Opt#trace_options.on_all_spawn or  Opt#trace_options.on_1st_spawn,
    LinkBool = Opt#trace_options.on_all_link or Opt#trace_options.on_1st_link,
    check_box(SpawnBox, SpawnBool),
    check_box(LinkBox,  LinkBool),
    enable({SpawnBox, SpwnAllRadio, SpwnFirstRadio}),
    enable({LinkBox, LinkAllRadio, LinkFirstRadio}),
    wxRadioButton:setValue(SpwnAllRadio, Opt#trace_options.on_all_spawn),
    wxRadioButton:setValue(SpwnFirstRadio, Opt#trace_options.on_1st_spawn),
    wxRadioButton:setValue(LinkAllRadio, Opt#trace_options.on_all_link),
    wxRadioButton:setValue(LinkFirstRadio, Opt#trace_options.on_1st_link),
    
    wxSizer:add(TopLeftSz, SendBox, []),
    wxSizer:add(TopLeftSz, RecBox, []),
    wxSizer:add(TopLeftSz, FuncBox, []),
    wxSizer:add(TopLeftSz, EventBox, []),
    wxSizer:add(TopLeftSz, 150, -1),
   
    wxSizer:add(TopSz, TopLeftSz, [{flag, ?wxEXPAND}]),
    wxSizer:add(TopSz, TopRightSz,[{flag, ?wxEXPAND}]),
    wxSizer:add(OptMainSz, TopSz, []),      
    
    wxWindow:setSizer(OptPanel, OptMainSz),
    wxNotebook:addPage(Notebook, OptPanel, "Trace options"),
    
    %%Setup trace function page
    FuncPanel = wxPanel:new(Notebook),
    FuncMainSz = wxBoxSizer:new(?wxVERTICAL),
    ModuleSz = wxStaticBoxSizer:new(?wxVERTICAL, FuncPanel, [{label, "Select module"}]),
    TreeSz = wxStaticBoxSizer:new(?wxVERTICAL, FuncPanel, [{label, "Selected functions"}]),
     
    AllModules = atomlist_to_stringlist(Modules),
    ModuleTxtCtrl = wxTextCtrl:new(FuncPanel, ?wxID_ANY),
    ModuleListBox = wxListBox:new(FuncPanel, ?wxID_ANY, [{choices, AllModules}, {style, ?wxLB_SINGLE}]),
    TreeCtrl = wxTreeCtrl:new(FuncPanel, [{size, {500, 200}}]),
    wxTreeCtrl:addRoot(TreeCtrl, atom_to_list(Node)),
    update_tree(TreeCtrl, TracedDict), 
    
    wxTextCtrl:connect(ModuleTxtCtrl, command_text_updated, [{userData,  {ModuleListBox, AllModules, listbox}}]),
    wxListBox:connect(ModuleListBox, command_listbox_doubleclicked, [{userData, TreeCtrl}]),
    wxTreeCtrl:connect(TreeCtrl, command_tree_sel_changed),
    
    wxSizer:add(ModuleSz, ModuleTxtCtrl, [{flag, ?wxEXPAND}]),
    wxSizer:add(ModuleSz, ModuleListBox, [{flag, ?wxEXPAND}]),
    wxSizer:add(TreeSz, TreeCtrl, [{flag, ?wxEXPAND},{proportion, 1}]),
    wxSizer:add(FuncMainSz, ModuleSz, [{flag, ?wxEXPAND}]),
    wxSizer:add(FuncMainSz, TreeSz, [{flag, ?wxEXPAND}]),

    wxWindow:setSizer(FuncPanel, FuncMainSz),
    wxNotebook:addPage(Notebook, FuncPanel, "Function options"),
    wxSizer:add(MainSz, Notebook, [{proportion, 1}, {flag, ?wxEXPAND}]),
    OKBtn = wxButton:new(Panel, ?wxID_OK, []),
    CancelBtn = wxButton:new(Panel, ?wxID_CANCEL, []),
    BottomSz = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(BottomSz, CancelBtn),
    wxSizer:add(BottomSz, OKBtn),
    wxSizer:add(MainSz, BottomSz),
    wxWindow:setSizer(Panel, MainSz),
    
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
    
    wxButton:connect(OKBtn, command_button_clicked, [{userData, {trace_options, Dialog, Boxes}}]),
    wxButton:connect(CancelBtn, command_button_clicked, [{userData, {trace_options, Dialog}}]),
    wxDialog:show(Dialog),
    TreeCtrl.
    

top_right(Panel, TopRightSz, Options, Text) ->
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    ChkBox = wxCheckBox:new(Panel, ?wxID_ANY, "Inherit on " ++ Text, []),
    %% wxCheckBox:set3StateValue(ChkBox, ?wxCHK_CHECKED),
    RadioSz = wxBoxSizer:new(?wxVERTICAL),
    Radio1 = wxRadioButton:new(Panel, ?wxID_ANY, "All " ++ Text, [{style, ?wxRB_GROUP}]),
    Radio2 = wxRadioButton:new(Panel, ?wxID_ANY, "First " ++ Text ++ " only", []),
    wxSizer:add(Sizer, ChkBox, []),
    wxSizer:add(RadioSz, Radio1, []),
    wxSizer:add(RadioSz, Radio2, []),
    wxSizer:add(Sizer, RadioSz, [{flag, ?wxLEFT},{border, 20}]),
    wxSizer:add(TopRightSz, Sizer, Options),
    wxCheckBox:connect(ChkBox, command_checkbox_clicked, [{userData, {ChkBox, Radio1, Radio2}}]),
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


enable({CheckBox, AllRadio, FirstRadio}) ->
    case wxCheckBox:isChecked(CheckBox) of
	false ->
	    wxWindow:disable(AllRadio),
	    wxWindow:disable(FirstRadio);
	true ->
	    wxWindow:enable(AllRadio),
	    wxWindow:enable(FirstRadio)
    end.


check_box(ChkBox, Bool) ->
    case Bool of
	true ->
	    wxCheckBox:set3StateValue(ChkBox, ?wxCHK_CHECKED);
	false ->
	    ignore
    end.

  
create_module_infobox(Parent, ModuleName, TracedDict) ->
    Module = list_to_atom(ModuleName),
    Value = dict:find(Module, TracedDict),
    TracedModFuncs = 
	case Value of
	    {ok, V} ->
		V;
	    error ->
		[]
	end,
    Functions = Module:module_info(functions),
    Choices = lists:sort([{Name, Arity} || {Name, Arity} <- Functions, not(erl_internal:guard_bif(Name, Arity))]),
    ParsedChoices = parse_function_names(Choices),

    Dialog = wxDialog:new(Parent, ?wxID_ANY, ModuleName, [{size, {350, 400}}]),
    Panel = wxPanel:new(Dialog),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    ChoiceSz = wxBoxSizer:new(?wxVERTICAL),
    SelBtnSz = wxBoxSizer:new(?wxHORIZONTAL),
    BtnSz = wxBoxSizer:new(?wxHORIZONTAL),
    
    TxtCtrl = wxTextCtrl:new(Panel, ?wxID_ANY),
    SelBtn = wxButton:new(Panel, ?SELECT, [{label, "Select"}]),
    DeSelBtn = wxButton:new(Panel, ?SELECT, [{label, "Deselect"}]),
    SelAllBtn = wxButton:new(Panel, ?SELECT_ALL, [{label, "Select all"}]),
    DeSelAllBtn = wxButton:new(Panel, ?SELECT_ALL, [{label, "Deselect all"}]),
    CheckListBox = wxCheckListBox:new(Panel, ?wxID_ANY, [{choices, ParsedChoices}, {style, ?wxLB_EXTENDED}]),
    Indices = find_index(TracedModFuncs, Choices),
    lists:foreach(fun(X) ->  wxCheckListBox:check(CheckListBox, X) end, Indices),
    
    OKBtn = wxButton:new(Panel, ?wxID_OK, []),
    CancelBtn = wxButton:new(Panel, ?wxID_CANCEL, []),
    

    wxSizer:add(SelBtnSz, SelBtn, [{flag, ?wxEXPAND}]),
    wxSizer:add(SelBtnSz, DeSelBtn, [{flag, ?wxEXPAND}]),
    wxSizer:add(SelBtnSz, SelAllBtn, [{flag, ?wxEXPAND}]),
    wxSizer:add(SelBtnSz, DeSelAllBtn, [{flag, ?wxEXPAND}]),
    wxSizer:add(ChoiceSz, TxtCtrl, [{flag, ?wxEXPAND}]),
    wxSizer:add(ChoiceSz, SelBtnSz, [{flag, ?wxEXPAND}]),
    wxSizer:add(ChoiceSz, CheckListBox, [{flag, ?wxEXPAND}]),
    wxSizer:add(BtnSz, CancelBtn, [{flag, ?wxEXPAND}]),
    wxSizer:add(BtnSz, OKBtn, [{flag, ?wxEXPAND}]),

    wxSizer:add(MainSz, ChoiceSz),
    wxSizer:add(MainSz, BtnSz, [{flag, ?wxEXPAND}]),
    wxWindow:setSizer(Panel, MainSz),
    Size = wxWindow:getMinSize(Dialog),
    wxWindow:setMinSize(Dialog, Size),
    
    wxButton:connect(SelBtn, command_button_clicked, [{userData, {true, CheckListBox}}]),
    wxButton:connect(DeSelBtn, command_button_clicked, [{userData, {false, CheckListBox}}]),
    wxButton:connect(SelAllBtn, command_button_clicked, [{userData, {true, CheckListBox}}]),
    wxButton:connect(DeSelAllBtn, command_button_clicked, [{userData, {false, CheckListBox}}]),
    wxButton:connect(OKBtn, command_button_clicked, [{userData, {module_infobox, Dialog, Module, CheckListBox, Choices}}]),
    wxButton:connect(CancelBtn, command_button_clicked, [{userData, {module_infobox, Dialog}}]),
    wxTextCtrl:connect(TxtCtrl, command_text_updated, [{userData, {CheckListBox, ParsedChoices, checklistbox}}]),
    wxCheckListBox:connect(CheckListBox, command_checklistbox_toggled),
    wxDialog:show(Dialog).

get_selections(Selections, FunctionList) ->
    get_selections(Selections, FunctionList, []).
get_selections([], _, Acc) ->
    Acc;
get_selections([Int|T], FuncList, Acc) ->
    get_selections(T, FuncList, [lists:nth(Int, FuncList) | Acc]).

find_index(Selections, FunctionList) ->
    find_index(Selections, FunctionList, 1, []).
find_index(Selections, FunctionList, N, Acc) when N > length(FunctionList); Selections =:= [] ->
    Acc;
find_index([Sel|STail] = Selections, FunctionList, N, Acc) ->
    case lists:nth(N, FunctionList) =:= Sel of
	true ->
	    find_index(STail, FunctionList, 1, [N-1|Acc]);
	false ->
	    find_index(Selections, FunctionList, N+1, Acc)
    end.



get_modules(Node) ->
    lists:sort([Module || {Module, _} <- rpc:call(Node, code, all_loaded, [])]).
    
atomlist_to_stringlist(Modules) ->
    [atom_to_list(X) || X <- Modules].



update_tree(Tree, Dict) ->
    RootId = wxTreeCtrl:getRootItem(Tree),
    wxTreeCtrl:deleteChildren(Tree, RootId),
    FillTree = fun(KeyAtom, ValueTupleList, acc_in) ->
		       ParsedList = parse_function_names(ValueTupleList),
		       Module = wxTreeCtrl:appendItem(Tree, RootId, atom_to_list(KeyAtom)),
		       lists:foreach(fun(Function) ->
					     wxTreeCtrl:appendItem(Tree, Module, Function)
				     end,
				     lists:reverse(ParsedList)),
		       acc_in
	       end,
    dict:fold(FillTree, acc_in, Dict),
    wxTreeCtrl:expand(Tree, RootId).

trace_functions(TracedDict) ->
    Trace = fun(KeyAtom, ValueList, acc_in) ->
		    
		    lists:foreach(fun({Function, Arity}) ->
					  Res = dbg:tpl({KeyAtom, Function, Arity}, []),
					      io:format("Module: ~p~n, Function: ~p,  Arity: ~p~n~p~n",
							[KeyAtom, Function, Arity, Res])
				  end,
				  ValueList),
		    acc_in
	    end,
    dict:fold(Trace, acc_in, TracedDict).


parse_function_names(TupleList) ->
    StrList = [atom_to_list(Func) ++ integer_to_list(Arity) || {Func, Arity} <- TupleList],
    parse_function_names(StrList, []).

parse_function_names([], Acc) ->
    lists:reverse(Acc);
parse_function_names([ [H1|T1] = Head | Tail ], Acc) ->
    Parsed = case is_inner_function(H1, T1) of
		 fun_ ->
		     "Fun: " ++ Head;
		 lc ->
		     "List comprehension: " ++ Head;
		 lbc ->
		     "Bit comprehension: " ++ Head;
		 _ ->
		     Head
	     end,
    parse_function_names(Tail, [Parsed | Acc]).


is_inner_function(Head, Tail) when Head =:= []; Tail =:= [] ->
    false;
is_inner_function($', Tail) ->
    [H|T] = Tail,
    is_inner_function(H, T);
is_inner_function($-, Tail) -> 
    [_|T] = lists:dropwhile(fun(Elem) -> Elem =/= $- end, Tail),
    check_innerfunc_sort(T);
is_inner_function(_, _) ->
    false.

check_innerfunc_sort(List) ->
    Fun = lists:prefix("fun-", List),
    Lc = lists:prefix("lc$^", List),
    Lb = lists:prefix("lbc$^", List),   
    if  Fun =:= true -> fun_;
	Lc =:= true -> lc;
	Lb =:= true -> lbc;
	true ->
	    false
    end.
