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
		traceoptions_opened,
		traced_procs,
		traced_funcs = dict:new(),
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
    
    create_traceoption_dialog(Frame, Node, TraceOpts, Modules, TracedFuncs),
    
    {Frame, State#state{parent = ParentPid,
			node = Node,
			traced_procs = TracedProcs,
			traceoptions_opened = true,
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
		    traceoptions_opened = false} = State) ->
    
    create_traceoption_dialog(Frame, Node, TraceOpts, Modules, TracedFuncs),
    {noreply, State#state{traceoptions_opened = true}};

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
		 event = #wxCommand{type = command_listbox_doubleclicked},
		userData = Tree},
	     #state{frame = Frame, traced_funcs = TracedDict} = State) ->
    ChosenModule = wxControlWithItems:getStringSelection(ListBox),
    UpdTracedDict = create_moduleinfo(Frame, ChosenModule, TracedDict),
    update_tree(Tree, UpdTracedDict),
    {noreply, State#state{traced_funcs = UpdTracedDict}};

handle_event(#wx{obj = TxtCtrl,
		 event = #wxCommand{type = command_text_updated},
		 userData = ListBox},
	     #state{modules = Modules} = State) ->
    Input = wxTextCtrl:getValue(TxtCtrl),
    AllModules = atomlist_to_stringlist(Modules),
    FilteredModules = lists:filter(fun(Module) ->
				   lists:prefix(Input, Module)
				   end,
				   AllModules),
    wxListBox:clear(ListBox),
    wxListBox:appendStrings(ListBox, FilteredModules),
    {noreply, State};

handle_event(#wx{id = ?wxID_OK,
		 event = #wxCommand{type = command_button_clicked},
		 userData = {Dialog, Boxes}}, 
	     #state{trace_options = TraceOpts, parent = Parent} = State) ->
    UpdTraceOpts = wx:batch(fun() ->
				    read_trace_boxes(Boxes, TraceOpts)
			    end),
    Parent ! {updated_traceopt, UpdTraceOpts},
    wxDialog:show(Dialog, [{show, false}]),
    {noreply, State#state{trace_options = UpdTraceOpts, traceoptions_opened = false}};

handle_event(#wx{id = ?wxID_CANCEL,
		 event = #wxCommand{type = command_button_clicked},
		 userData = Dialog},
	     State) ->
    wxDialog:show(Dialog, [{show, false}]),
    {noreply, State#state{traceoptions_opened = false}};

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
    lists:foreach(fun(Pid) -> dbg:p(Pid, Flags) end, TracedProcs),
    
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
    
    wxTextCtrl:connect(ModuleTxtCtrl, command_text_updated, [{userData, ModuleListBox}]),
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
    
    wxButton:connect(OKBtn, command_button_clicked, [{userData, {Dialog, Boxes}}]),
    wxButton:connect(CancelBtn, command_button_clicked, [{userData, Dialog}]),
    wxDialog:show(Dialog).
    

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

  
create_moduleinfo(Parent, ModuleName, TracedDict) ->

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
    Dialog = wxMultiChoiceDialog:new(Parent, "Select functions to trace", ModuleName, ParsedChoices),
    Indices = find_index(TracedModFuncs, Choices),
    wxMultiChoiceDialog:setSelections(Dialog, Indices),
        
    Selections = case wxMultiChoiceDialog:showModal(Dialog) of
		     ?wxID_OK ->
			 Index = lists:map(fun(Int) ->
						   Int+1
					   end,
					   wxMultiChoiceDialog:getSelections(Dialog)),
			 
			 get_selections(Index, Choices);
		     ?wxID_CANCEL ->
			 TracedModFuncs
		 end,
    case Selections of
	[] ->
	    dict:erase(Module, TracedDict);
	_ ->
	    dict:store(Module, Selections, TracedDict)
    end.

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
