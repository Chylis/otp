-module(observer_trace_wx).

-export([start/5]).
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include("erltop_defs.hrl").

-define(PREDEFINED_MS, [
			#match_spec{ms = "[{'_', [], [{return_trace}]}]"},
			#match_spec{ms = "[{'_', [], [{exception_trace}]}]"},
			#match_spec{ms = "[{'_', [], [{message, {caller}}]}]"},
			#match_spec{ms = "[{'_', [], [{message, {process_dump}}]}]"}
		       ]).

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
-define(FUN2MS, 12).
-define(ADD_MS_BTN, 13).
-define(ADD_MS_ALIAS_BTN, 14).
-define(DIALOG_TRACEOPTS, 15).

-record(match_spec, {alias,
		     ms = [],
		     fun2ms}).

-record(traced_func, {func_name, %atom
		       arity, %integer
		       match_spec = []}). % #match_spec

-record(state, {frame,
		text_ctrl,
		trace_options,
		toggle_button,
		node,
		parent,
		traceoptions_open,
		module_infobox_open,
		traced_procs,
		traced_funcs = dict:new(), % Key =:= Module::atom, Value =:= [ #traced_func  ]
		match_specs = ?PREDEFINED_MS, % [ Matchspecification::String ... ]
		checked_funcs = [],
		tree,
		modules}).

-record(boxes, {send, 'receive', functions, events,
		on_spawn, on_link, all_spawn, all_link}).








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
    MatchSpecs = State#state.match_specs,
    
    Tree = create_traceoption_dialog(Frame, Node, TraceOpts, Modules, TracedFuncs, MatchSpecs),
    
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
    wxFrame:connect(Frame, close_window,[{userData, main_tracerwin}, {skip,true}]),
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
       {"&File", [#create_menu{id = ?OPTIONS, text = "&Trace Options"},
		 #create_menu{id = ?SAVE_BUFFER, text = "&Save buffer"},
		 #create_menu{id = ?CLOSE, text = "&Close"}
		]},
       {"&View", [#create_menu{id = ?CLEAR, text = "&Clear buffer"}
		]}
      ],
      MenuBar).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
						%Main window

handle_event(#wx{id = ?CLOSE, event = #wxCommand{type = command_menu_selected}}, State) ->
    io:format("~p Shutdown. ~p\n", [?MODULE, self()]),
    {stop, shutdown, State};

handle_event(#wx{id = ?OPTIONS, event = #wxCommand{type = command_menu_selected}}, 
	     #state{frame = Frame, trace_options = TraceOpts,
		    traced_funcs = TracedFuncs,
		    modules = Modules, node = Node,
		    match_specs = MatchSpecs,
		    traceoptions_open = false} = State) ->
    
    create_traceoption_dialog(Frame, Node, TraceOpts, Modules, TracedFuncs, MatchSpecs),
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

handle_event(#wx{event = #wxClose{type = close_window},
		 userData = main_tracerwin}, State) ->
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
	     #state{text_ctrl = TxtCtrl, 
		    toggle_button = ToggleBtn} = State) ->
    dbg:stop_clear(),
    wxTextCtrl:appendText(TxtCtrl, "Stop Trace.\n"),
    wxToggleButton:setLabel(ToggleBtn, "Start Trace"),
    {noreply, State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
						%Trace option window

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% All pages

handle_event(#wx{id = ?wxID_OK,
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

handle_event(#wx{id = ?wxID_CANCEL,
		 event = #wxCommand{type = command_button_clicked},
		 userData = {trace_options, Dialog}},
	     State) ->
    wxDialog:show(Dialog, [{show, false}]),
    {noreply, State#state{traceoptions_open = false}};

handle_event(#wx{event = #wxClose{type = close_window},
		 userData = {trace_options, Dialog}}, State) ->
    wxDialog:show(Dialog, [{show, false}]),
    {noreply, State#state{traceoptions_open = false}};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Page - Trace Options

handle_event(#wx{event = #wxCommand{type = command_checkbox_clicked}, userData = UserData},
	     State) ->
    enable(UserData),
    {noreply, State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Page - Function options

handle_event(#wx{obj = ListBox,
		 event = #wxCommand{type = command_listbox_doubleclicked},
		 userData = trace_functions},
	     #state{frame = Frame, 
		    traced_funcs = TracedDict,
		    module_infobox_open = false} = State) ->
    ChosenModule = wxControlWithItems:getStringSelection(ListBox),
    CheckedFuncs = create_module_infobox(Frame, ChosenModule, TracedDict),
    {noreply, State#state{module_infobox_open = true,
			  checked_funcs = CheckedFuncs}};

handle_event(#wx{obj = TxtCtrl,
		 event = #wxCommand{type = command_text_updated},
		 userData = {ListBox, Data, Type}},
	     #state{checked_funcs = CheckedFuncs} = State) -> 

    Input = wxTextCtrl:getValue(TxtCtrl),
    FilteredData = [X || X <- Data, re:run(X, Input) =/= nomatch],
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

handle_event(#wx{obj = Tree, event = #wxTree{type = command_tree_item_activated,
					     item = Item}},
	     #state{frame = Frame, 
		    traced_funcs = TracedDict,
		    match_specs = MatchSpecs,
		    module_infobox_open = false} = State) ->

    
    
    Dialog = wxDialog:new(Frame, ?wxID_ANY, "Match specification"),
    {MatchPanel, MatchSz, ListBox} = create_matchspec_page(Dialog, MatchSpecs),
    ApplyBtn = wxButton:new(MatchPanel, ?wxID_OK, [{label, "Apply"}]),
    CancelBtn = wxButton:new(MatchPanel, ?wxID_CANCEL, []),
    DialogBtnSz = wxStdDialogButtonSizer:new(),
    wxStdDialogButtonSizer:addButton(DialogBtnSz, ApplyBtn),
    wxStdDialogButtonSizer:addButton(DialogBtnSz, CancelBtn),
    wxStdDialogButtonSizer:realize(DialogBtnSz),
    wxSizer:add(MatchSz, DialogBtnSz),
    
    TracedDict2 = case wxDialog:showModal(Dialog) of
		      ?wxID_OK ->
			  IntSelection = wxListBox:getSelection(ListBox),
			  StrSelection = wxControlWithItems:getString(ListBox, IntSelection),
			  
			  MS = find_ms(StrSelection, MatchSpecs),
			  RootId = wxTreeCtrl:getRootItem(Tree),
			  ItemParent = wxTreeCtrl:getItemParent(Tree, Item),
			  if (Item =:= RootId) ->
				  apply_matchspec(MS, TracedDict, root);
			     (ItemParent =:= RootId) ->
				  Module = list_to_atom(wxTreeCtrl:getItemText(Tree, Item)),
				  apply_matchspec(MS, TracedDict, {module, Module});
			     true -> 	
				  ParentModule = list_to_atom(
						   wxTreeCtrl:getItemText(Tree, wxTreeCtrl:getItemParent(Tree, Item))),
				  {Function, Arity} = unparse_function_name((wxTreeCtrl:getItemText(Tree, Item))),
				  apply_matchspec(MS, 
						 TracedDict, 
						 {function,
						  ParentModule,
						  list_to_atom(Function), 
						  list_to_integer(Arity)})
			  end;
		      ?wxID_CANCEL ->
			  TracedDict
    end,
    io:format("Traced before: ~p~n", [TracedDict]),
    io:format("Traced after: ~p~n", [TracedDict2]),
    {noreply, State#state{traced_funcs = TracedDict2}};
    %% case wxTreeCtrl:getItemParent(Tree, Item) =:= wxTreeCtrl:getRootItem(Tree) of
    %% 	true ->
    %% 	    ChosenModule = wxTreeCtrl:getItemText(Tree, Item),
    %% 	    CheckedFuncs = create_module_infobox(Frame, ChosenModule, TracedDict),
    %% 	    {noreply, State#state{module_infobox_open = true,
    %% 				  checked_funcs = CheckedFuncs}};
    %% 	false ->
    %% 	    {noreply, State}
    %% end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Page - Match specifications

handle_event(#wx{obj = ListBox,
		 event = #wxCommand{type = command_listbox_selected},
		 userData = {match_spec, StyledTxtCtrl}},
	     #state{match_specs = MatchSpecs} = State) ->
    SavedTxt = wxControlWithItems:getStringSelection(ListBox),
    MsOrFun = find_and_format_ms(SavedTxt, MatchSpecs),
    wxStyledTextCtrl:setText(StyledTxtCtrl, MsOrFun),
    {noreply, State};

handle_event(#wx{id = ?FUN2MS,
		 event = #wxCommand{type = command_button_clicked}},
	     #state{frame = Frame} = State) ->
    %% FunTxt = wxStyledTextCtrl:getText(Fun2MSTxtCtrl),
    io:format("Not implemented... yet!~n"),
    %% {ok, Tokens,_} = erl_scan:string(FunTxt),
    %% io:format("Tokens ~p~n", [Tokens]),
    %% {ok, Term} = erl_parse:parse_term(Tokens),
    %% MatchSpec = dbg:fun2ms(Term),
    %% wxStyledTextCtrl:appendText(MSTxtCtrl, MatchSpec),
    {noreply, State};

handle_event(#wx{id = ?ADD_MS_BTN,
		 event = #wxCommand{type = command_button_clicked},
		 userData = {StyledTxtCtrl, ListBox}},
	     #state{match_specs = MatchSpecs} = State) ->
    MS = wxStyledTextCtrl:getText(StyledTxtCtrl),
    Occupied = [wxControlWithItems:getString(ListBox, Int) || 
		   Int <- lists:seq(0, wxControlWithItems:getCount(ListBox))],
    MSRecord = #match_spec{ms = MS},
    MatchSpecs2 = case lists:member(MS, Occupied) of
		      true ->
			  MatchSpecs;
		      false ->
			  wxControlWithItems:append(ListBox, MS),
			  lists:reverse([MSRecord | MatchSpecs])
		  end,
    {noreply, State#state{match_specs = MatchSpecs2}};


handle_event(#wx{id = ?ADD_MS_ALIAS_BTN,
		 event = #wxCommand{type = command_button_clicked},
		 userData = {Panel, StyledTxtCtrl, ListBox}},
	     #state{match_specs = MatchSpecs} = State) ->
    Dialog = wxTextEntryDialog:new(Panel, "Enter ms alias: "),
    MatchSpecs2 = case wxDialog:showModal(Dialog) of
		      ?wxID_OK ->
			  Alias = wxTextEntryDialog:getValue(Dialog),
			  Occupied = [wxControlWithItems:getString(ListBox, Int) || 
					 Int <- lists:seq(0, wxControlWithItems:getCount(ListBox))],
			  MS = wxStyledTextCtrl:getText(StyledTxtCtrl),
			  MSRecord = #match_spec{alias = Alias, ms = MS},
			  case lists:member(Alias, Occupied) of
			      true ->				
				  MatchSpecs;
			      false ->
				  wxControlWithItems:append(ListBox, Alias),
				  lists:reverse([MSRecord | MatchSpecs])
			  end;
		      ?wxID_CANCEL ->
			  MatchSpecs
		  end,
    {noreply, State#state{match_specs = MatchSpecs2}};


%% handle_event(#wx{id = ?wxID_SAVE, event = #wxCommand{type = command_button_clicked}}, State) -> %Bör save finnas?
%%     File = ?MODULE:save_options(State#state.trace_options),
%%     State#state.parent ! {save, File},
%%     {noreply, State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
						%Module infobox

handle_event(#wx{id = ?wxID_OK, %Module_infobox OK
		 event = #wxCommand{type = command_button_clicked},
		 userData = {module_infobox, Dialog, Module, ParsedChoices, Choices}},
	     #state{traced_funcs = TracedDict,
		    tree = Tree,
		    checked_funcs = CheckedFuncs} = State) ->
    
    Indices = [I+1 || I <- find_index(CheckedFuncs, ParsedChoices)],
    Selections = get_selections(Indices, Choices),
    TracedDict2 = case Selections of
		     [] ->
			  dict:erase(Module, TracedDict);
		      _ ->
			  Traced = [#traced_func{arity = Arity,
						func_name = Function}
				    || {Function, Arity} <- Selections],
			  dict:store(Module, Traced, TracedDict)
		  end,
    
    update_tree(Tree, TracedDict2),
    wxDialog:show(Dialog, [{show, false}]),
    {noreply, State#state{traced_funcs = TracedDict2,
			  checked_funcs = [],
			  module_infobox_open = false}};

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
    CheckedFuncs2 = case Bool of
			  true ->
			      [X || X <- StrSelections,
				    not(lists:member(X, CheckedFuncs))] ++ CheckedFuncs;
			  false ->
			      CheckedFuncs -- StrSelections
		      end,
    {noreply, State#state{checked_funcs = CheckedFuncs2}};

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

handle_event(#wx{event = #wxClose{type = close_window},
		 userData = {module_infobox, Dialog}}, State) ->
    wxDialog:show(Dialog, [{show, false}]),
    {noreply, State#state{module_infobox_open = false, checked_funcs = []}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	new ->
	    dbg:p(new, Flags);
	_Pids ->
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


create_traceoption_dialog(Frame, Node, Opt, Modules, TracedDict, MatchSpecs) ->
    %%Setup main window
    Dialog = wxDialog:new(Frame, ?DIALOG_TRACEOPTS, "Trace options", [{size, {400, 500}}]),
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
    TreeCtrl = wxTreeCtrl:new(FuncPanel),
    wxTreeCtrl:addRoot(TreeCtrl, atom_to_list(Node)),
    update_tree(TreeCtrl, TracedDict), 
    
    wxTextCtrl:connect(ModuleTxtCtrl, command_text_updated, [{userData,  {ModuleListBox, AllModules, listbox}}]),
    wxListBox:connect(ModuleListBox, command_listbox_doubleclicked, [{userData, trace_functions}]),
    wxTreeCtrl:connect(TreeCtrl, command_tree_item_activated),
    
    wxSizer:add(ModuleSz, ModuleTxtCtrl, [{flag, ?wxEXPAND}]),
    wxSizer:add(ModuleSz, ModuleListBox, [{flag, ?wxEXPAND}]),
    wxSizer:add(TreeSz, TreeCtrl, [{flag, ?wxEXPAND},{proportion, 1}]),
    wxSizer:add(FuncMainSz, ModuleSz, [{flag, ?wxEXPAND}]),
    wxSizer:add(FuncMainSz, TreeSz, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxWindow:setSizer(FuncPanel, FuncMainSz),
    wxNotebook:addPage(Notebook, FuncPanel, "Function options"),
    
    
    %%Setup match specification page
    {MatchPanel, _, _} = create_matchspec_page(Notebook, MatchSpecs),
    wxNotebook:addPage(Notebook, MatchPanel, "Match specifications"),
    
    %%Dialog
    wxSizer:add(MainSz, Notebook, [{proportion, 1}, {flag, ?wxEXPAND}]),
    OKBtn = wxButton:new(Panel, ?wxID_OK, []),
    CancelBtn = wxButton:new(Panel, ?wxID_CANCEL, []),
    DialogBtnSz = wxStdDialogButtonSizer:new(),
    wxStdDialogButtonSizer:addButton(DialogBtnSz, OKBtn),
    wxStdDialogButtonSizer:addButton(DialogBtnSz, CancelBtn),
    wxStdDialogButtonSizer:realize(DialogBtnSz),
    wxSizer:add(MainSz, DialogBtnSz),
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
    wxDialog:connect(Dialog, close_window, [{userData, {trace_options, Dialog}}]),
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


create_styled_txtctrl(Parent) ->
    FixedFont = wxFont:new(12, ?wxFONTFAMILY_TELETYPE, ?wxFONTSTYLE_NORMAL, ?wxNORMAL,[]),
    Ed = wxStyledTextCtrl:new(Parent),
    wxStyledTextCtrl:styleClearAll(Ed),
    wxStyledTextCtrl:styleSetFont(Ed, ?wxSTC_STYLE_DEFAULT, FixedFont),
    wxStyledTextCtrl:setLexer(Ed, ?wxSTC_LEX_ERLANG),
    wxStyledTextCtrl:setMarginType(Ed, 1, ?wxSTC_MARGIN_NUMBER),
    wxStyledTextCtrl:setSelectionMode(Ed, ?wxSTC_SEL_LINES),
    wxStyledTextCtrl:setUseHorizontalScrollBar(Ed, false),
    
    Styles =  [{?wxSTC_ERLANG_DEFAULT,  {0,0,0}},
	       {?wxSTC_ERLANG_COMMENT,  {160,53,35}},
	       {?wxSTC_ERLANG_VARIABLE, {150,100,40}},
	       {?wxSTC_ERLANG_NUMBER,   {5,5,100}},
	       {?wxSTC_ERLANG_KEYWORD,  {130,40,172}},
	       {?wxSTC_ERLANG_STRING,   {170,45,132}},
	       {?wxSTC_ERLANG_OPERATOR, {30,0,0}},
	       {?wxSTC_ERLANG_ATOM,     {0,0,0}},
	       {?wxSTC_ERLANG_FUNCTION_NAME, {64,102,244}},
	       {?wxSTC_ERLANG_CHARACTER,{236,155,172}},
	       {?wxSTC_ERLANG_MACRO,    {40,144,170}},
	       {?wxSTC_ERLANG_RECORD,   {40,100,20}},
	       {?wxSTC_ERLANG_SEPARATOR,{0,0,0}},
	       {?wxSTC_ERLANG_NODE_NAME,{0,0,0}}],
    SetStyle = fun({Style, Color}) ->
		       wxStyledTextCtrl:styleSetFont(Ed, Style, FixedFont),
		       wxStyledTextCtrl:styleSetForeground(Ed, Style, Color)
	       end,
    [SetStyle(Style) || Style <- Styles],
    wxStyledTextCtrl:setKeyWords(Ed, 0, keyWords()),

    Ed.

keyWords() ->
    L = ["after","begin","case","try","cond","catch","andalso","orelse",
	 "end","fun","if","let","of","query","receive","when","bnot","not",
	 "div","rem","band","and","bor","bxor","bsl","bsr","or","xor"],
    lists:flatten([K ++ " " || K <- L] ++ [0]).


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
    TracedModRecs = 
	case Value of
	    {ok, V} ->
		V;
	    error ->
		[]
	end,
    Functions = Module:module_info(functions),
    Choices = lists:sort([{Name, Arity} || {Name, Arity} <- Functions, not(erl_internal:guard_bif(Name, Arity))]),
    ParsedChoices = parse_function_names(Choices),

    Dialog = wxDialog:new(Parent, ?wxID_ANY, ModuleName),
    Panel = wxPanel:new(Dialog),
    MainSz = wxBoxSizer:new(?wxVERTICAL),

    SelBtnSz = wxBoxSizer:new(?wxHORIZONTAL),
    TxtCtrl = wxTextCtrl:new(Panel, ?wxID_ANY),
    SelBtn = wxButton:new(Panel, ?SELECT, [{label, "Select"}]),
    DeSelBtn = wxButton:new(Panel, ?SELECT, [{label, "Deselect"}]),
    SelAllBtn = wxButton:new(Panel, ?SELECT_ALL, [{label, "Select all"}]),
    DeSelAllBtn = wxButton:new(Panel, ?SELECT_ALL, [{label, "Deselect all"}]),
    CheckListBox = wxCheckListBox:new(Panel, ?wxID_ANY, [{choices, ParsedChoices}, {style, ?wxLB_EXTENDED}]),
    Indices = find_index(TracedModRecs, Choices),
    lists:foreach(fun(X) ->  wxCheckListBox:check(CheckListBox, X) end, Indices),
    Selections = [wxControlWithItems:getString(CheckListBox, I) || I <- Indices],
    
    OKBtn = wxButton:new(Panel, ?wxID_OK, []),
    CancelBtn = wxButton:new(Panel, ?wxID_CANCEL, []),
    DialogBtnSz = wxStdDialogButtonSizer:new(),
    wxStdDialogButtonSizer:addButton(DialogBtnSz, OKBtn),
    wxStdDialogButtonSizer:addButton(DialogBtnSz, CancelBtn),
    wxStdDialogButtonSizer:realize(DialogBtnSz),
    
    wxSizer:add(SelBtnSz, SelBtn),
    wxSizer:add(SelBtnSz, DeSelBtn),
    wxSizer:add(SelBtnSz, SelAllBtn),
    wxSizer:add(SelBtnSz, DeSelAllBtn),
    wxSizer:add(MainSz, TxtCtrl, [{flag, ?wxEXPAND}]),
    wxSizer:add(MainSz, CheckListBox, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(MainSz, SelBtnSz, [{flag, ?wxEXPAND}]),
    wxSizer:add(MainSz, DialogBtnSz),
    wxWindow:setSizer(Panel, MainSz),
    
    wxButton:connect(SelBtn, command_button_clicked, [{userData, {true, CheckListBox}}]),
    wxButton:connect(DeSelBtn, command_button_clicked, [{userData, {false, CheckListBox}}]),
    wxButton:connect(SelAllBtn, command_button_clicked, [{userData, {true, CheckListBox}}]),
    wxButton:connect(DeSelAllBtn, command_button_clicked, [{userData, {false, CheckListBox}}]),
    wxButton:connect(OKBtn, command_button_clicked, [{userData, {module_infobox, Dialog, Module, ParsedChoices, Choices}}]),
    wxButton:connect(CancelBtn, command_button_clicked, [{userData, {module_infobox, Dialog}}]),
    wxTextCtrl:connect(TxtCtrl, command_text_updated, [{userData, {CheckListBox, ParsedChoices, checklistbox}}]),
    wxCheckListBox:connect(CheckListBox, command_checklistbox_toggled),
    wxDialog:connect(Dialog, close_window, [{userData, {module_infobox, Dialog}}]),
    wxDialog:show(Dialog),
    Selections.
    
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

find_index([#traced_func{func_name = Name, arity = Arity} |STail] = Selections, 
	   FunctionList, N, Acc) ->
    {Fname, A} = lists:nth(N, FunctionList),
    case (Fname =:= Name) and (A =:= Arity) of
	true ->
	    find_index(STail, FunctionList, 1, [N-1|Acc]);
	false ->
	    find_index(Selections, FunctionList, N+1, Acc)
    end;

find_index([Sel|STail] = Selections, FunctionList, N, Acc) when is_list(Sel) ->
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
    FillTree = fun(KeyAtom, RecordList, acc_in) ->
		       ParsedList = parse_record_function_names(RecordList),
		       Module = wxTreeCtrl:appendItem(Tree, RootId, atom_to_list(KeyAtom)),
		       lists:foreach(fun(Function) ->
					     wxTreeCtrl:appendItem(Tree, Module, Function)
				     end,
				     ParsedList),
		       wxTreeCtrl:sortChildren(Tree, Module),
		       acc_in
	       end,
    dict:fold(FillTree, acc_in, Dict),
    wxTreeCtrl:sortChildren(Tree, RootId),
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


parse_record_function_names(RecordList) ->
    StrList = [atom_to_list(FName) ++ integer_to_list(Arity) 
	       || #traced_func{func_name = FName, arity = Arity} <- RecordList],
    parse_function_names(StrList, []).

parse_function_names(Choices) ->
    StrList = [atom_to_list(Name) ++ integer_to_list(Arity) || {Name, Arity} <- Choices],
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


unparse_function_name("Fun: " ++ Body) ->
    io:format("Body: ~p~n", [Body]),
    Arity = lists:last(Body),
    Function = Body -- [Arity],
    {Function, [Arity]};
unparse_function_name("List comprehension: " ++ Body) ->
    Arity = lists:last(Body),
    Function = Body -- [Arity],
    {Function, [Arity]};
unparse_function_name("Bit comprehension: " ++ Body) ->
    Arity = lists:last(Body),
    Function = Body -- [Arity],
    {Function, [Arity]};
unparse_function_name(Body) ->
    Arity = lists:last(Body),
    Function = Body -- [Arity],
    {Function, [Arity]}.


show_ms_in_savedlistbox(MatchSpecList) ->
    MsOrAlias = fun(#match_spec{alias = A, ms = M}) ->
		   case A of
		       undefined ->
			   M;
		       _ ->
			   A
		   end
	   end,
    [MsOrAlias(X) || X <- MatchSpecList].
	       



find_and_format_ms(Ms, [ #match_spec{ms = Spec, alias = Alias, fun2ms = Fun} | T ]) ->
    case (Ms =:= Spec) or (Ms =:= Alias) of
	true ->
	    case Fun of
		undefined ->
		    Spec;
		Fun ->
		    Fun
	    end;
	false ->
	    find_and_format_ms(Ms, T)
    end.

find_ms([], _) ->
    [];
find_ms(Str, [ #match_spec{ms = Spec, alias = Alias} = MS | T ]) ->
    case (Str =:= Spec) or (Str =:= Alias) of
	true ->
	    MS;
	false ->
	    find_ms(Str, T)
    end.
    
apply_matchspec(MatchSpec, TracedDict, root) ->
    UpdateMS = fun(_Key, RecordList) ->
		       [X#traced_func{match_spec = MatchSpec} || X <- RecordList]
	       end,
    dict:map(UpdateMS, TracedDict);
apply_matchspec(MatchSpec, TracedDict, {module, Module}) ->
    RecordList = dict:fetch(Module, TracedDict),
    RecordList2 = [X#traced_func{match_spec = MatchSpec} || X <- RecordList],
    dict:store(Module, RecordList2, TracedDict);
apply_matchspec(MatchSpec, TracedDict, {function, Module, Function, Arity}) ->
    RecordList = dict:fetch(Module, TracedDict),
    [OldFunc] = [X || #traced_func{func_name = Name,
				   arity = A} = X <- RecordList,
		      Name =:= Function, A =:= Arity],
    NewFunc = OldFunc#traced_func{match_spec = MatchSpec},
    
    RecordList2 = [NewFunc | [X || #traced_func{func_name = Name,
					       arity = A} = X <- RecordList, 
				   (Name =/= Function) or (A =/= Arity)]],
    dict:store(Module, RecordList2, TracedDict).

create_matchspec_page(Parent, MatchSpecs) ->
    Panel = wxPanel:new(Parent),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    TxtSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Match specification:"}]),
    BtnSz = wxBoxSizer:new(?wxHORIZONTAL),
    SavedSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Saved match specifications:"}]),
    
    TxtCtrl = create_styled_txtctrl(Panel),
    wxSizer:add(TxtSz, TxtCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),
    
    AddMsBtn = wxButton:new(Panel, ?ADD_MS_BTN, [{label, "Add"}]),
    AddMsAliasBtn = wxButton:new(Panel, ?ADD_MS_ALIAS_BTN, [{label, "Add with alias"}]),
    Fun2MSBtn = wxButton:new(Panel, ?FUN2MS, [{label, "Fun2ms"}]),
    wxSizer:add(BtnSz, AddMsBtn),
    wxSizer:add(BtnSz, AddMsAliasBtn),
    wxSizer:add(BtnSz, Fun2MSBtn),
    
    Choices = show_ms_in_savedlistbox(MatchSpecs),
    SavedMSListBox = wxListBox:new(Panel, ?wxID_ANY, [{choices, Choices}]),
    wxSizer:add(SavedSz, SavedMSListBox, [{flag, ?wxEXPAND}, {proportion, 1}]),
    
    wxButton:connect(AddMsBtn, command_button_clicked, [{userData, {TxtCtrl, SavedMSListBox}}]),
    wxButton:connect(AddMsAliasBtn, command_button_clicked, [{userData, {Panel, TxtCtrl, SavedMSListBox}}]),
    wxButton:connect(Fun2MSBtn, command_button_clicked),
    wxListBox:connect(SavedMSListBox, command_listbox_selected, [{userData, {match_spec, TxtCtrl}}]),
    wxSizer:add(MainSz, TxtSz, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(MainSz, BtnSz),
    wxSizer:add(MainSz, SavedSz, [{flag, ?wxEXPAND}, {proportion, 1}]),
    
    wxWindow:setSizer(Panel, MainSz),
    {Panel, MainSz, SavedMSListBox}.
