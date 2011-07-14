-module(observer_trace_wx).

-export([start/5]).
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include("erltop_defs.hrl").

-define(OPTIONS, 1).
-define(SAVE_BUFFER, 2).
-define(CLOSE, 3).
-define(CLEAR, 4).
-define(SAVE_TRACEOPTS, 5).
-define(LOAD_TRACEOPTS, 6).

-record(match_spec, {alias,
		     term_ms = [],
		     str_ms = [],
		     fun2ms}).

-record(traced_func, {func_name, %atom
		      arity, %integer
		      match_spec = #match_spec{}}). % #match_spec
-record(state, {
	  parent,
	  frame,
	  text_ctrl,
	  trace_options,
	  toggle_button,
	  node,
	  traceoptions_open,
	  traced_procs,
	  traced_funcs = dict:new(), % Key =:= Module::atom, Value =:= [ #traced_func  ]
	  match_specs = []}).%?PREDEFINED_MS}). % [ Matchspecification::String ... ]


start(Node, TracedProcs, TraceOpts, ParentFrame, ParentPid) ->
    wx_object:start_link(?MODULE, [Node, TracedProcs, TraceOpts, ParentFrame, ParentPid], []).

init([Node, TracedProcs, TraceOpts, ParentFrame, ParentPid]) ->
    State = 
	wx:batch(fun() ->
			 create_window(ParentFrame, TraceOpts)
		 end),
    
    Frame = State#state.frame,
    TraceOpts2 = State#state.trace_options,
    TracedFuncs = State#state.traced_funcs,
    StrMs1 = "[{'_', [], [{return_trace}]}]",
    StrMs2 = "[{'_', [], [{exception_trace}]}]",
    StrMs3 = "[{'_', [], [{message, {caller}}]}]",
    StrMs4 = "[{'_', [], [{message, {process_dump}}]}]",
       
    {ok, Tokens1, _} = erl_scan:string(StrMs1 ++ "."),
    {ok, Tokens2, _} = erl_scan:string(StrMs2 ++ "."),
    {ok, Tokens3, _} = erl_scan:string(StrMs3 ++ "."),
    {ok, Tokens4, _} = erl_scan:string(StrMs4 ++ "."),
    {ok, Term1} = erl_parse:parse_term(Tokens1),
    {ok, Term2} = erl_parse:parse_term(Tokens2),
    {ok, Term3} = erl_parse:parse_term(Tokens3),
    {ok, Term4} = erl_parse:parse_term(Tokens4),

    MatchSpecs = [#match_spec{term_ms = Term1, str_ms = StrMs1},
		  #match_spec{term_ms = Term2, str_ms = StrMs2},
		  #match_spec{term_ms = Term3, str_ms = StrMs3},
		  #match_spec{term_ms = Term4, str_ms = StrMs4}],
    
        wx_object:start(observer_traceoptions_wx,
    		    [Frame, self(), Node, TraceOpts2, TracedFuncs, MatchSpecs],
    		    []),
    
    {Frame, State#state{parent = ParentPid,
			node = Node,
			traced_procs = TracedProcs,
			traceoptions_open = true}}.


create_window(ParentFrame, TraceOpts) ->
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
    wxSizer:add(Sizer, ToggleButton, [{flag, ?wxALL},
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
	   trace_options = TraceOpts#trace_options{main_window = false}}.
	   

create_menues(MenuBar) ->
    observer_wx:create_menu(
      [
       {"File", [
		 #create_menu{id = ?LOAD_TRACEOPTS, text = "Load settings"},
		 #create_menu{id = ?SAVE_TRACEOPTS, text = "Save settings"},
		 #create_menu{id = ?SAVE_BUFFER, text = "Save buffer"},
		 #create_menu{id = ?CLOSE, text = "Close"}
		]},
       {"View", [
		 #create_menu{id = ?CLEAR, text = "Clear buffer"}
		]},
       {"Options", [
		    #create_menu{id = ?OPTIONS, text = "Trace options"}
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
		    node = Node,
		    match_specs = MatchSpecs,
		    traceoptions_open = false} = State) ->
    
    wx_object:start(observer_traceoptions_wx,
		    [Frame, self(),  Node, TraceOpts, TracedFuncs, MatchSpecs],
		    []),
    
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

handle_event(#wx{id = ?SAVE_TRACEOPTS,
		 event = #wxCommand{type = command_menu_selected}},
	     #state{trace_options = TraceOpts,
		    traced_funcs = TracedDict,
		    match_specs = MatchSpecs,
		    traced_procs = TracedProcs} = State) ->
    io:format("Not implemented... yet!~n"),
    {noreply, State};

handle_event(#wx{id = ?LOAD_TRACEOPTS,
		 event = #wxCommand{type = command_menu_selected}},
	     #state{trace_options = TraceOpts,
		    traced_funcs = TracedDict,
		    match_specs = MatchSpecs,
		    traced_procs = TracedProcs} = State) ->
    io:format("Not implemented... yet!~n"),
    {noreply, State};
    

handle_event(#wx{event = #wxClose{type = close_window}}, State) ->
    io:format("~p Shutdown. ~p\n", [?MODULE, self()]),
    {stop, shutdown, State};

handle_event(#wx{event = #wxCommand{type = command_togglebutton_clicked, commandInt = 1}}, 
	     #state{node = Node,
		    traced_procs = TracedProcs,
		    traced_funcs = TracedDict, 
		    trace_options = TraceOpts, 
		    text_ctrl = TextCtrl, 
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

handle_event(#wx{event = What}, State) ->
    io:format("~p~p: Unhandled event: ~p ~n", [?MODULE, self(), What]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({updated_traceopts,
	     TraceOpts,
	     MatchSpecs,
	     TracedFuncs}, State) ->
    io:format("~p~p updating traceoptions~n TraceOpts: ~p~n MatchSpecs ~p~n TracedFuncs ~p~n",
	      [?MODULE, ?LINE, TraceOpts, MatchSpecs, TracedFuncs]),
    {noreply, State#state{trace_options = TraceOpts,
			  match_specs = MatchSpecs,
			  traced_funcs = TracedFuncs,
			  traceoptions_open = false}};

handle_info(traceopts_closed, State) ->
    {noreply, State#state{traceoptions_open = false}};

handle_info(Tuple, #state{text_ctrl = TxtCtrl} = State) when is_tuple(Tuple) ->
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

trace_functions(TracedDict) ->
    Trace = fun(KeyAtom, RecordList, acc_in) ->
		    
		    lists:foreach(fun(#traced_func{func_name = Function,
						   arity = Arity,
						   match_spec = #match_spec{term_ms = MS}}) ->
					  Res = dbg:tpl({KeyAtom, Function, Arity}, MS),
					  io:format("Module: ~p~nFunction: ~p Arity: ~p~nMS: ~p~nResult: ~p~n",
						    [KeyAtom, Function, Arity, MS, Res])
				  end,
				  RecordList),
		    acc_in
	    end,
    dict:fold(Trace, acc_in, TracedDict).


