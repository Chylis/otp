-module(observer_procinfo).

-behaviour(wx_object).

-export([start/5]).

-export([init/1, handle_event/2, handle_cast/2, terminate/2, code_change/3, 
	 handle_call/3, handle_info/2]).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-define(CLOSE, 1).
-define(REFRESH, 2).
-define(SELECT_ALL, 3).
-define(MODULE_INFO, 4).
-define(MODULE_CODE, 5).
-define(PROC_INFO, 6).

-record(procinfo_state, {parent,
			 frame,
			 node,
			 pid,
			 module,
			 main_sizer,
			 checkbox_sizer,
			 styled_txtctrl,
			 checklistbox,
			 current_view, % procinfo::atom | module_info::atom | module_code::atom
			 itemlist = [{backtrace, false},
				     {binary, false},
				     {catchlevel, false},
				     {current_function, false},
				     {dictionary, false},
				     {error_handler, true},
				     {garbage_collection, true},
				     {group_leader, true},
				     {heap_size, true},
				     {initial_call, false},
				     {last_calls, false},
				     {links, true},
				     {memory, false},
				     {message_queue_len, true},
				     {messages, false},
				     {monitored_by, false},
				     {monitors, false},
				     {priority, true},
				     {reductions, false},
				     {registered_name, false},
				     {sequential_trace_token, false},
				     {stack_size, false},
				     {status, false},
				     {suspending, false},
				     {total_heap_size, false},
				     {trace, false},
				     {trap_exit,true}]
			}).


			    

start(Node, Process, ParentFrame, Parent, View) ->
    wx_object:start(?MODULE, [Node, Process, ParentFrame, Parent, View], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Node, Process, ParentFrame, Parent, View]) ->
    try
	State = #procinfo_state{parent = Parent,
				node = Node,
				pid = Process
			       },
	ItemList = generate_itemlist(State, View),
	Module = case observer_wx:try_rpc(Node, erlang, process_info, [Process, registered_name]) of
		     [] ->
			 {initial_call, {M, _, _}} = observer_wx:try_rpc(Node, erlang, process_info, [Process, initial_call]),
			 M;
		     {registered_name, M} ->
			 M
		 end,
	{Frame, STC, CheckListBox, CheckSz, MainSz} = setup(ParentFrame, Node, Process, ItemList, Module, View),
	{Frame, State#procinfo_state{frame = Frame,
				     module = Module,
				     styled_txtctrl = STC,
				     checklistbox = CheckListBox,
				     main_sizer = MainSz,
				     checkbox_sizer = CheckSz,
				     current_view = View}}
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(ParentFrame, Node),
	    {stop, badrpc, #procinfo_state{}}
    end.


setup(ParentFrame, Node, Pid, ItemList, Module, View) ->
    Title = get_title(View, Node, Module),
    Frame = wxFrame:new(ParentFrame, ?wxID_ANY, Title, [{size, {900,900}}]),
    Panel = wxPanel:new(Frame, []),
    MainSz = wxBoxSizer:new(?wxHORIZONTAL),
    CheckSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "View"}]),
    BtnSz = wxBoxSizer:new(?wxHORIZONTAL),
    MenuBar = wxMenuBar:new(),
    create_menues(MenuBar, View),
    
    Stc = create_styled_txtctrl(Panel, View),

    Choices = [atom_to_list(Tag) || {Tag, _} <- ItemList],
    CheckListBox = wxCheckListBox:new(Panel, ?wxID_ANY, [{choices, Choices},
							 {style, ?wxLB_EXTENDED},
							 {style, ?wxLB_SORT}, 
							 {style, ?wxLB_NEEDED_SB}]),
    
    check_boxes(CheckListBox, ItemList),
    wxCheckListBox:connect(CheckListBox, command_checklistbox_toggled),
    
    SelAllBtn = wxButton:new(Panel, ?SELECT_ALL, [{label, "Select all"}]),
    DeSelAllBtn = wxButton:new(Panel, ?SELECT_ALL, [{label, "Deselect all"}]),
    
    wxButton:connect(SelAllBtn, command_button_clicked, [{userData, true}]),
    wxButton:connect(DeSelAllBtn, command_button_clicked, [{userData, false}]),
    
    wxSizer:add(MainSz, Stc, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(CheckSz, CheckListBox, [{proportion, 1}]),
    wxSizer:add(BtnSz, SelAllBtn),
    wxSizer:add(BtnSz, DeSelAllBtn),
    wxSizer:add(CheckSz, BtnSz),
    wxSizer:add(MainSz, CheckSz, [{flag, ?wxEXPAND}]),
        
    wxWindow:setSizer(Panel, MainSz),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:show(Frame),
    wxFrame:connect(Frame, close_window),
    wxMenu:connect(Frame, command_menu_selected),
    
    case View of
	procinfo ->
	    load_procinfo_page(Stc, Frame, CheckSz, MainSz, Node, Pid, ItemList, Module);
	module_info ->
	    load_modinfo_page(Stc, Frame, CheckSz, MainSz, Node, Module);
	module_code ->
	    load_modcode_page(Stc, Frame, CheckSz, MainSz, Node, Module)
    end,
    {Frame, Stc, CheckListBox, CheckSz, MainSz}.

generate_itemlist(State, procinfo) ->
    State#procinfo_state.itemlist;
generate_itemlist(State, _) ->
    lists:keymap(fun(_) -> false end, 2, State#procinfo_state.itemlist).

create_menues(MenuBar, View) ->
    [PI, MI, MC, SEP, RFR] = [#create_menu{id = ?PROC_INFO, text = "Process info",
					   type = radio},
			      #create_menu{id = ?MODULE_INFO, text = "Module info",
					   type = radio},
			      #create_menu{id = ?MODULE_CODE, text = "Module code",
					   type = radio},
			      separator,
			      #create_menu{id = ?REFRESH, text = "Refresh"}],
    
    ViewMenu = case View of
		   procinfo ->
		       [PI, MI, MC, SEP, RFR];
		   module_info ->
		       [MI, PI, MC, SEP, RFR];
		   module_code ->
		       [MC, PI, MI, SEP, RFR]
	       end,
    
    observer_wx:create_menu(
      [
       {"File", [#create_menu{id = ?CLOSE, text = "Close"}]},
       {"View", ViewMenu}
      ],
      MenuBar).

check_boxes(CheckListBox, Bool, all) ->
    lists:foreach(fun(Index) ->
			  wxCheckListBox:check(CheckListBox, Index, [{check, Bool}])
		  end,
		  lists:seq(0, wxControlWithItems:getCount(CheckListBox))).
check_boxes(CheckListBox, ItemList) ->
    lists:foldl(fun({_, Bool}, Index) ->
			wxCheckListBox:check(CheckListBox, Index, [{check, Bool}]),
			Index+1
		end,
		0, ItemList).

create_styled_txtctrl(Parent, View) ->
    FixedFont = wxFont:new(11, ?wxFONTFAMILY_TELETYPE, ?wxFONTSTYLE_NORMAL, ?wxNORMAL,[]),
    Stc = wxStyledTextCtrl:new(Parent),
    wxStyledTextCtrl:styleClearAll(Stc),
    wxStyledTextCtrl:styleSetFont(Stc, ?wxSTC_STYLE_DEFAULT, FixedFont),
    wxStyledTextCtrl:setLexer(Stc, ?wxSTC_LEX_ERLANG),
    wxStyledTextCtrl:setMarginType(Stc, 2, ?wxSTC_MARGIN_NUMBER),
    W = wxStyledTextCtrl:textWidth(Stc, ?wxSTC_STYLE_LINENUMBER, "9"),
    wxStyledTextCtrl:setMarginWidth(Stc, 2, W*3),
    
    wxStyledTextCtrl:setSelectionMode(Stc, ?wxSTC_SEL_LINES),
    wxStyledTextCtrl:setUseHorizontalScrollBar(Stc, false),
    
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
		       wxStyledTextCtrl:styleSetFont(Stc, Style, FixedFont),
		       wxStyledTextCtrl:styleSetForeground(Stc, Style, Color)
	       end,
    [SetStyle(Style) || Style <- Styles],
    
    KeyWords = case View of
		   procinfo ->
		       get_procinfo_keywords();
		   module_info ->
		       get_modinfo_keywords();
		   module_code ->
		       get_erl_keywords()
	       end,
    wxStyledTextCtrl:setKeyWords(Stc, 0, KeyWords),
    Stc.


get_title(procinfo, Node, Module) ->
    "Process information: " ++ atom_to_list(Node) ++ ":"
	++ atom_to_list(Module);
get_title(module_info, Node, Module) ->
    "Module information: " ++ atom_to_list(Node) ++ ":"
	++ atom_to_list(Module);
get_title(module_code, Node, Module) ->
    "Module code: " ++ atom_to_list(Node) ++ ":"
	++ atom_to_list(Module).

get_erl_keywords() ->
    L = ["after","begin","case","try","cond","catch","andalso","orelse",
	 "end","fun","if","let","of","query","receive","when","bnot","not",
	 "div","rem","band","and","bor","bxor","bsl","bsr","or","xor"],
    lists:flatten([K ++ " "|| K <- L] ++ [0]).	
get_procinfo_keywords() ->
    L = ["backtrace","binary","catchlevel","current_function","dictionary",
	 "error_handler","garbage_collection","group_leader", "heap_size",
	 "initial_call","last_calls","links","memory","message_queue_len",
	 "messages","monitored_by","monitors", "priority","reductions",
	 "registered_name", "sequential_trace_token","stack_size","status",
	 "suspending", "total_heap_size","trace","trap_exit"],
    lists:flatten([K ++ " "|| K <- L] ++ [0]).
get_modinfo_keywords() ->
    L = ["exports", "imports", "attributes", "compile"],
    lists:flatten([K ++ " "|| K <- L] ++ [0]).

get_formatted_values(Node, Process, ItemList) ->
    TagList = [Tag || {Tag, Bool} <- ItemList, Bool =:= true],
    Values = observer_wx:try_rpc(Node, erlang, process_info, [Process, TagList]),
    lists:flatten([io_lib:format("~p~n", [V]) || V <- Values]).

get_formatted_modinfo(Node, Module) ->
    Info = observer_wx:try_rpc(Node, Module, module_info, []),
    lists:flatten([io_lib:format("~p~n", [I]) || I <- Info]).

get_src_file(Module) ->
    case filename:find_src(Module) of
        {error, {Reason, Mod}} ->
	    io:format("find src reason: ~p~n module: ~p~n", [Reason, Mod]),
            {error, error};
        {SrcFile, _} ->
            case file:read_file_info(SrcFile ++ ".erl") of
                {error, Reason} ->
		    io:format("read file info reason: ~p~n", [Reason]),
                    {error, error};
                {ok, _} ->
                    {ok, SrcFile ++ ".erl"}
            end
    end.


set_text(Stc, Text, text) ->
    wxStyledTextCtrl:setReadOnly(Stc, false),
    wxStyledTextCtrl:setText(Stc, Text),
    wxStyledTextCtrl:setReadOnly(Stc, true);
set_text(Stc, File, file) ->
    wxStyledTextCtrl:setReadOnly(Stc, false),
    wxStyledTextCtrl:loadFile(Stc, File),
    wxStyledTextCtrl:setReadOnly(Stc, true).
    
load_procinfo_page(Stc, Frame, CheckSz, MainSz, Node, Process, ItemList, Module) ->
    wxTopLevelWindow:setTitle(Frame, get_title(procinfo, Node, Module)),
    show_check_sizer(CheckSz, MainSz, true),
    wxStyledTextCtrl:setKeyWords(Stc, 0, get_procinfo_keywords()),
    Txt = get_formatted_values(Node, Process, ItemList),
    set_text(Stc, Txt, text).
load_modinfo_page(Stc, Frame, CheckSz, MainSz, Node, Module) ->
    wxTopLevelWindow:setTitle(Frame, get_title(module_info, Node, Module)),
    show_check_sizer(CheckSz, MainSz, false),
    wxStyledTextCtrl:setKeyWords(Stc, 0, get_modinfo_keywords()),
    Txt = get_formatted_modinfo(Node, Module),
    set_text(Stc, Txt, text).
load_modcode_page(Stc, Frame, CheckSz, MainSz, Node, Module) ->
    wxTopLevelWindow:setTitle(Frame, get_title(module_code, Node, Module)),
    show_check_sizer(CheckSz, MainSz, false),
    wxStyledTextCtrl:setKeyWords(Stc, 0, get_erl_keywords()),
    case get_src_file(Module) of
	{ok, File} ->
	    set_text(Stc, File, file);
	{error, _}->
	    set_text(Stc, "Error! Could not read sourcefile", text)
    end.

show_check_sizer(CheckSz, MainSz, Show) when is_boolean(Show) ->
    case wxSizer:isShown(CheckSz, 0) of
	Show -> ok;
	_ ->
	    wxSizer:show(CheckSz, Show),
	    wxSizer:layout(MainSz)
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Callbacks%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(#wx{event = #wxClose{type = close_window}},
	     #procinfo_state{parent = Parent,
			     pid = Pid} = State) ->
    Parent ! {procinfo_menu_closed, Pid},
    {stop, shutdown, State};

handle_event(#wx{id = ?CLOSE, 
		 event = #wxCommand{type = command_menu_selected}},
	     #procinfo_state{parent = Parent,
			     pid = Pid} = State) ->
    Parent ! {procinfo_menu_closed, Pid},
    {stop, shutdown, State};

handle_event(#wx{id = ?REFRESH, 
		 event = #wxCommand{type = command_menu_selected}},
	     #procinfo_state{frame = Frame,
			     current_view = View,
			     styled_txtctrl = Stc,
			     node = Node,
			     pid = Process,
			     itemlist = ItemList} = State) when View =:= procinfo ->
    try
	Text = get_formatted_values(Node, Process, ItemList),
	set_text(Stc, Text, text),
	{noreply, State}
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Frame, Node),
	    {stop, badrpc, State}
    end;

handle_event(#wx{id = ?REFRESH, 
		 event = #wxCommand{type = command_menu_selected}},
	     #procinfo_state{frame = Frame,
			     node = Node,
			     current_view = View,
			     styled_txtctrl = Stc,
			     module = Module} = State) when View =:= module_info ->
    try
	Text = get_formatted_modinfo(Node, Module),
	set_text(Stc, Text, text),
	{noreply, State}
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Frame, Node),
	    {stop, badrpc, State}
    end;

handle_event(#wx{id = ?REFRESH, 
		 event = #wxCommand{type = command_menu_selected}},
	     #procinfo_state{current_view = View,
			     styled_txtctrl = Stc,
			     module = Module} = State) when View =:= module_code ->
    case get_src_file(Module) of
	{ok, File} ->
	    set_text(Stc, File, file);
	{error, _} ->
	    set_text(Stc, "Error! Could not read sourcefile", text)
    end,
    {noreply, State};


handle_event(#wx{id = ?PROC_INFO, 
		 event = #wxCommand{type = command_menu_selected}},
	     #procinfo_state{frame = Frame,
			     module = Module,
			     checkbox_sizer = CheckSz,
			     main_sizer = MainSz,
			     styled_txtctrl = Stc,
			     node = Node,
			     pid = Process,
			     itemlist = ItemList} = State) ->
    try
	load_procinfo_page(Stc, Frame, CheckSz, MainSz, Node, Process, ItemList, Module),
	{noreply, State#procinfo_state{current_view = procinfo}}
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Frame, Node),
	    {stop, badrpc, State}
    end;

handle_event(#wx{id = ?MODULE_INFO, 
		 event = #wxCommand{type = command_menu_selected}},
	     #procinfo_state{frame = Frame,
			     node = Node,
			     main_sizer = MainSz,
			     checkbox_sizer = CheckSz,
			     styled_txtctrl = Stc,
			     module = Module} = State) ->
    try
	load_modinfo_page(Stc, Frame, CheckSz, MainSz, Node, Module),
	{noreply, State#procinfo_state{current_view = module_info}}
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Frame, Node),
	    {stop, badrpc, State}
    end;

handle_event(#wx{id = ?MODULE_CODE, 
		 event = #wxCommand{type = command_menu_selected}},
	     #procinfo_state{frame = Frame,
			     node = Node,
			     main_sizer = MainSz,
			     checkbox_sizer = CheckSz,
			     styled_txtctrl = Stc,
			     module = Module} = State) ->
    load_modcode_page(Stc, Frame, CheckSz, MainSz, Node, Module),
    {noreply, State#procinfo_state{current_view = module_code}};

handle_event(#wx{event = #wxCommand{type = command_checklistbox_toggled,
				    commandInt = Index},
		 obj = CheckListbox},
	     #procinfo_state{frame = Frame,
			     node = Node,
			     pid = Process,
			     styled_txtctrl = Stc,
			     itemlist = ItemList} = State) ->
    try
	{Tag, _} = lists:nth(Index+1, ItemList),
	ItemList2 = case wxCheckListBox:isChecked(CheckListbox, Index) of
			true ->
			    lists:keyreplace(Tag, 1, ItemList, {Tag, true});
			false ->
			    lists:keyreplace(Tag, 1, ItemList, {Tag, false})
		    end,
	Txt = get_formatted_values(Node, Process, ItemList2),
	set_text(Stc, Txt, text),
	{noreply, State#procinfo_state{itemlist = ItemList2}}

    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Frame, Node),
	    {stop, badrpc, State}
    end;

handle_event(#wx{id = ?SELECT_ALL,
		 event = #wxCommand{type = command_button_clicked},
		 userData = Bool},
	     #procinfo_state{frame = Frame,
			     node = Node,
			     pid = Process,
			     itemlist = ItemList,
			     styled_txtctrl = Stc,
			     checklistbox = CheckListBox} = State) ->
    try
	check_boxes(CheckListBox, Bool, all),
	ItemList2 = lists:keymap(fun(_) ->
					 Bool
				 end,
				 2, ItemList),
	Txt = get_formatted_values(Node, Process, ItemList2),
	set_text(Stc, Txt, text),
	{noreply, State#procinfo_state{itemlist = ItemList2}}
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Frame, Node),
	    {stop, badrpc, State}
    end;

handle_event(Event, State) ->
    io:format("~p: ~p, Handle event: ~p~n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

handle_info(Info, State) -> 
    io:format("~p: ~p, Handle info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

handle_call(Call, _From, State) ->
    io:format("~p ~p: Got call ~p~n",[?MODULE, ?LINE, Call]),
    {reply, ok, State}.

handle_cast(Cast, State) ->
    io:format("~p ~p: Got cast ~p~n", [?MODULE, ?LINE, Cast]),
    {noreply, State}.

terminate(Reason, #procinfo_state{frame = Frame}) ->
    io:format("~p terminating. Reason: ~p~n", [?MODULE, Reason]),
    wxFrame:destroy(Frame),
    ok.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.
