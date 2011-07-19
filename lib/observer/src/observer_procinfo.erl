-module(observer_procinfo).

-behaviour(wx_object).

-export([start/4]).

-export([init/1, handle_event/2, handle_cast/2, terminate/2, code_change/3, 
	 handle_call/3, handle_info/2]).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-define(CLOSE, 1).
-define(REFRESH, 2).
-define(SELECT_ALL, 3).

-record(procinfo_state, {parent,
			 frame,
			 node,
			 pid,
			 styled_txtctrl,
			 checklistbox,
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
				     {sequential_trace_token, false},
				     {stack_size, false},
				     {status, false},
				     {suspending, false},
				     {total_heap_size, false},
				     {trace, false},
				     {trap_exit,true}]
			}).

start(Node, Process, ParentFrame, Parent) ->
    wx_object:start(?MODULE, [Node, Process, ParentFrame, Parent], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Node, Process, ParentFrame, Parent]) ->
    State = #procinfo_state{parent = Parent,
			    node = Node,
			    pid = Process
			   },
    {Frame, STC, CheckListBox} = setup(ParentFrame, Node, Process, State#procinfo_state.itemlist),
    {Frame, State#procinfo_state{frame = Frame,
				 styled_txtctrl = STC,
				 checklistbox = CheckListBox}}.

setup(ParentFrame, Node, Pid, ItemList) ->
    Frame = wxFrame:new(ParentFrame, ?wxID_ANY, "Process information", [{size, {900,900}}]),
    Panel = wxPanel:new(Frame, []),
    MainSz = wxBoxSizer:new(?wxHORIZONTAL),
    CheckSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "View"}]),
    BtnSz = wxBoxSizer:new(?wxHORIZONTAL),
    MenuBar = wxMenuBar:new(),
    create_menues(MenuBar),
    
    StyledTxtCtrl = create_styled_txtctrl(Panel),
    wxStyledTextCtrl:setText(StyledTxtCtrl, 
			     get_formatted_values(Node, Pid, ItemList)),

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
    
    wxSizer:add(MainSz, StyledTxtCtrl, [{proportion, 1}, {flag, ?wxEXPAND}]),
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
    {Frame, StyledTxtCtrl, CheckListBox}.

create_menues(MenuBar) ->
    observer_wx:create_menu(
      [
       {"File", [#create_menu{id = ?CLOSE, text = "Close"}]},
       {"View", [#create_menu{id = ?REFRESH, text = "Refresh"}]}
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

create_styled_txtctrl(Parent) ->
    FixedFont = wxFont:new(12, ?wxFONTFAMILY_TELETYPE, ?wxFONTSTYLE_NORMAL, ?wxNORMAL,[]),
    Stc = wxStyledTextCtrl:new(Parent),
    wxStyledTextCtrl:styleClearAll(Stc),
    wxStyledTextCtrl:styleSetFont(Stc, ?wxSTC_STYLE_DEFAULT, FixedFont),
    wxStyledTextCtrl:setLexer(Stc, ?wxSTC_LEX_ERLANG),
    wxStyledTextCtrl:setMarginType(Stc, 1, ?wxSTC_MARGIN_NUMBER),
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
    wxStyledTextCtrl:setKeyWords(Stc, 0, keyWords()),
    Stc.

keyWords() ->
    L = ["backtrace","binary","catchlevel","current_function","dictionary",
	 "error_handler","garbage_collection","group_leader", "heap_size",
	 "initial_call","last_calls","links","memory","message_queue_len",
	 "messages","monitored_by","monitors", "priority","reductions",
	 "sequential_trace_token","stack_size","status","suspending",
	 "total_heap_size","trace","trap_exit"],
    lists:flatten([K ++ " "|| K <- L] ++ [0]).

get_formatted_values(Node, Process, ItemList) ->
    TagList = [Tag || {Tag, Bool} <- ItemList, Bool =:= true],
    Values = rpc:call(Node, erlang, process_info, [Process, TagList]),
    lists:flatten(io_lib:format("~p", [Values])).
    
    
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
	     #procinfo_state{styled_txtctrl = Stc,
			     node = Node,
			     pid = Process,
			     itemlist = ItemList} = State) ->
    wxStyledTextCtrl:setText(Stc, get_formatted_values(Node, Process, ItemList)),
    {noreply, State};

handle_event(#wx{event = #wxCommand{type = command_checklistbox_toggled,
				    commandInt = Index},
		 obj = CheckListbox},
	     #procinfo_state{node = Node,
			     pid = Process,
			     styled_txtctrl = STC,
			     itemlist = ItemList} = State) ->
    {Tag, _} = lists:nth(Index+1, ItemList),
    ItemList2 = case wxCheckListBox:isChecked(CheckListbox, Index) of
		    true ->
			lists:keyreplace(Tag, 1, ItemList, {Tag, true});
		    false ->
			lists:keyreplace(Tag, 1, ItemList, {Tag, false})
		end,
    StyledProcInfo = get_formatted_values(Node, Process, ItemList2),
    wxStyledTextCtrl:setText(STC, StyledProcInfo),
    {noreply, State#procinfo_state{itemlist = ItemList2}};

handle_event(#wx{id = ?SELECT_ALL,
		 event = #wxCommand{type = command_button_clicked},
		 userData = Bool},
	     #procinfo_state{node = Node,
			     pid = Process,
			     itemlist = ItemList,
			     styled_txtctrl = Stc,
			     checklistbox = CheckListBox} = State) ->
    check_boxes(CheckListBox, Bool, all),
    ItemList2 = lists:keymap(fun(_) ->
				     Bool
			     end,
			     2, ItemList),
    wxStyledTextCtrl:setText(Stc, get_formatted_values(Node, Process, ItemList2)),
    {noreply, State#procinfo_state{itemlist = ItemList2}};
    

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
