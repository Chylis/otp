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
-export([get_row/5, get_attr/3]).

-include_lib("wx/include/wx.hrl").
-include_lib("runtime_tools/include/observer_backend.hrl").
-include("observer_defs.hrl").

%% Defines
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
-define(ID_REFRESH_INTERVAL, 204).
-define(ID_DUMP_TO_FILE, 205).
-define(ID_TRACEMENU, 206).
-define(ID_TRACE_ALL_MENU, 207).
-define(ID_TRACE_NEW_MENU, 208).
-define(ID_NO_OF_LINES, 209).
-define(ID_MODULE_INFO, 211).
-define(ID_SYSHIDE, 213). %% ej implementerat
-define(ID_HIDENEW, 214). %% ej implementerat

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
		       match_specs = [],
		       refr_timer = false,
		       tracemenu_opened,
		       procinfo_menu_pids = [],
		       selected_pid,
		       sort_order = {#etop_proc_info.reds, decr},
		       holder}).

start_link(Notebook, Parent) ->
    wx_object:start_link(?MODULE, [Notebook, Parent], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Notebook, Parent]) ->
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
    MatchSpecs = generate_matchspecs(),
    {ProPanel, State#pro_wx_state{match_specs = MatchSpecs}}.

setup(Notebook, Parent, Holder, Count) ->
    ProPanel = wxPanel:new(Notebook, []),
    Menu = create_popup_menu(),

    Grid = create_list_box(ProPanel, Holder, Count),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Grid, [{flag, ?wxEXPAND bor ?wxALL},
			      {proportion, 1},
			      {border,4}]),
    
    wxWindow:setSizer(ProPanel, Sizer),

    State =  #pro_wx_state{parent = Parent,
			   popup_menu  = Menu,
			   grid        = Grid,
			   frame       = ProPanel,
			   parent_notebook = Notebook,
			   tracemenu_opened = false,
			   holder = Holder}, 
    {ProPanel, State}.

generate_matchspecs() ->
    try
	StrMs1 = "[{'_', [], [{return_trace}]}].",
	StrMs2 = "[{'_', [], [{exception_trace}]}].",
	StrMs3 = "[{'_', [], [{message, {caller}}]}].",
	StrMs4 = "[{'_', [], [{message, {process_dump}}]}].",
	
	{ok, Tokens1, _} = erl_scan:string(StrMs1),
	{ok, Tokens2, _} = erl_scan:string(StrMs2),
	{ok, Tokens3, _} = erl_scan:string(StrMs3),
	{ok, Tokens4, _} = erl_scan:string(StrMs4),
	{ok, Term1} = erl_parse:parse_term(Tokens1),
	{ok, Term2} = erl_parse:parse_term(Tokens2),
	{ok, Term3} = erl_parse:parse_term(Tokens3),
	{ok, Term4} = erl_parse:parse_term(Tokens4),
	
	[#match_spec{term_ms = Term1, str_ms = StrMs1},
	 #match_spec{term_ms = Term2, str_ms = StrMs2},
	 #match_spec{term_ms = Term3, str_ms = StrMs3},
	 #match_spec{term_ms = Term4, str_ms = StrMs4}]
    catch
	_:_ ->
	    []
    end.

%% UI-creation

create_pro_menu(Parent) ->
    MenuEntries = [{"View",
		    [#create_menu{id = ?ID_SYSHIDE, text = "Hide system processes"},
		     #create_menu{id = ?ID_HIDENEW, text = "Auto-hide new"},
		     separator,
		     #create_menu{id = ?ID_REFRESH, text = "Refresh"},
		     #create_menu{id = ?ID_REFRESH_INTERVAL, text = "Refresh Interval"}]},
		   {"Options",
		    [#create_menu{id = ?ID_NO_OF_LINES, text = "Number of lines"},
		     #create_menu{id = ?ID_DUMP_TO_FILE, text = "Dump to file"}]},
		   {"Trace",
		    [#create_menu{id = ?ID_TRACEMENU, text = "Trace selected processes"},
		     #create_menu{id = ?ID_TRACE_NEW_MENU, text = "Trace new processes"},
		     #create_menu{id = ?ID_TRACE_ALL_MENU, text = "Trace all processes"}]}
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

get_node() ->
    etop_server ! {get_node, self()},
    receive
	{node, Node} ->
	    Node
    end.
    
change_lines(Int) when is_integer(Int) ->
    etop_server ! {config, {lines, Int}}.

get_lines() ->
    etop_server ! {get_lines, self()},
    receive
	{lines, Lines} ->
	    Lines
    end.

change_intv(NewIntv) ->
    etop_server ! {config, {intverval, NewIntv}}.

get_intv() ->
    etop_server ! {get_intv, self()},
    receive
	{intv, Intv} ->
	    Intv
    end.

refresh_grid(Holder) ->
    io:format("Refreshing~n"),
    etop_server ! {update, Holder}.

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

interval_dialog(Parent, Enabled, Value, Min, Max) ->
    Dialog = wxDialog:new(Parent, ?wxID_ANY, "Update Interval",
			  [{style, ?wxDEFAULT_DIALOG_STYLE bor
				?wxRESIZE_BORDER}]),
    Panel = wxPanel:new(Dialog),
    Check = wxCheckBox:new(Panel, ?wxID_ANY, "Periodical refresh"),
    wxCheckBox:setValue(Check, Enabled),
    Style = ?wxSL_HORIZONTAL bor ?wxSL_AUTOTICKS bor ?wxSL_LABELS,
    Slider = wxSlider:new(Panel, ?wxID_ANY, Value, Min, Max,
			  [{style, Style}, {size, {200, -1}}]),
    wxWindow:enable(Slider, [{enable, Enabled}]),
    InnerSizer = wxBoxSizer:new(?wxVERTICAL),
    Buttons = wxDialog:createButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
    Flags = [{flag, ?wxEXPAND bor ?wxALL}, {border, 2}],
    wxSizer:add(InnerSizer, Check,  Flags),
    wxSizer:add(InnerSizer, Slider, Flags),
    wxPanel:setSizer(Panel, InnerSizer),
    TopSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(TopSizer, Panel, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(TopSizer, Buttons, [{flag, ?wxEXPAND}]),
    wxWindow:setSizerAndFit(Dialog, TopSizer),
    wxSizer:setSizeHints(TopSizer, Dialog),
    wxCheckBox:connect(Check, command_checkbox_clicked,
		       [{callback, fun(#wx{event=#wxCommand{commandInt=Enable0}},_) ->
					   Enable = Enable0 > 0,
					   wxWindow:enable(Slider, [{enable, Enable}])
				   end}]),
    io:format("10~n"), 
    Res = case wxDialog:showModal(Dialog) of
    	      ?wxID_OK ->
    		  {wxCheckBox:isChecked(Check), wxSlider:getValue(Slider)};
    	      ?wxID_CANCEL ->
    		  cancel
    	  end,
    io:format("11~n"),
    wxDialog:destroy(Dialog),
    io:format("12~n"),
    Res.

start_procinfo(Node, Pid, Frame, Opened, View) ->
    case lists:member(Pid, Opened) of
	true ->
	    Opened;
	false ->
	    observer_procinfo:start(Node, Pid, Frame, self(), View),
	    [Pid | Opened]
    end.

%%%%%%%%%%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({holder_updated, Count}, #pro_wx_state{grid = Grid} = State) ->
    wxListCtrl:setItemCount(Grid, Count),
    wxListCtrl:refreshItems(Grid, 0, wxListCtrl:getItemCount(Grid)),
    {noreply, State};

handle_info(refresh_interval, #pro_wx_state{holder = Holder} = State) ->
    io:format("Refresh interval ~p~n", [time()]),
    refresh_grid(Holder),
    {noreply, State};

handle_info({tracemenu_closed, TraceOpts, MatchSpecs}, State) -> %When tracemenu terminates..
    {noreply, State#pro_wx_state{tracemenu_opened = false,
				 trace_options = TraceOpts,
				 match_specs = MatchSpecs}};

handle_info({procinfo_menu_closed, Pid}, 
	    #pro_wx_state{procinfo_menu_pids = Opened} = State) ->
    NewPids = lists:delete(Pid, Opened),
    {noreply, State#pro_wx_state{procinfo_menu_pids = NewPids}};

handle_info({active, Node}, #pro_wx_state{holder = Holder,
					  refr_timer = Timer0,
					  parent = Parent} = State) ->
    create_pro_menu(Parent),
    change_node(Node),
    refresh_grid(Holder),
    Timer = case Timer0 of
		true ->
		    Intv = get_intv(),
		    {ok, Ref} = timer:send_interval(Intv*1000, refresh_interval),
		    Ref;
		false ->
		    false
	    end,
    {noreply, State#pro_wx_state{refr_timer = Timer}};

handle_info(not_active, #pro_wx_state{refr_timer = Timer0} = State) ->
    Timer = case Timer0 of
		false -> false;
		true -> true;
		Timer0 ->
		    timer:cancel(Timer0),
		    true
	    end,
    
    {noreply, State#pro_wx_state{refr_timer=Timer}};

handle_info({node, Node}, #pro_wx_state{holder = Holder} = State) ->
    change_node(Node),
    refresh_grid(Holder),
    {noreply, State};

handle_info(Info, State) ->
    io:format("~p, ~p, Handled unexpected info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(Reason, #pro_wx_state{holder = Holder}) ->
    etop:stop(),
    Holder ! stop,
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


handle_event(#wx{id = ?ID_NO_OF_LINES, event = #wxCommand{type = command_menu_selected}}, 
	     #pro_wx_state{frame = Frame, 
			   holder = Holder} = State) ->
    OldLines = integer_to_list(get_lines()),
    create_line_dialog(Frame, OldLines, Holder),
    {noreply, State};   

handle_event(#wx{id = ?ID_REFRESH, event = #wxCommand{type = command_menu_selected}}, 
	     #pro_wx_state{holder = Holder} = State) ->
    io:format("~p:~p, Klickade på refresh~n", [?MODULE, ?LINE]),
    refresh_grid(Holder),
    {noreply, State};

handle_event(#wx{id = ?ID_REFRESH_INTERVAL},
	     #pro_wx_state{frame = Frame, refr_timer=Timer0} = State) ->
    Intv0 = get_intv(),
    case interval_dialog(Frame, Timer0 /= false, Intv0, 10, 5*60) of
	cancel ->
	    {noreply, State};
	{true, Intv} ->
	    case Timer0 of
		false -> ok;
		_ -> timer:cancel(Timer0)
	    end,
	    change_intv(Intv),
	    {ok, Timer} = timer:send_interval(Intv * 1000, refresh_interval),
	    {noreply, State#pro_wx_state{refr_timer=Timer}};
	{false, _} ->
	    case Timer0 of
		false -> ok;
		_ -> timer:cancel(Timer0)
	    end,
	    {noreply, State#pro_wx_state{refr_timer=false}}
    end;

handle_event(#wx{id = ?ID_KILL}, #pro_wx_state{selected_pid = SelPid} = State) ->
    case SelPid of
	undefined -> ignore;
 	Pid when is_pid(Pid) -> exit(SelPid, kill)
    end,
    {noreply, State#pro_wx_state{selected_pid = undefined}};

handle_event(#wx{id = ?ID_MODULE_INFO, 
		 event = #wxCommand{type = command_menu_selected}}, 
	     #pro_wx_state{frame = Frame,
			   selected_pid = Pid,
			   procinfo_menu_pids = Opened} = State)  ->
    Node = get_node(),
    Opened2 = start_procinfo(Node, Pid, Frame, Opened, module_info),
    {noreply, State#pro_wx_state{procinfo_menu_pids = Opened2}};

handle_event(#wx{id = ?ID_PROC}, 
	     #pro_wx_state{frame = Frame,
			   selected_pid = Pid,
			   procinfo_menu_pids = Opened} = State) ->
    Node = get_node(),
    Opened2 = start_procinfo(Node, Pid, Frame, Opened, procinfo),
    {noreply, State#pro_wx_state{procinfo_menu_pids = Opened2}};
    
handle_event(#wx{id = ?ID_TRACEMENU, event = #wxCommand{type = command_menu_selected}}, 
	     #pro_wx_state{trace_options = Options,
			   match_specs = MatchSpecs,
			   holder = Holder,
			   grid = Grid,
			   tracemenu_opened = false, 
			   frame = Frame} = State) ->
    Node = get_node(),
    IndexList = get_selected_items(Grid),
    PidList = get_selected_pids(Holder, IndexList),
    observer_trace_wx:start(Node,
     			    PidList,
     			    Options,
			    MatchSpecs,
     			    Frame,
     			    self()),
    {noreply,  State#pro_wx_state{tracemenu_opened = true}};

handle_event(#wx{id = ?ID_TRACE_ALL_MENU, event = #wxCommand{type = command_menu_selected}},
	     #pro_wx_state{trace_options = Options,
			   match_specs = MatchSpecs,
			   tracemenu_opened = false,
			   frame = Frame} = State) ->
    Node = get_node(),
    observer_trace_wx:start(Node,
			    all,
			    Options,
			    MatchSpecs,
			    Frame,
			    self()),
    {noreply, State#pro_wx_state{tracemenu_opened = true}};


handle_event(#wx{id = ?ID_TRACE_NEW_MENU, event = #wxCommand{type = command_menu_selected}},
	     #pro_wx_state{trace_options = Options,
			   match_specs = MatchSpecs,
			   tracemenu_opened = false,
			   frame = Frame} = State) ->
    Node = get_node(),
    observer_trace_wx:start(Node,
			    new,
			    Options,
			    MatchSpecs,
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
%% 			   holder = Holder,
%% 			   popup_menu = PopupMenu} = State) -> %Row selected
%%     Pid = get_row(Holder, Row, pid),
%%     wxWindow:popupMenu(Grid, PopupMenu, []),
%%     {noreply, State#pro_wx_state{selected_pid = Pid}};

handle_event(#wx{event = #wxList{type = command_list_item_selected,
				 itemIndex = Row}}, 
	     #pro_wx_state{holder = Holder} = State) ->
    ProcInfo = get_row(Holder, Row, all),
    Str = lists:flatten(io_lib:format("~p", [ProcInfo])),
    io:format("List item selected ~p~n", [Str]),
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
			   procinfo_menu_pids= Opened} = State) ->
    Node = get_node(),
    Pid = get_row(Holder, Row, pid),
    Opened2 = start_procinfo(Node, Pid, Frame, Opened, procinfo),
    {noreply, State#pro_wx_state{procinfo_menu_pids = Opened2}};
	
handle_event(Event, State) ->
    io:format("~p~p, handle event ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.







create_line_dialog(ParentFrame, OldLines, Holder) ->
    Dialog = wxDialog:new(ParentFrame, ?wxID_ANY, "Enter number of lines", 
			  [{style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER}]),
    Panel = wxPanel:new(Dialog),
    TxtCtrl = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, OldLines}]),
    InnerSz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(InnerSz, TxtCtrl, [{flag, ?wxEXPAND bor ?wxALL}]),
    wxPanel:setSizer(Panel, InnerSz),

    OKBtn = wxButton:new(Dialog, ?wxID_OK),
    CancelBtn = wxButton:new(Dialog, ?wxID_CANCEL),
    
    Buttons = wxStdDialogButtonSizer:new(),
    wxStdDialogButtonSizer:addButton(Buttons, OKBtn),
    wxStdDialogButtonSizer:addButton(Buttons, CancelBtn),
    
    TopSz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(TopSz, Panel, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(TopSz, Buttons, [{flag, ?wxEXPAND}]),
    wxStdDialogButtonSizer:realize(Buttons),
    wxWindow:setSizerAndFit(Dialog, TopSz),
    wxSizer:setSizeHints(TopSz, Dialog),

    wxButton:connect(OKBtn, command_button_clicked, 
		     [{callback, 
		       fun(#wx{id = ?wxID_OK,
			       event = #wxCommand{type = command_button_clicked}},_) ->
			       try change_lines(list_to_integer(wxTextCtrl:getValue(TxtCtrl))),
				    refresh_grid(Holder)
			       catch error:badarg -> ignore 
			       end,
			       wxDialog:show(Dialog, [{show, false}])
		       end}]),
    wxButton:connect(CancelBtn, command_button_clicked, 
		     [{callback, 
		       fun(#wx{id = ?wxID_CANCEL,
			       event = #wxCommand{type = command_button_clicked}},_) ->
			       wxDialog:show(Dialog, [{show, false}])
		       end}]),
    wxDialog:show(Dialog).















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
	{update, #etop_info{procinfo = ProcInfo}} ->
	    Parent ! {holder_updated, length(ProcInfo)},
	    table_holder(S0#holder{procinfo = ProcInfo});
	stop ->
	    ok;
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
    Data = case Row+1 > length(SortedInfo) of
	       true ->
		   null;
	       false ->
		   ProcInfo = lists:nth(Row+1, SortedInfo),
		   get_procinfo_data(Col, ProcInfo)
	   end,
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

