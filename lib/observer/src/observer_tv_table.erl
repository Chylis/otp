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

-module(observer_tv_table).

-export([start_link/2]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_sync_event/3, handle_cast/2]).

-export([get_table/3]).

-import(observer_pro_wx, [to_str/1]).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-include("observer_tv.hrl").

-define(ID_TABINFO, 400).
-define(ID_REFRESH, 401).
-define(ID_REFRESH_INTERVAL, 402).
-define(ID_EDIT, 403).
-define(ID_DELETE, 404).
-define(ID_SEARCH, 405).

-define(SEARCH_ENTRY, 420).
-define(GOTO_ENTRY,   421).

-define(DEFAULT_COL_WIDTH, 100).

-record(state,
	{
	  parent,
	  frame,
	  grid,
	  status,
	  sizer,
	  search,
	  node=node(),
	  columns,
	  pid,
	  source,
	  tab,
	  attrs
	}).

-record(opt,
	{
	  sort_key=2,
	  sort_incr=true
	}).

-record(attrs, {even, odd, deleted, changed, searched}).

-record(search,
	{enable=true,          %  Subwindow is enabled
	 win,                  %  Sash Sub window obj
	 name,                 %  name

	 search,               %  Search input ctrl
	 goto,                 %  Goto  input ctrl
	 radio,                %  Radio buttons

	 find                  %  Search string
	}).

-record(find, {start,              % start pos
	       strlen,             % Found
	       found               % false
	      }).

start_link(Parent, Opts) ->
    wx_object:start_link(?MODULE, [Parent, Opts], []).

init([Parent, Opts]) ->
    Source = proplists:get_value(type, Opts),
    Table  = proplists:get_value(table, Opts),
    Node   = proplists:get_value(node, Opts),
    Title0 = atom_to_list(Table#tab.name) ++ " @ " ++ atom_to_list(Node),
    Title = case Source of
		ets -> "TV Ets: " ++ Title0;
		mnesia -> "TV Mnesia: " ++ Title0
	    end,
    Frame = wxFrame:new(Parent, ?wxID_ANY, Title, [{size, {800, 300}}]),
    IconFile = filename:join(code:priv_dir(observer), "erlang_observer.png"),
    Icon = wxIcon:new(IconFile, [{type,?wxBITMAP_TYPE_PNG}]),
    wxFrame:setIcon(Frame, Icon),
    wxIcon:destroy(Icon),
    MenuBar = wxMenuBar:new(),
    create_menus(MenuBar),
    wxFrame:setMenuBar(Frame, MenuBar),
    %% wxFrame:setAcceleratorTable(Frame, AccelTable),
    wxMenu:connect(Frame, command_menu_selected),

    StatusBar = wxFrame:createStatusBar(Frame, []),
    try
	TabId = table_id(Table),
	ColumnNames = column_names(Node, Source, TabId),
	KeyPos = key_pos(Node, Source, TabId),

	Attrs = create_attrs(),

	Self = self(),
	Holder = spawn_link(fun() ->
				    init_table_holder(Self, Table, Source,
						      length(ColumnNames), Node, Attrs)
			    end),

	Panel = wxPanel:new(Frame),
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	Style = ?wxLC_REPORT bor ?wxLC_VIRTUAL bor ?wxLC_SINGLE_SEL bor ?wxLC_HRULES,
	Grid = wxListCtrl:new(Panel, [{style, Style},
				      {onGetItemText,
				       fun(_, Item,Col) -> get_row(Holder, Item, Col+1) end},
				      {onGetItemAttr,
				       fun(_, Item) -> get_attr(Holder, Item) end}
				     ]),
	wxListCtrl:connect(Grid, command_list_item_activated),
	wxListCtrl:connect(Grid, command_list_item_selected),
	wxListCtrl:connect(Grid, command_list_col_click),
	wxListCtrl:connect(Grid, size, [{skip, true}]),
	wxWindow:setFocus(Grid),

	Search = search_area(Panel),
	wxSizer:add(Sizer, Grid,
		    [{flag, ?wxEXPAND bor ?wxALL}, {proportion, 1}, {border, 5}]),
	wxSizer:add(Sizer, Search#search.win,
		    [{flag,?wxEXPAND bor ?wxLEFT bor ?wxRIGHT bor
			  ?wxRESERVE_SPACE_EVEN_IF_HIDDEN},
		     {border, 5}]),
	wxWindow:setSizer(Panel, Sizer),
	wxSizer:hide(Sizer, Search#search.win),

	Cols = add_columns(Grid, 0, ColumnNames),
	wxFrame:show(Frame),
	{Panel, #state{frame=Frame, grid=Grid, status=StatusBar, search=Search,
		       sizer = Sizer,
		       parent=Parent, columns=Cols,
		       pid=Holder, source=Source, tab=Table#tab{keypos=KeyPos},
		       attrs=Attrs}}
    catch node_or_table_down ->
	    wxFrame:destroy(Frame),
	    stop
    end.

add_columns(Grid, Start, ColumnNames) ->
    Li = wxListItem:new(),
    AddListEntry = fun(Name, Col) ->
			   wxListItem:setText(Li, to_str(Name)),
			   wxListItem:setAlign(Li, ?wxLIST_FORMAT_LEFT),
			   wxListCtrl:insertColumn(Grid, Col, Li),
			   wxListCtrl:setColumnWidth(Grid, Col, ?DEFAULT_COL_WIDTH),
			   Col + 1
		   end,
    Cols = lists:foldl(AddListEntry, Start, ColumnNames),
    wxListItem:destroy(Li),
    Cols.

create_menus(MB) ->
    File = wxMenu:new(),
    wxMenu:append(File, ?ID_TABINFO, "Table Information"),
    wxMenu:append(File, ?wxID_CLOSE, "Close"),
    wxMenuBar:append(MB, File, "File"),
    Edit = wxMenu:new(),
    wxMenu:append(Edit, ?ID_EDIT, "Edit Object"),
    wxMenu:append(Edit, ?ID_DELETE, "Delete Object"),
    wxMenu:appendSeparator(Edit),
    wxMenu:append(Edit, ?ID_SEARCH, "Search\tCtrl-S"),
    wxMenu:appendSeparator(Edit),
    wxMenu:append(Edit, ?ID_REFRESH, "Refresh\tCtrl-R"),
    wxMenu:append(Edit, ?ID_REFRESH_INTERVAL, "Refresh interval..."),
    wxMenuBar:append(MB, Edit, "Edit"),
    Help = wxMenu:new(),
    wxMenu:append(Help, ?wxID_HELP, "Help"),
    wxMenuBar:append(MB, Help, "Help"),
    ok.

search_area(Parent) ->
    HSz = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(HSz, wxStaticText:new(Parent, ?wxID_ANY, "Find:"),
		[{flag,?wxALIGN_CENTER_VERTICAL}]),
    TC1 = wxTextCtrl:new(Parent, ?SEARCH_ENTRY, [{style, ?wxTE_PROCESS_ENTER}]),
    wxSizer:add(HSz, TC1,  [{proportion,3}, {flag, ?wxEXPAND}]),
    Nbtn = wxRadioButton:new(Parent, ?wxID_ANY, "Next"),
    wxRadioButton:setValue(Nbtn, true),
    wxSizer:add(HSz,Nbtn,[{flag,?wxALIGN_CENTER_VERTICAL}]),
    Pbtn = wxRadioButton:new(Parent, ?wxID_ANY, "Previous"),
    wxSizer:add(HSz,Pbtn,[{flag,?wxALIGN_CENTER_VERTICAL}]),
    Cbtn = wxCheckBox:new(Parent, ?wxID_ANY, "Match Case"),
    wxSizer:add(HSz,Cbtn,[{flag,?wxALIGN_CENTER_VERTICAL}]),
    wxSizer:add(HSz, 15,15, [{proportion,1}, {flag, ?wxEXPAND}]),
    wxSizer:add(HSz, wxStaticText:new(Parent, ?wxID_ANY, "Goto Entry:"),
		[{flag,?wxALIGN_CENTER_VERTICAL}]),
    TC2 = wxTextCtrl:new(Parent, ?GOTO_ENTRY, [{style, ?wxTE_PROCESS_ENTER}]),
    wxSizer:add(HSz, TC2,  [{proportion,0}, {flag, ?wxEXPAND}]),
    wxTextCtrl:connect(TC1, command_text_updated),
    wxTextCtrl:connect(TC1, command_text_enter),
    wxTextCtrl:connect(TC1, kill_focus),
    wxTextCtrl:connect(TC2, command_text_enter),
    wxWindow:connect(Parent, command_button_clicked),

    #search{name='Search Area', win=HSz,
	    search=TC1,goto=TC2,radio={Nbtn,Pbtn,Cbtn}}.

%% handle_event(#wx{id=?ID_REFRESH},State = #state{node=Node, grid=Grid}) ->
%%     {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_col_click, col=Col}},
	     State = #state{pid=Pid}) ->
    Pid ! {sort, Col+1},
    {noreply, State};

handle_event(#wx{event=#wxSize{size={W,_}}},  State=#state{grid=Grid}) ->
    wx:batch(fun() ->
		     Cols = wxListCtrl:getColumnCount(Grid),
		     Last = lists:foldl(fun(I, Last) ->
		     				Last - wxListCtrl:getColumnWidth(Grid, I)
		     			end, W-2, lists:seq(0, Cols - 2)),
		     Size = max(?DEFAULT_COL_WIDTH, Last),
		     wxListCtrl:setColumnWidth(Grid, Cols-1, Size)
	     end),
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_selected, itemIndex=Index}},
	     State = #state{pid=Pid, grid=Grid, status=StatusBar}) ->
    N = wxListCtrl:getItemCount(Grid),
    Str = get_row(Pid, Index, all),
    wxStatusBar:setStatusText(StatusBar, io_lib:format("Objects: ~w: ~s",[N, Str])),
    {noreply, State};

handle_event(#wx{id=?wxID_CLOSE}, State) ->
    {stop, normal, State};

handle_event(Help = #wx{id=?wxID_HELP}, State = #state{parent=Parent}) ->
    Parent ! Help,
    {noreply, State};

handle_event(#wx{id=?GOTO_ENTRY, event=#wxCommand{cmdString=Str}},
	     State = #state{grid=Grid}) ->
    try
	Row0 = list_to_integer(Str),
	Row1 = min(0, Row0),
	Row  = max(wxListCtrl:getItemCount(Grid)-1,Row1),
	wxListCtrl:ensureVisible(Grid, Row),
	ok
    catch _:_ -> ok
    end,
    {noreply, State};

%% Search functionality
handle_event(#wx{id=?ID_SEARCH},
	     State = #state{sizer=Sz, search=Search}) ->
    wxSizer:show(Sz, Search#search.win),
    wxWindow:setFocus(Search#search.search),
    wxSizer:layout(Sz),
    {noreply, State};
handle_event(#wx{id=?SEARCH_ENTRY, event=#wxFocus{}},
	     State = #state{search=Search, pid=Pid}) ->
    Pid ! {mark_search_hit, false},
    {noreply, State#state{search=Search#search{find=undefined}}};
handle_event(#wx{id=?SEARCH_ENTRY, event=#wxCommand{cmdString=""}},
	     State = #state{search=Search, pid=Pid}) ->
    Pid ! {mark_search_hit, false},
    {noreply, State#state{search=Search#search{find=undefined}}};
handle_event(#wx{id=?SEARCH_ENTRY, event=#wxCommand{type=command_text_enter,cmdString=Str}},
	     State = #state{grid=Grid, pid=Pid, status=SB,
			    search=Search=#search{radio={Next0, _, Case0},
						  find=Find}})
  when Find =/= undefined ->
    Dir  = wxRadioButton:getValue(Next0) xor wx_misc:getKeyState(?WXK_SHIFT),
    Case = wxCheckBox:getValue(Case0),
    Pos = if Find#find.found, Dir ->  %% Forward Continuation
		  Find#find.start+1;
	     Find#find.found ->  %% Backward Continuation
		  Find#find.start-1;
	     Dir ->   %% Forward wrap
		  0;
	     true ->  %% Backward wrap
		  wxListCtrl:getItemCount(Grid)-1
	  end,
    Pid ! {mark_search_hit, false},
    case search(Pid, Str, Pos, Dir, Case) of
	false ->
	    wxStatusBar:setStatusText(SB, "Not found"),
	    Pid ! {mark_search_hit, Find#find.start},
	    wxListCtrl:refreshItem(Grid, Find#find.start),
	    {noreply, State#state{search=Search#search{find=#find{found=false}}}};
	Row ->
	    wxListCtrl:ensureVisible(Grid, Row),
	    wxListCtrl:refreshItem(Grid, Row),
	    Status = "Found: (Hit Enter for next, Shift-Enter for previous)",
	    wxStatusBar:setStatusText(SB, Status),
	    {noreply, State#state{search=Search#search{find=#find{start=Row, found=true}}}}
    end;
handle_event(#wx{id=?SEARCH_ENTRY, event=#wxCommand{cmdString=Str}},
	     State = #state{grid=Grid, pid=Pid, status=SB,
			    search=Search=#search{radio={Next0, _, Case0},
						  find=Find}}) ->
    try
	Dir  = wxRadioButton:getValue(Next0),
	Case = wxCheckBox:getValue(Case0),
	Start = case Dir of 
		    true -> 0;
		    false -> wxListCtrl:getItemCount(Grid)-1
		end,
	Cont = case Find of
		   undefined ->
		       #find{start=Start, strlen=length(Str)};
		   #find{strlen=Old} when Old < length(Str) ->
		       Find#find{start=Start, strlen=length(Str)};
		   _ ->
		       Find#find{strlen=length(Str)}
	       end,
	
	Pid ! {mark_search_hit, false},
	case search(Pid, Str, Cont#find.start, Dir, Case) of
	    false ->
		wxStatusBar:setStatusText(SB, "Not found"),
		{noreply, State};
	    Row ->
		wxListCtrl:ensureVisible(Grid, Row),
		wxListCtrl:refreshItem(Grid, Row),
		Status = "Found: (Hit Enter for next, Shift-Enter for previous)",
		wxStatusBar:setStatusText(SB, Status),
		{noreply, State#state{search=Search#search{find=#find{start=Row, found=true}}}}
	end
    catch _:_ -> {noreply, State}
    end;

handle_event(Event, State) ->
    io:format("~p:~p, handle event ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

handle_sync_event(Event, _Obj, _State) ->
    io:format("~p:~p, handle sync_event ~p\n", [?MODULE, ?LINE, Event]),
    ok.

handle_call(Event, From, State) ->
    io:format("~p:~p, handle call (~p) ~p\n", [?MODULE, ?LINE, From, Event]),
    {noreply, State}.

handle_cast(Event, State) ->
    io:format("~p:~p, handle cast ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

handle_info({no_rows, N}, State = #state{grid=Grid, status=StatusBar}) ->
    wxListCtrl:setItemCount(Grid, N),
    wxStatusBar:setStatusText(StatusBar, io_lib:format("Objects: ~w",[N])),
    {noreply, State};
handle_info({new_cols, New}, State = #state{grid=Grid, columns=Cols0}) ->
    Cols = add_columns(Grid, Cols0, New),
    {noreply, State#state{columns=Cols}};
handle_info({refresh, Min, Max}, State = #state{grid=Grid}) ->
    wxListCtrl:refreshItems(Grid, Min, Max),
    {noreply, State};
handle_info(Event, State) ->
    io:format("~p:~p, handle info ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

terminate(Event, #state{pid=Pid,
			attrs=#attrs{odd=Odd, deleted=D, changed=Ch, searched=S}}) ->
    %% ListItemAttr are not auto deleted
    wxListItemAttr:destroy(Odd),
    wxListItemAttr:destroy(D),
    wxListItemAttr:destroy(Ch),
    wxListItemAttr:destroy(S),
    unlink(Pid),
    exit(Pid, window_closed),
    io:format("~p:~p, terminate ~p\n", [?MODULE, ?LINE, Event]),
    ok.

code_change(_, _, State) ->
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Table holder needs to be in a separate process otherwise
%%  the callback get_row/3 may deadlock if the process do
%%  wx calls when callback is invoked.
get_row(Table, Item, Column) ->
    Ref = erlang:monitor(process, Table),
    Table ! {get_row, self(), Item, Column},
    receive
	{'DOWN', Ref, _, _, _} -> "";
	{Table, Res} ->
	    erlang:demonitor(Ref),
	    Res
    end.

get_attr(Table, Item) ->
    Ref = erlang:monitor(process, Table),
    Table ! {get_attr, self(), Item},
    receive
	{'DOWN', Ref, _, _, _} -> "";
	{Table, Res} ->
	    erlang:demonitor(Ref),
	    Res
    end.

search(Table, Str, Row, Dir, Case) ->
    Ref = erlang:monitor(process, Table),
    Table ! {search, [Str, Row, Dir, Case]},
    receive
	{'DOWN', Ref, _, _, _} -> "";
	{Table, Res} ->
	    erlang:demonitor(Ref),
	    Res
    end.

-record(holder, {node, parent, pid,
		 table=[], n=0, columns,
		 vis_info,
		 search,
		 source,
		 sort,
		 attrs
		}).

init_table_holder(Parent, Table, MnesiaOrEts, Cols, Node, Attrs) ->
    TabId = case Table#tab.id of
		ignore -> Table#tab.name;
		Id -> Id
	    end,
    Pid = rpc:call(Node, ?MODULE, get_table, [self(), TabId, MnesiaOrEts]),
    table_holder(#holder{node=Node, parent=Parent, pid=Pid,
			 vis_info=array:new(),
			 source=MnesiaOrEts, columns=Cols,
			 attrs=Attrs}).

table_holder(S0 = #holder{parent=Parent, pid=Pid, table=Table}) ->
    receive
	{get_attr, From, Row} ->
	    get_attr(From, Row, S0),
	    table_holder(S0);
	{get_row, From, Row, Col} ->
	    get_row(From, Row, Col, Table),
	    table_holder(S0);
	{Pid, Data} ->
	    S1 = handle_new_data_chunk(Data, S0),
	    table_holder(S1);
	{sort, Col} ->
	    table_holder(sort(Col, S0));
	{search, Data} ->
	    table_holder(search(Data, S0));
	{mark_search_hit, Row} ->
	    Old = S0#holder.search,
	    is_integer(Old) andalso (Parent ! {refresh, Old, Old}),
	    table_holder(S0#holder{search=Row});
	What ->
	    io:format("Table holder got ~p~n",[What]),
	    table_holder(S0)
    end.

handle_new_data_chunk(Data, S0 = #holder{columns=Cols, parent=Parent}) ->
    S1 = #holder{columns=NewCols} = handle_new_data_chunk2(Data, S0),
    case NewCols =:= Cols of
	true -> S1;
	false ->
	    Parent ! {new_cols, lists:seq(Cols+1, NewCols)},
	    S1
    end.

handle_new_data_chunk2('$end_of_table', S0 = #holder{parent=Parent, n=N}) ->
    Parent ! {no_rows, N},
    S0;
handle_new_data_chunk2(Data, S0 = #holder{n=N0, columns=Cols0, source=ets, table=Tab0}) ->
    {Tab, Cols, N} = parse_ets_data(Data, N0, Cols0, Tab0),
    S0#holder{n=N, columns=Cols, table=Tab};
handle_new_data_chunk2(Data, S0 = #holder{n=N0, source=mnesia, table=Tab}) ->
    N = length(Data),
    S0#holder{n=N+N0, table=(lists:append(Data) ++ Tab)}.

parse_ets_data([[Rec]|Rs], N, C, Tab) ->
    parse_ets_data(Rs, N+1, max(tuple_size(Rec), C), [Rec|Tab]);
parse_ets_data([Recs|Rs], N0, C0, Tab0) ->
    {Tab, Cols, N} = parse_ets_data(Recs, N0, C0, Tab0),
    parse_ets_data(Rs, N, Cols, Tab);
parse_ets_data([], N, Cols, Tab) ->
    {Tab, Cols, N}.

sort(Col, S=#holder{n=N, parent=Parent, sort=Opt0, table=Table0}) ->
    {Opt, Table} = sort(Col, Opt0, Table0),
    Parent ! {refresh, 0, N},
    S#holder{sort=Opt, table=Table}.

sort(Col, Opt = #opt{sort_key=Col, sort_incr=Bool}, Table) ->
    {Opt#opt{sort_incr=not Bool}, lists:reverse(Table)};
sort(Col, _, Table) ->
    {#opt{sort_key=Col}, lists:keysort(Col, Table)}.

search([Str, Row, Dir0, CaseSens],
       S=#holder{parent=Parent, table=Table}) ->
    Opt = case CaseSens of
	      true -> [];
	      false -> [caseless]
	  end,
    {ok, Re} = re:compile(Str, Opt),
    Dir = case Dir0 of
	      true -> 1;
	      false -> -1
	  end,
    Res = search(Row, Dir, Re, Table),
    Parent ! {self(), Res},
    S#holder{search=Res}.

search(Row, Dir, Re, Table) ->
    Res = try lists:nth(Row+1, Table) of
	      Term ->
		  Str = io_lib:format("~w", [Term]),
		  re:run(Str, Re)
	  catch _:_ -> no_more
	  end,
    case Res of
	nomatch -> search(Row+Dir, Dir, Re, Table);
	no_more -> false;
	{match,_} -> Row
    end.

get_row(From, Row, Col, Table) ->
    case lists:nth(Row+1, Table) of
	Object when Col =:= all ->
	    From ! {self(), io_lib:format("~w", [Object])};
	Object when tuple_size(Object) >= Col ->
	    From ! {self(), io_lib:format("~w", [element(Col, Object)])};
	_ ->
	    From ! {self(), ""}
    end.

get_attr(From, Row, #holder{attrs=Attrs, search=Row}) ->
    What = Attrs#attrs.searched,
    From ! {self(), What};
get_attr(From, Row, #holder{vis_info=Table, attrs=Attrs}) ->
    What = case array:get(Row, Table) of
	       deleted  -> Attrs#attrs.deleted;
	       changed  -> Attrs#attrs.changed;
	       undefined when (Row rem 2) > 0 ->
		   Attrs#attrs.odd;
	       undefined ->
		   Attrs#attrs.even
	   end,
    From ! {self(), What}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_table(Parent, Table, Module) ->
    spawn(fun() ->
		  link(Parent),
		  get_table2(Parent, Table, Module)
	  end).

get_table2(Parent, Table, Type) ->
    Size = case Type of
	       ets -> ets:info(Table, size);
	       mnesia -> mnesia:table_info(Table, size)
	   end,
    case Size > 0 of
	false ->
	    Parent ! {self(), '$end_of_table'},
	    normal;
	true when Type =:= ets ->
	    Mem = ets:info(Table, memory),
	    Average = Mem div Size,
	    NoElements = max(10, 20000 div Average),
	    get_ets_loop(Parent, ets:match(Table, '$1', NoElements));
	true ->
	    Mem = mnesia:table_info(Table, memory),
	    Average = Mem div Size,
	    NoElements = max(10, 20000 div Average),
	    Ms = [{'$1', [], ['$1']}],
	    Get = fun() ->
			  get_mnesia_loop(Parent, mnesia:select(Table, Ms, NoElements, read))
		  end,
	    mnesia:transaction(Get)
    end.

get_ets_loop(Parent, '$end_of_table') ->
    Parent ! {self(), '$end_of_table'};
get_ets_loop(Parent, {Match, Cont}) ->
    Parent ! {self(), Match},
    get_ets_loop(Parent, ets:match(Cont)).

get_mnesia_loop(Parent, '$end_of_table') ->
    Parent ! {self(), '$end_of_table'};
get_mnesia_loop(Parent, {Match, Cont}) ->
    Parent ! {self(), Match},
    get_ets_loop(Parent, mnesia:select(Cont)).

column_names(Node, Type, Table) ->
    case Type of
	ets -> [1, 2];
	mnesia ->
	    Attrs = rpc:call(Node, mnesia, table_info, [Table, attributes]),
	    is_list(Attrs) orelse throw(node_or_table_down),
	    ["Record Name"|Attrs]
    end.

table_id(#tab{id=ignore, name=Name}) -> Name;
table_id(#tab{id=Id}) -> Id.

key_pos(_, mnesia, _) -> 2;
key_pos(Node, ets, TabId) ->
    KeyPos = rpc:call(Node, ets, info, [TabId, keypos]),
    is_integer(KeyPos) orelse throw(node_or_table_down),
    KeyPos.

create_attrs() ->
    Font = wxSystemSettings:getFont(?wxSYS_DEFAULT_GUI_FONT),
    Text = wxSystemSettings:getColour(?wxSYS_COLOUR_LISTBOXTEXT),
    #attrs{even = wx:typeCast(wx:null(), wxListItemAttr),
	   odd  = wxListItemAttr:new(Text, {240,240,255}, Font),
	   deleted = wxListItemAttr:new({240,30,30}, {10,10,10}, Font),
	   changed = wxListItemAttr:new(Text, {255,215,0}, Font),
	   searched = wxListItemAttr:new(Text, {235,215,90}, Font)
	  }.
