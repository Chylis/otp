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
-module(observer_tv_wx).

-export([start_link/2]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-compile(export_all).

-import(observer_pro_wx, [to_str/1]).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").
-include("observer_tv.hrl").

-define(GRID, 500).
-define(ID_REFRESH, 401).
-define(ID_ETS, 402).
-define(ID_MNESIA, 403).
-define(ID_UNREADABLE, 404).
-define(ID_SYSTEM_TABLES, 405).

-define(TC(Cmd), tc(fun() -> Cmd end, ?MODULE, ?LINE)).

-record(opt, {type=ets,
	      sys_hidden=true,
	      unread_hidden=true,
	      sort_key=2,
	      sort_incr=true
	     }).

-record(state,
	{
	  parent,
	  grid,
	  node=node(),
	  opt=#opt{},
	  tabs
	}).

start_link(Notebook,  Parent) ->
    wx_object:start_link(?MODULE, [Notebook, Parent], []).

init([Notebook, Parent]) ->
    Panel = wxPanel:new(Notebook),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    Style = ?wxLC_REPORT bor ?wxLC_SINGLE_SEL bor ?wxLC_HRULES,
    Grid = wxListCtrl:new(Panel, [{winid, ?GRID}, {style, Style}]),
    wxSizer:add(Sizer, Grid, [{flag, ?wxEXPAND bor ?wxALL},
			      {proportion, 1}, {border, 5}]),
    wxWindow:setSizer(Panel, Sizer),
    Li = wxListItem:new(),
    AddListEntry = fun({Name, Align, DefSize}, Col) ->
			   wxListItem:setText(Li, Name),
			   wxListItem:setAlign(Li, Align),
			   wxListCtrl:insertColumn(Grid, Col, Li),
			   wxListCtrl:setColumnWidth(Grid, Col, DefSize),
			   Col + 1
		   end,
    ListItems = [{"Table Name", ?wxLIST_FORMAT_LEFT,  200},
		 {"Table Id",   ?wxLIST_FORMAT_RIGHT, 100},
		 {"Table Size", ?wxLIST_FORMAT_RIGHT, 100},
		 {"Owner Pid",  ?wxLIST_FORMAT_CENTER, 150},
		 {"Owner Name", ?wxLIST_FORMAT_LEFT,  200}
		],
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),

    wxListCtrl:connect(Grid, command_list_item_activated),
    wxListCtrl:connect(Grid, command_list_col_click),
    wxListCtrl:connect(Grid, size, [{skip, true}]),

    wxWindow:setFocus(Grid),
    {Panel, #state{grid=Grid, parent=Parent}}.

handle_event(#wx{id=?ID_REFRESH},
	     State = #state{node=Node, grid=Grid, opt=Opt}) ->
    Tables = get_tables(Node, Opt),
    Tabs = update_grid(Grid, Opt, Tables),
    {noreply, State#state{tabs=Tabs}};

handle_event(#wx{event=#wxList{type=command_list_col_click, col=Col}},
	     State = #state{node=Node, grid=Grid,
			    opt=Opt0=#opt{sort_key=Key, sort_incr=Bool}}) ->
    Opt = case Col+2 of
	      Key -> Opt0#opt{sort_incr=not Bool};
	      NewKey -> Opt0#opt{sort_key=NewKey}
	  end,
    Tables = ?TC(get_tables(Node, Opt)),
    Tabs = ?TC(update_grid(Grid, Opt, Tables)),
    wxWindow:setFocus(Grid),
    {noreply, State#state{opt=Opt, tabs=Tabs}};

handle_event(#wx{id=Id}, State = #state{node=Node, grid=Grid, opt=Opt0})
  when Id >= ?ID_ETS, Id =< ?ID_SYSTEM_TABLES ->
    Opt = case Id of
	      ?ID_ETS -> Opt0#opt{type=ets};
	      ?ID_MNESIA -> Opt0#opt{type=mnesia};
	      ?ID_UNREADABLE -> Opt0#opt{unread_hidden= not Opt0#opt.unread_hidden};
	      ?ID_SYSTEM_TABLES -> Opt0#opt{sys_hidden= not Opt0#opt.sys_hidden}
	  end,
    Tables = get_tables(Node, Opt),
    Tabs = update_grid(Grid, Opt, Tables),
    wxWindow:setFocus(Grid),
    {noreply, State#state{opt=Opt, tabs=Tabs}};

handle_event(#wx{event=#wxSize{size={W,_}}},  State=#state{grid=Grid}) ->
    wx:batch(fun() ->
		     Cols = wxListCtrl:getColumnCount(Grid),
		     Last = lists:foldl(fun(I, Last) ->
						Last - wxListCtrl:getColumnWidth(Grid, I)
					end, W-2, lists:seq(0, Cols - 2)),
		     Size = max(200, Last),
		     wxListCtrl:setColumnWidth(Grid, Cols-1, Size)
	     end),
    {noreply, State};

handle_event(#wx{obj=Grid, 
		 event=_Ev=#wxList{type=command_list_item_activated, 
				   itemIndex=Index}},
	     State=#state{grid=Grid, node=Node, opt=#opt{type=Type}, tabs=Tabs}) ->
    %% TableName = wxListCtrl:getItemText(Grid, Index),
    %% io:format("\rSelected: ~p ~p~n ~p => ~p~n",[Index, TableName, _Ev, lists:nth(Index+1, Tabs)]),
    Table = lists:nth(Index+1, Tabs),
    observer_tv_table:start_link(Grid, [{node,Node}, {type,Type}, {table,Table}]),
    {noreply, State};

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

handle_info({active, Node}, State = #state{parent=Parent, grid=Grid, opt=Opt}) ->
    Tables = get_tables(Node, Opt),
    Tabs = update_grid(Grid, Opt, Tables),
    wxWindow:setFocus(Grid),
    create_menus(Parent),
    {noreply, State#state{node=Node, tabs=Tabs}};

handle_info({node, Node}, State = #state{grid=Grid, opt=Opt}) ->
    Tables = get_tables(Node, Opt),
    Tabs = update_grid(Grid, Opt, Tables),
    wxWindow:setFocus(Grid),
    {noreply, State#state{node=Node, tabs=Tabs}};

handle_info(Event, State) ->
    io:format("~p:~p, handle info ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

terminate(Event, _State) ->
    io:format("~p:~p, terminate ~p\n", [?MODULE, ?LINE, Event]),
    ok.

code_change(_, _, State) ->
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_menus(Parent) ->
    MenuEntries = [{"View",
		    [#create_menu{id = ?ID_REFRESH, text = "&Refresh"},
		     separator,
		     #create_menu{id = ?ID_ETS, text = "&Ets Tables", type=radio, check=true},
		     #create_menu{id = ?ID_MNESIA, text = "&Mnesia Tables", type=radio},
		     separator,
		     #create_menu{id = ?ID_UNREADABLE, text = "View &Unreadable Tables", type=check},
		     #create_menu{id = ?ID_SYSTEM_TABLES, text = "View &System Tables", type=check}]}],
    observer_wx:create_menus(Parent, MenuEntries).

get_tables(Node, Opt) ->
    case rpc:call(Node, ?MODULE, get_table_list, [Opt]) of
	{badrpc, Error} ->
	    handle_error(Error);
	Result -> Result
    end.

get_table_list(#opt{type=ets, unread_hidden=HideUnread, sys_hidden=HideSys}) ->
    Info = fun(Id, Acc) ->
		   try
		       TabId = case ets:info(Id, named_table) of
				   true -> ignore;
				   false -> Id
			       end,
		       Name = ets:info(Id, name),
		       Readable = ets:info(Id, protection) /= private,
		       ignore(HideUnread andalso (not Readable), unreadable),
		       Owner = ets:info(Id, owner),
		       RegName = case catch process_info(Owner, registered_name) of
				     [] -> ignore;
				     {registered_name, ProcName} -> ProcName
				 end,
		       ignore(HideSys andalso ordsets:is_element(RegName, sys_processes()), system_tab),
		       ignore(HideSys andalso ordsets:is_element(Name, sys_tables()), system_tab),
		       ignore((RegName == mnesia_monitor)
			      andalso Name /= schema
			      andalso is_atom((catch mnesia:table_info(Name, where_to_read))), mnesia_tab),
		       Tab = #tab{name = Name,
				  id = TabId,
				  readable = Readable,
				  owner = Owner,
				  size = ets:info(Id, size),
				  reg_name = RegName},
		       [Tab|Acc]
		   catch _:_What ->
			   %% io:format("Skipped ~p: ~p ~n",[Id, _What]),
			   Acc
		   end
	   end,
    lists:foldl(Info, [], ets:all());
get_table_list(#opt{type=mnesia, sys_hidden=HideSys}) ->
    Info = fun(Id, Acc) ->
		   try
		       Name = Id,
		       ignore(HideSys andalso ordsets:is_element(Name, mnesia_tables()), system_tab),
		       Owner = ets:info(Id, owner),
		       {registered_name, RegName} = process_info(Owner, registered_name),
		       ignore(Name =:= schema, mnesia_tab),
		       Tab = #tab{name = Name,
				  owner = Owner,
				  size = mnesia:table_info(Id, size),
				  reg_name = RegName},
		       [Tab|Acc]
		   catch _:_What ->
			   %% io:format("Skipped ~p: ~p ~n",[Id, _What]),
			   Acc
		   end
	   end,
    lists:foldl(Info, [], mnesia:system_info(tables)).

sys_tables() ->
    [ac_tab,  asn1,
     cdv_dump_index_table,  cdv_menu_table,  cdv_decode_heap_table,
     cell_id,  cell_pos,  clist,
     cover_internal_data_table,   cover_collected_remote_data_table, cover_binary_code_table,
     code, code_names,  cookies,
     corba_policy,  corba_policy_associations,
     dets, dets_owners, dets_registry,
     disk_log_names, disk_log_pids,
     eprof,  erl_atom_cache, erl_epmd_nodes,
     etop_accum_tab,  etop_tr,
     ets_coverage_data,
     file_io_servers,
     gs_mapping, gs_names,  gstk_db,
     gstk_grid_cellid, gstk_grid_cellpos, gstk_grid_id,
     httpd,
     id,
     ign_req_index, ign_requests,
     index,
     inet_cache, inet_db, inet_hosts,
     'InitialReferences',
     int_db,
     interpreter_includedirs_macros,
     ir_WstringDef,
     lmcounter,  locks,
%     mnesia_decision,
     mnesia_gvar, mnesia_stats,
%     mnesia_transient_decision,
     pg2_table,
     queue,
     schema,
     shell_records,
     snmp_agent_table, snmp_local_db2, snmp_mib_data, snmp_note_store, snmp_symbolic_ets,
     tkFun, tkLink, tkPriv,
     ttb, ttb_history_table,
     udp_fds, udp_pids
    ].

sys_processes() ->
    [auth, code_server, global_name_server, inet_db,
     mnesia_recover, net_kernel, timer_server, wxe_master].

mnesia_tables() ->
    [ir_AliasDef, ir_ArrayDef, ir_AttributeDef, ir_ConstantDef,
     ir_Contained, ir_Container, ir_EnumDef, ir_ExceptionDef,
     ir_IDLType, ir_IRObject, ir_InterfaceDef, ir_ModuleDef,
     ir_ORB, ir_OperationDef, ir_PrimitiveDef, ir_Repository,
     ir_SequenceDef, ir_StringDef, ir_StructDef, ir_TypedefDef,
     ir_UnionDef, logTable, logTransferTable, mesh_meas,
     mesh_type, mnesia_clist, orber_CosNaming,
     orber_objkeys, user
    ].

handle_error(Foo) ->
    io:format("ERROR: ~p~n",[Foo]),
    [].

update_grid(Grid, Opt, Tables) ->
    wx:batch(fun() -> update_grid2(Grid, Opt, Tables) end).
update_grid2(Grid, #opt{sort_key=Sort,sort_incr=Dir}, Tables) ->
    wxListCtrl:deleteAllItems(Grid),
    Update =
	fun(#tab{name = Name, id = Id, owner = Owner, size = Size,
		 readable = Readable, reg_name = RegName}, Row) ->
		_Item = wxListCtrl:insertItem(Grid, Row, ""),
		if (Row rem 2) =:= 0 ->
			wxListCtrl:setItemBackgroundColour(Grid, Row, {240,240,255});
		   true -> ignore
		end,
		if not Readable ->
			wxListCtrl:setItemTextColour(Grid, Row, {200,130,50});
		   true -> ignore
		end,

		lists:foreach(fun({_, ignore}) -> ignore;
				 ({Col, Val}) ->
				      wxListCtrl:setItem(Grid, Row, Col, to_str(Val))
			      end,
			      [{0,Name}, {1,Id}, {2,Size}, {3,Owner},{4,RegName}]),
		%% wxListCtrl:setItemData(Grid, Item, Item),
		Row + 1
	end,
    ProcInfo = case Dir of
		   false -> lists:reverse(lists:keysort(Sort, Tables));
		   true -> lists:keysort(Sort, Tables)
	       end,
    lists:foldl(Update, 0, ProcInfo),
    ProcInfo.

ignore(true, Reason) -> throw(Reason);
ignore(_,_ ) -> ok.

tc(Fun, Mod, Line) ->
    {Time, Res} = timer:tc(Fun),
    io:format("~p:~p: ~wus~n",[Mod,Line, Time]),
    Res.
