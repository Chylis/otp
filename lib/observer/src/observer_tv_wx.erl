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
-define(GRID, 500).

-record(tab, {name,
	      id,
	      size,
	      readable,
	      owner,
	      reg_name}).

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
	  opt=#opt{}
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
		 {"Owner Pid",  ?wxLIST_FORMAT_CENTER, 100},
		 {"Owner Name", ?wxLIST_FORMAT_LEFT,  200}
		],
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),

    wxListCtrl:connect(Grid, command_list_item_activated),
    %% wxListCtrl:connect(Grid, command_list_item_selected),
    wxListCtrl:connect(Grid, command_list_col_click),
    %% wxListCtrl:connect(Grid, size, [{skip, true}]),
    %% wxListCtrl:connect(Grid, key_up, [{id, ?GRID}, {skip,true}]),

    %% wxWindow:connect(Panel, enter_window, [{skip,true}]),
    wxWindow:setFocus(Grid),
    {Panel, #state{grid=Grid, parent=Parent}}.

handle_event(#wx{event=#wxList{type=command_list_col_click, col=Col}},
	     State = #state{node=Node, grid=Grid,
			    opt=Opt0=#opt{sort_key=Key, sort_incr=Bool}}) ->
    Opt = case Col+2 of
	      Key -> Opt0#opt{sort_incr=not Bool};
	      NewKey -> Opt0#opt{sort_key=NewKey}
	  end,
    Tables = get_tables(Node, Opt),
    update_grid(Grid, Opt, Tables),
    wxWindow:setFocus(Grid),
    {noreply, State#state{opt=Opt}};

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

handle_info({active, Node}, State = #state{grid=Grid, opt=Opt}) ->
    Tables = get_tables(Node, Opt),
    update_grid(Grid, Opt, Tables),
    wxWindow:setFocus(Grid),
    {noreply, State#state{node=Node}};

handle_info(Event, State) ->
    io:format("~p:~p, handle info ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

terminate(Event, _State) ->
    io:format("~p:~p, terminate ~p\n", [?MODULE, ?LINE, Event]),
    ok.

code_change(_, _, State) ->
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_tables(Node, Opt) ->
    case rpc:call(Node, ?MODULE, get_table_list, [Opt]) of
	{badrpc, Error} ->
	    handle_error(Error);
	Result -> Result
    end.

get_table_list(#opt{type=ets}) ->
    Info = fun(Id, Acc) ->
		   try
		       TabId = case ets:info(Id, named_table) of
				   true -> ignore;
				   false -> Id
			       end,
		       Readable = ets:info(Id, protection) /= private,
		       Owner = ets:info(Id, owner),
		       RegName = case catch process_info(Owner, registered_name) of
				     [] -> ignore;
				     {registered_name, ProcName} -> ProcName
				 end,
		       Tab = #tab{name = ets:info(Id, name),
				  id = TabId,
				  readable = Readable,
				  owner = Owner,
				  size = ets:info(Id, size),
				  reg_name = RegName},
		       [Tab|Acc]
		   catch _:What ->
			   io:format("~p: ~p ~n",[Id, What]),
			   Acc
		   end
	   end,
    lists:foldl(Info, [], ets:all()).


handle_error(Foo) ->
   io:format("ERROR: ~p~n",[Foo]).


update_grid(Grid, #opt{sort_key=Sort,sort_incr=Dir}, Tables) ->
    wxListCtrl:deleteAllItems(Grid),

    Update =
	fun(#tab{name = Name,
		 id = Id,
		 owner = Owner,
		 size = Size,
		 reg_name = RegName}, Row) ->
		wxListCtrl:insertItem(Grid, Row, ""),
		if (Row rem 2) =:= 0 ->
			wxListCtrl:setItemBackgroundColour(Grid, Row, {240,240,255});
		   true -> ignore
		end,
		lists:foreach(fun({_, ignore}) -> ignore;
				 ({Col, Val}) ->
				      wxListCtrl:setItem(Grid, Row, Col, to_str(Val))
			      end,
			      [{0,Name}, {1,Id}, {2,Size}, {3,Owner},{4,RegName}]),
		Row + 1
	end,
    ProcInfo = case Dir of
		   false -> lists:reverse(lists:keysort(Sort, Tables));
		   true -> lists:keysort(Sort, Tables)
	       end,
    wx:foldl(Update, 0, ProcInfo),
    ok.
