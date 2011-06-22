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
-module(observer_wx).

-behaviour(wx_object).

-export([start/0, create_menus/2]).
-export([init/1, handle_event/2, handle_cast/2, terminate/2, code_change/3, 
	 handle_call/3, handle_info/2, check_page_title/1]).

%% Includes
-include_lib("wx/include/wx.hrl").

-include("observer_defs.hrl").

%% Defines

-define(ID_PING, 1).
-define(ID_CONNECT, 2).

-define(FIRST_NODES_MENU_ID, 1000).
-define(LAST_NODES_MENU_ID,  2000).

%% Records
-record(state,
	{frame,
	 menubar,
	 status_bar,
	 notebook,
	 main_panel,
	 app_panel,
	 pro_panel,
	 tv_panel,
	 sys_panel,
	 active_tab,
	 node,
	 nodes
	}).

start() ->
    wx_object:start(?MODULE, [], []).

create_menus(Object, Menus) when is_list(Menus) ->
    wx_object:call(Object, {create_menus, Menus}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Observer", [{size, {1000, 500}}]),
    State = #state{frame = Frame},
    UpdState = setup(State),
    wxFrame:show(Frame),
    net_kernel:monitor_nodes(true),
    {Frame, UpdState}.

setup(#state{frame = Frame} = State) ->
    %% Setup Menubar & Menus
    MenuBar = wxMenuBar:new(),

    {Nodes, NodeMenus} = get_nodes(),
    DefMenus = default_menus(NodeMenus),
    create_menu(DefMenus, MenuBar),

    wxFrame:setMenuBar(Frame, MenuBar),
    StatusBar = wxFrame:createStatusBar(Frame, []),
    wxFrame:setTitle(Frame, atom_to_list(node())),
    wxStatusBar:setStatusText(StatusBar, atom_to_list(node())),
    
    %% Setup panels
    Panel = wxPanel:new(Frame, []),
    Notebook = wxNotebook:new(Panel, 1, [{style, ?wxBK_DEFAULT}]),
  
    %% Setup sizer
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    
    %% System Panel
    SysPanel = ?OBS_SYS_WX:start_link(Notebook, self()),
    wxNotebook:addPage(Notebook, SysPanel, "System", []),
    
    %% Process Panel
    ProPanel = ?OBS_PRO_WX:start_link(Notebook, self()),
    wxNotebook:addPage(Notebook, ProPanel, "Processes", []),
   
    %% Application Panel
    %AppPanel = wxPanel:new(Notebook, []),
    %wxNotebook:addPage(Notebook, AppPanel, "Applications", []),

    %% Table Viewer Panel
    TVPanel = observer_tv_wx:start_link(Notebook, self()),
    wxNotebook:addPage(Notebook, TVPanel, "Table Viewer", []),
    
    wxSizer:add(MainSizer, Notebook, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, MainSizer),
    
    wxNotebook:connect(Notebook, command_notebook_page_changed),
    wxFrame:connect(Frame, close_window, [{skip, true}]),
    wxMenu:connect(Frame, command_menu_selected, [{skip, true}]),

    SysPid = wx_object:get_pid(SysPanel),
    SysPid ! {active, node()},
    UpdState = State#state{main_panel = Panel,
			   notebook = Notebook,
			   menubar = MenuBar,
			   status_bar = StatusBar,
			   sys_panel = SysPanel,
			   pro_panel = ProPanel,
			   tv_panel  = TVPanel,
%%			   app_panel = AppPanel,
			   active_tab = SysPid,
			   node  = node(),
			   nodes = Nodes
			  },
    UpdState.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Callbacks
handle_event(#wx{obj = Notebook, event = #wxNotebook{type = command_notebook_page_changed}},
	     #state{active_tab=Previous, node=Node, notebook = Notebook} = State) ->
    Pid = case check_page_title(Notebook) of
	      "Processes" ->
		  wx_object:get_pid(State#state.pro_panel);
	      "Applications" ->
		  wx_object:get_pid(State#state.app_panel);
	      "System" ->
		  wx_object:get_pid(State#state.sys_panel);
	      "Table Viewer" ->
		  wx_object:get_pid(State#state.tv_panel);
	      _Other ->
		  ok
	  end,
    Previous ! not_active,
    Pid ! {active, Node},
    {noreply, State#state{active_tab=Pid}};

handle_event(#wx{event = #wxClose{}}, State) ->
    {stop, normal, State};

handle_event(#wx{id = ?wxID_EXIT, event = #wxCommand{type = command_menu_selected}}, State) ->
    io:format("~p ~p, you clicked close", [?MODULE, ?LINE]),
    wxWindow:destroy(State#state.frame),
    {stop, normal, State};

handle_event(#wx{id = ?wxID_HELP, event = #wxCommand{type = command_menu_selected}}, State) ->
    io:format("~p ~p, you clicked help", [?MODULE, ?LINE]),
    {noreply, State};

handle_event(#wx{id = ?ID_CONNECT, event = #wxCommand{type = command_menu_selected}}, State) ->
    UpdState = case create_connect_dialog(connect, State) of
		   cancel ->
		       State;
		   {value, []} ->
		       State;
		   {value, Value} when is_list(Value) -> %% felhantering, t.ex start går ej, om man rdan connectat, samt välja name/sname, cookie, hidden?
		       net_kernel:start([erlang:list_to_atom(Value)]),
		       io:format("connected. nodename: ~p~n", [node()]),
		       change_node_view(node(), State)
	       end,
    {noreply, UpdState};

handle_event(#wx{id = ?ID_PING, event = #wxCommand{type = command_menu_selected}}, State) ->

    UpdState = case create_connect_dialog(ping, State) of
		   cancel ->
		       State;
		   {value, Value} when is_list(Value) ->
		       Node = list_to_atom(Value),
		       case net_adm:ping(Node) of
			   pang ->
			       create_popup_dialog("Connect failed", "Pang", State),
			       State;
			   pong ->
			       change_node_view(Node, State)
		       end
	       end,
    {noreply, UpdState};

handle_event(#wx{id = Id, event = #wxCommand{type = command_menu_selected}}, State)
  when Id > ?FIRST_NODES_MENU_ID, Id < ?LAST_NODES_MENU_ID ->
    
    Node = lists:nth(Id - ?FIRST_NODES_MENU_ID, State#state.nodes),
    io:format("~p:~p, Klickade på nod ~p~n", [?MODULE, ?LINE, Node]),
    UpdState = change_node_view(Node, State),
    {noreply, UpdState};

handle_event(Event, State) ->
    Pid = case check_page_title(State#state.notebook) of
	      "Processes" ->
		  wx_object:get_pid(State#state.pro_panel);
	      "Applications" ->
		  wx_object:get_pid(State#state.app_panel);
	      "System" ->
		  wx_object:get_pid(State#state.sys_panel);
	      _Other ->
		  ok
	  end,
    Pid ! Event,
    {noreply, State}.
    
handle_cast(Cast, State) ->
    io:format("~p:~p: Got cast ~p~n", [?MODULE, ?LINE, Cast]),
    {noreply, State}.

handle_call({create_menus, TabMenus}, _From, State = #state{menubar=MenuBar}) ->
    {_Nodes, NodeMenus} = get_nodes(),
    DefMenus = default_menus(NodeMenus),
    Menus = merge_menus(DefMenus, TabMenus),
    clean_menus(MenuBar),
    create_menu(Menus, MenuBar),
    {reply, ok, State};

handle_call(Msg, _From, State) ->
    io:format("~p~p: Got Call ~p~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.

handle_info({nodeup, _Node}, State) ->
    State2 = update_node_list(State),
    {noreply, State2};

handle_info({nodedown, Node}, State) ->
    State2 = case Node =:= State#state.node of
		 true ->
		     change_node_view(node(), State);
		 false ->
		     State
	     end,
    State3 = update_node_list(State2),
    Msg = ["Node down: " | atom_to_list(Node)],
    create_popup_dialog(Msg, "Node down", State3),
    {noreply, State3};

handle_info(Info, State) ->
    io:format("~p, ~p, Handle info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p terminating. Reason: ~p~n", [?MODULE, Reason]),
    ok.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_node_view(Node, #state{} = State) ->
    Pids = [wx_object:get_pid(State#state.pro_panel), wx_object:get_pid(State#state.sys_panel)],
    lists:foreach(fun(Pid) -> Pid ! {node, Node} end, 
		  Pids),
    StatusText = ["Observer - " | atom_to_list(Node)],
    wxFrame:setTitle(State#state.frame, StatusText),
    wxStatusBar:setStatusText(State#state.status_bar, StatusText),
    State#state{node = Node}.

check_page_title(Notebook) ->
    Selection = wxNotebook:getSelection(Notebook),
    wxNotebook:getPageText(Notebook, Selection).

create_connect_dialog(ping, #state{frame = Frame}) ->
    Dialog = wxTextEntryDialog:new(Frame, "Connect to node"),
    case wxDialog:showModal(Dialog) of
	?wxID_OK ->
	    {value, wxTextEntryDialog:getValue(Dialog)};
	?wxID_CANCEL ->
	    cancel
    end;
create_connect_dialog(connect, #state{frame = Frame}) ->
    Dialog = wxTextEntryDialog:new(Frame, "Node name: "),
    case wxDialog:showModal(Dialog) of
	?wxID_OK ->
	    {value, wxTextEntryDialog:getValue(Dialog)};
	?wxID_CANCEL ->
	    cancel
    end.

create_popup_dialog(Msg, Title, #state{frame = Frame}) ->
    Popup = wxMessageDialog:new(Frame, Msg),
    wxMessageDialog:setTitle(Popup, Title),
    wxDialog:showModal(Popup).

default_menus(NodesMenuItems) ->
    FileMenu = {"File", [#create_menu{id = ?wxID_EXIT, text = "&Quit"}]},
    HelpMenu = {"Help", [#create_menu{id = ?wxID_HELP, text = "&Help"}]},
    NodeMenu = case erlang:is_alive() of
		   true ->
		       {"Nodes", NodesMenuItems ++
			    [#create_menu{id = ?ID_PING, text = "&Ping node"}]};
		   false ->
		       {"Nodes", NodesMenuItems ++
			    [#create_menu{id = ?ID_CONNECT, text = "&Connect"}]}
	       end,
    [FileMenu, NodeMenu, HelpMenu].

clean_menus(MenuBar) ->
    io:format("Remove menus..."),
    Count = wxMenuBar:getMenuCount(MenuBar),
    remove_menu_item(MenuBar, Count),
    io:format("done~n").

remove_menu_item(MenuBar, Item) when Item > -1 ->
    Menu = wxMenuBar:getMenu(MenuBar, Item),
    wxMenuBar:remove(MenuBar, Item),
    wxMenu:destroy(Menu),
    remove_menu_item(MenuBar, Item-1);
remove_menu_item(_, _) ->
    ok.

merge_menus([{Label, Items}|Default], [{Label, TabItems}|TabMenus]) ->
    [{Label, TabItems ++ Items} | merge_menus(Default, TabMenus)];
merge_menus([Menu = {"File", _}|Default], TabMenus) ->
    [Menu | merge_menus(Default, TabMenus)];
merge_menus(Default = [{"Nodes", _}|_], TabMenus) ->
    TabMenus ++ Default.

create_menu(Menus, MenuBar) ->
    io:format("Menus ~p~n",[Menus]),
    Add = fun({Name, MenuItems}) ->
		  Menu = wxMenu:new(),
		  lists:foreach(fun(Record) ->
					create_menu_item(Record, Menu)
				end,
				MenuItems),
		  wxMenuBar:append(MenuBar, Menu, Name)
	  end,
    wx:foreach(Add, Menus),
    ok.

create_menu_item(#create_menu{id = Id, text = Text, type = Type, check = Check}, Menu) ->
    case Type of
	append ->
	    wxMenu:append(Menu, Id, Text);
	appendCheck ->
	    wxMenu:appendCheckItem(Menu, Id, Text),
	    wxMenu:check(Menu, Id, Check);
	separator ->
	    wxMenu:appendSeparator(Menu)
    end.


get_nodes() ->
    Nodes = [node()| nodes()],
    {_, Menues} =
	lists:foldl(fun(Node, {Id, Acc}) when Id < ?LAST_NODES_MENU_ID ->
			    {Id + 1, [#create_menu{id = Id + ?FIRST_NODES_MENU_ID, 
						   text =  atom_to_list(Node)} | Acc]} 
		    end,
		    {1, []},
		    Nodes),
    {Nodes, lists:reverse(Menues)}.

update_node_list(State = #state{menubar=MenuBar}) ->
    {Nodes, NodesMenuItems} = get_nodes(),
    NodeMenuId = wxMenuBar:findMenu(MenuBar, "Nodes"),
    NodeMenu = wxMenuBar:getMenu(MenuBar, NodeMenuId),
    wx:foreach(fun(Item) ->
		       wxMenu:'Destroy'(NodeMenu, Item)
	       end,
	       wxMenu:getMenuItems(NodeMenu)),
    
    case erlang:is_alive() of
	true ->
	    create_menu_item(#create_menu{id = ?ID_PING, text = "&Ping node"},
			     NodeMenu);
	false ->
	    create_menu_item(#create_menu{id = ?ID_CONNECT, text = "&Connect"},
			     NodeMenu)
    end,
    
    wx:foreach(fun(Record) ->
		       create_menu_item(Record, NodeMenu)
	       end, NodesMenuItems),
    State#state{nodes = Nodes}.











