-module(observer_wx).

-behaviour(wx_object).

-export([start/0]).
-export([init/1, handle_event/2, handle_cast/2, terminate/2, code_change/3, 
	 handle_call/3, handle_info/2, check_page_title/1]).

%% Includes
-include_lib("wx/include/wx.hrl").

%% Defines
-define(OBS_SYS_WX, observer_sys_wx).
-define(OBS_PRO_WX, observer_pro_wx).


-define(ID_PING, 1).
-define(ID_CONNECT, 2).

-define(FIRST_NODES_MENU_ID, 1000).
-define(LAST_NODES_MENU_ID,  2000).

%% Records
-record(state,
	{	  frame,
		  menubar,
		  node_menu,
		  file_menu,
		  help_menu,
		  status_bar,
		  notebook,
		  main_panel,
		  app_panel,
		  pro_panel,
		  tv_panel,
		  sys_panel,
		  node,
		  nodes
	}).

-record(create_menu, 
	{id,
	 text,
	 type = append,
	 check = true
	}).

start() ->
    wx_object:start(?MODULE, [], []).

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
    Env = wx:get_env(),

    %% Setup Menubar & Menus
    MenuBar = wxMenuBar:new(),

    FileMenu = create_menu([#create_menu{id = ?wxID_EXIT, text = "&Quit"}], "File", MenuBar),
    HelpMenu = create_menu([#create_menu{id = ?wxID_HELP, text = "&Help"}], "Help", MenuBar),
    {Nodes, NodesMenuItems} = get_nodes(),
    
    NodeMenu = case erlang:is_alive() of
		   true ->
		       create_menu([#create_menu{id = ?ID_PING, text = "&Ping node"} | NodesMenuItems], "Nodes", MenuBar);
		   false ->
		       create_menu([#create_menu{id = ?ID_CONNECT, text = "&Connect"} | NodesMenuItems], "Nodes", MenuBar)
	       end,
    


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
    SysPanel = ?OBS_SYS_WX:start_link(Notebook, MenuBar),
    wxNotebook:addPage(Notebook, SysPanel, "System", []),
    
    %% Process Panel
    ProPanel = ?OBS_PRO_WX:start_link(Notebook, Env, MenuBar, StatusBar),
    wxNotebook:addPage(Notebook, ProPanel, "Processes", []),
   
    %% Application Panel
    %AppPanel = wxPanel:new(Notebook, []),
    %wxNotebook:addPage(Notebook, AppPanel, "Applications", []),

    %% Table Viewer Panel
    TVPanel = observer_tv_wx:start_link(Notebook, MenuBar, StatusBar),
    wxNotebook:addPage(Notebook, TVPanel, "Table Viewer", []),
    
    wxSizer:add(MainSizer, Notebook, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, MainSizer),
    
    wxNotebook:connect(Notebook, command_notebook_page_changed),
    wxFrame:connect(Frame, close_window, [{skip, true}]),
    wxMenu:connect(Frame, command_menu_selected, [{skip, true}]),
          
    UpdState = State#state{main_panel = Panel,
			   notebook = Notebook,
			   menubar = MenuBar,
			   node_menu = NodeMenu,
			   file_menu = FileMenu,
			   help_menu = HelpMenu,
			   status_bar = StatusBar,
			   sys_panel = SysPanel,
			   pro_panel = ProPanel,
			   tv_panel  = TVPanel,
%%			   app_panel = AppPanel,
			   nodes = Nodes
			  },
    UpdState.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Callbacks
handle_event(#wx{obj = Notebook, event = #wxNotebook{type = command_notebook_page_changed}} = Event, State) ->
    io:format("~p~n", [node()]),
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
    Pid ! Event,
    {noreply, State};

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

create_menu(MenuItems, Name, MenuBar) ->
    Menu = wxMenu:new(),
    lists:foreach(fun(Record) ->
			  create_menu_item(Record, Menu)
		  end,
		  MenuItems),
    wxMenuBar:append(MenuBar, Menu, Name),
    Menu.

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

update_node_list(State) ->
    {Nodes, NodesMenuItems} = get_nodes(),
    lists:foreach(fun(Item) ->
			  wxMenu:delete(State#state.node_menu, Item)
		  end,
		  wxMenu:getMenuItems(State#state.node_menu)),
    
    case erlang:is_alive() of
	true ->
	    create_menu_item(#create_menu{id = ?ID_PING, text = "&Ping node"}, State#state.node_menu);
	false ->
	    create_menu_item(#create_menu{id = ?ID_CONNECT, text = "&Connect"}, State#state.node_menu)
    end,
    
    lists:foreach(fun(Record) ->
			  create_menu_item(Record, State#state.node_menu)
		  end,
		  NodesMenuItems),
    State#state{nodes = Nodes}.











