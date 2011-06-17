-module(observer_wx).

-behaviour(wx_object).

-export([start/0]).
-export([init/1, handle_event/2, handle_cast/2, terminate/2, code_change/3, 
	 handle_call/3, handle_info/2, check_page_title/1, get_pro_panel/0]).

%% Includes
-include_lib("wx/include/wx.hrl").

%% Defines
-define(OBS_SYS_WX, observer_sys_wx).
-define(OBS_PRO_WX, observer_pro_wx).

-define(ID_REFRESH, 1).
-define(ID_FILE, 2).
-define(ID_DUMP_TO_FILE, 4).
-define(ID_QUIT, 5).
-define(ID_OPTIONS, 9).
-define(ID_SAVE_OPT, 10).

-define(FIRST_NODES_MENU_ID, 1000).
-define(LAST_NODES_MENU_ID,  2000).

%% Records
-record(state,
	{	  frame,
		  menu_bar,
		  node_menu,
		  file_menu,
		  view_menu,
		  status_bar,
		  notebook,
		  main_panel,
		  app_panel,
		  pro_panel,
		  sys_panel,
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

    FileMenu = create_menu([#create_menu{id = ?ID_FILE, text = "Opt1"}, 
			    #create_menu{id = ?wxID_ANY, text = "&Opt2"}], "File", MenuBar),
    ViewMenu = create_menu([#create_menu{id = ?ID_REFRESH, text = "&Refresh"}, 
			    #create_menu{id = ?wxID_ANY, text = "Opt2"}], "View", MenuBar), %View
   
    {Nodes, NodesMenuItems} = get_nodes(),
    NodeMenu = create_menu(NodesMenuItems, "Nodes", MenuBar),
    
    wxFrame:setMenuBar(Frame, MenuBar),

    %% Setup Statusbar
    StatusBar = wxFrame:createStatusBar(Frame, []),
    
    %% Setup panels
    Panel = wxPanel:new(Frame, []),
    Notebook = wxNotebook:new(Panel, 1, [{style, ?wxBK_DEFAULT}]),
  
    %% Setup sizer
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    
    %% System Panel
    SysPanel = ?OBS_SYS_WX:start_link(Notebook, Env),
    wxNotebook:addPage(Notebook, SysPanel, "System", []),
    
    %% Process Panel
    ProPanel = ?OBS_PRO_WX:start_link(Notebook, Env, MenuBar, StatusBar),
    wxNotebook:addPage(Notebook, ProPanel, "Processes", []),
   
    %% Application Panel
    AppPanel = wxPanel:new(Notebook, []),
    wxNotebook:addPage(Notebook, AppPanel, "Applications", []),
    
    wxSizer:add(MainSizer, Notebook, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, MainSizer),
    
    wxNotebook:connect(Notebook, command_notebook_page_changed),
    wxFrame:connect(Frame, close_window, [{skip, true}]),
    wxMenu:connect(Frame, command_menu_selected, []),
          
    UpdState = State#state{main_panel = Panel,
			   notebook = Notebook,
			   menu_bar = MenuBar,
			   node_menu = NodeMenu,
			   file_menu = FileMenu,
			   view_menu = ViewMenu,
			   status_bar = StatusBar,
			   sys_panel = SysPanel,
			   pro_panel = ProPanel,
			   app_panel = AppPanel,
			   nodes = Nodes
			  },
    UpdState.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Callbacks
handle_event(#wx{obj = Notebook, event = #wxNotebook{type = command_notebook_page_changed}}, State) ->
    Selection = wxNotebook:getSelection(Notebook),
    Title = wxNotebook:getPageText(Notebook, Selection),
    case Title of
	"Processes" ->
	    create_pro_menu(State#state.menu_bar);
	"Applications" ->
	    ok;
	"System" ->
	    ok
    end,
    {noreply, State};

handle_event(#wx{event = #wxClose{}}, State) ->
    {stop, normal, State};

handle_event(#wx{id = ?ID_SAVE_OPT, 
		 event = #wxCommand{type = command_menu_selected}}, State) -> 
    ?OBS_PRO_WX:pro_save_options(),
    {noreply, State};

handle_event(#wx{id = ?ID_DUMP_TO_FILE}, State) -> %% PRO MENU EVENT
    ?OBS_PRO_WX:pro_dump_to_file(),
    {noreply, State};

handle_event(#wx{id = ?ID_OPTIONS}, State) ->% #pro_wx_state{trace_options = Options} = State}) -> %% PRO MENU EVENT
    ?OBS_PRO_WX:pro_view_trace_options(),
    {noreply, State};

handle_event(#wx{id = ?ID_REFRESH, event = #wxCommand{type = command_menu_selected}}, State) ->
    io:format("Klickade på view~n"),
    case check_page_title(State#state.notebook) of
	"Processes" ->
	    ?OBS_PRO_WX:pro_refresh(),
	    io:format("Did pro_refresh()");
	_Else ->
	    ok% do something depending on which page
    end,
    {noreply, State};


handle_event(#wx{id = Id, 
		 event = #wxCommand{type = command_menu_selected}}, State)
  when Id > ?FIRST_NODES_MENU_ID, Id < ?LAST_NODES_MENU_ID ->
    case check_page_title(State#state.notebook) of
	"Processes" ->
	    Node2 = lists:nth(Id - ?FIRST_NODES_MENU_ID, State#state.nodes),
	    ?OBS_PRO_WX:pro_change_node(Node2);
	_Else ->
	    ok %do something depending on page
    end,
    {noreply, State};

handle_event(Event, State) ->
    %% TODO: HANTERA MENUKOMMANDON UTIFRÅN VILKEN FLIK MAN ÄR. T.EX DUMP TO FILE ETC.
    %% BYT MENYER UTIFRÅN FLIK
    io:format("handle event ~p\n", [Event]),
    {noreply, State}.

handle_cast(Cast, State) ->
    io:format("~p:~p: Got cast ~p~n", [?MODULE, ?LINE, Cast]),
    {noreply, State}.

handle_call(get_pro_panel, _From, State) ->
    io:format("~p~p: Got Call ~p~n",[?MODULE, ?LINE, "get pro panel"]),
    ProPanel = State#state.pro_panel,
    {reply, ProPanel, State};

handle_call(Msg, _From, State) ->
    io:format("~p~p: Got Call ~p~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.

handle_info({nodeup, _Node}, State) ->
    State2 = update_node_list(State),
    {noreply, State2};

handle_info({nodedown, _Node}, State) ->
    State2 = update_node_list(State),
    {noreply, State2};

handle_info(Info, State) ->
    io:format("~p, ~p, Handle info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p terminating. Reason: ~p~n", [?MODULE, Reason]),
    ok.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_page_title(Notebook) ->
    Selection = wxNotebook:getSelection(Notebook),
    wxNotebook:getPageText(Notebook, Selection).

get_pro_panel() ->
    wx_object:call(self(), get_pro_panel).

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
    lists:foreach(fun(Record) ->
			  create_menu_item(Record, State#state.node_menu)
		  end,
		  NodesMenuItems),
    State#state{nodes = Nodes}.



create_pro_menu(MenuBar) ->
    clear_menu_bar(MenuBar),
    create_menu([#create_menu{id = ?ID_DUMP_TO_FILE, text = "&Dump to file"},
		 #create_menu{id = ?ID_OPTIONS, text = "Options"},
		 #create_menu{id = ?ID_SAVE_OPT, text = "Save options..."}],
		 %#create_menu{id = ?ID_QUIT, text = "&Quit"}],
		"File", MenuBar),
    create_menu([#create_menu{id = ?ID_REFRESH, text = "&Refresh"}],
			  "View", MenuBar),
    {Nodes, NodesMenuItems} = get_nodes(),
    create_menu(NodesMenuItems, "Nodes", MenuBar).


clear_menu_bar(MenuBar) ->
    Count = wxMenuBar:getMenuCount(MenuBar),
    remove_menu_item(MenuBar, Count).

remove_menu_item(MenuBar, 0) ->
    ok;
remove_menu_item(MenuBar, Item) ->
    wxMenuBar:remove(MenuBar, Item),
    remove_menu_item(MenuBar, Item-1).





