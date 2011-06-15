-module(observer_wx).

-behaviour(wx_object).

-compile(export_all).
%% Client API
-export([start_link/0]).

%% wx_object callbacks
-export([init/1, handle_event/2, handle_cast/2, terminate/2]).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").
-include("erltop_defs.hrl").
	 

start_link() ->
    wx_object:start_link(?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Observer", [{size, {1000, 500}}]),
    State = #state{name  = frame, ref = Frame},
    UpdState = setup(State),
    wxFrame:show(Frame),
    {Frame, UpdState}.

setup(#state{ref = Frame} = State) ->
    
    %% Setup Menubar & Menus
    MenuBar = wxMenuBar:new(),
    create_menu([#create_menu{id = ?wxID_ANY, text = "Opt1"}, #create_menu{id = ?wxID_ANY, text = "Opt2"}], "File", MenuBar), %File
    create_menu([#create_menu{id = ?wxID_ANY, text = "Opt1"}, #create_menu{id = ?wxID_ANY, text = "Opt2"}], "View", MenuBar), %View
    create_menu([#create_menu{id = ?wxID_ANY, text = "Opt1"}, #create_menu{id = ?wxID_ANY, text = "Opt2"}], "Nodes", MenuBar),%node
    wxFrame:setMenuBar(Frame, MenuBar),
    
    %% Setup panels
    Panel = wxPanel:new(Frame, []),
    Notebook = wxNotebook:new(Panel, 1, [{style, ?wxBK_DEFAULT}]),
    
    SysPanel = wxPanel:new(Notebook, []),
    ProPanel = wxPanel:new(Notebook, []),
    AppPanel = wxPanel:new(Notebook, []),

    %% Setup sizer
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    
    %% System Panel
    wxNotebook:addPage(Notebook, SysPanel, "System", []),
    Env = wx:get_env(),
    ?OBS_SYS_WX:start_link(Notebook, SysPanel, Env),
    
    %% Process Panel
    wxNotebook:addPage(Notebook, ProPanel, "Processes", []),
   
    %% Application Panel
    wxNotebook:addPage(Notebook, AppPanel, "Applications", []),
    
    wxSizer:add(MainSizer, Notebook, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, MainSizer),
    
    Sel = wxNotebook:changeSelection(Notebook, 0),
    io:format("SEL: ~w\n", [Sel]),
    wxNotebook:connect(Notebook, command_notebook_page_changed),
    wxFrame:connect(Frame, close_window),
    
    PanelRec = #obj{name = panel, ref = Panel, parent = Frame},
    NoteRec = #obj{name = notebook, ref = Notebook, parent = Panel},
    SysPanelRec = #obj{name = sys_panel, ref = SysPanel},
    AppPanelRec = #obj{name = app_panel, ref = AppPanel},
    ProPanelRec = #obj{name = pro_panel, ref = ProPanel},
    
    UpdState = State#state{children = [PanelRec,
				       NoteRec,
				       SysPanelRec,
				       AppPanelRec,
				       ProPanelRec]},
    UpdState.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Callbacks
handle_event(#wx{obj = Object, event = #wxNotebook{type = command_notebook_page_changed}}, State) ->
    Selection = wxNotebook:getSelection(Object),
    Title = wxNotebook:getPageText(Object, Selection),
    case Title of
	"Processes" ->
	    ok;
	"Applications" ->
	    ok;
	"System" ->
	    ok
    end,
    {noreply, State};

handle_event(#wx{event = #wxClose{}}, State) ->
    {stop, normal, State};

handle_event(Event, State) ->
    io:format("handle event stop ~p\n", [Event]),
    {noreply, State}.

handle_cast(Cast, State) ->
    io:format("~p:~p: Got cast ~p~n", [?MODULE, ?LINE, Cast]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p terminating. Reason: ~p~n", [?MODULE, Reason]),
    wx:destroy(),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_page_title(Notebook) ->
    Selection = wxNotebook:getSelection(Notebook),
    wxNotebook:getPageText(Notebook, Selection).

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
