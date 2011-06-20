%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% File    : erltop_options.erl
%% Author  : Olle Mattsson <olle@erix.ericsson.se>
%% Description : Edit options
%%
%% Created : 12 Aug 2008 by Olle Mattsson <olle@erix.ericsson.se>
%%-------------------------------------------------------------------
-module(erltop_options).
-author('olle@erix.ericsson.se').

-export([start/4, init/4]).

-include_lib("wx/include/wx.hrl").
-include("erltop_defs.hrl").

-define(OK, 10).
-define(CANCEL, 11).
-define(SAVE, 12).

-record(state, {boxes,
		trace_options,
		parent,
		process_info_boxes,
		process_info,
		interval}).
-record(boxes, {send, 'receive', functions, events,
		on_spawn, on_link, all_spawn, all_link,
		in_window, to_file, interval}).




start(Opt, ProcessInfo, Interval, Parent) ->
    spawn_link(?MODULE, init, [Opt, ProcessInfo, Interval, Parent]).

init(Opt, ProcessInfo, Interval, Parent) ->
    Wx = wx:new(),
    {ProcessInfoBoxes, Boxes} =
	wx:batch(fun() -> create_window(Opt, ProcessInfo, Interval, Wx) end),
    loop(#state{boxes = Boxes,
		trace_options = Opt,
		parent = Parent,
		process_info_boxes = ProcessInfoBoxes,
		process_info = ProcessInfo,
		interval = Interval}).
		     
create_window(Opt, ProcessInfo, Interval, Wx) ->
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Trace Options"),
    wxFrame:connect(Frame, close_window,[{skip,true}]),
    Panel = wxPanel:new(Frame, []),

    MainSz = wxBoxSizer:new(?wxVERTICAL),
    TopSz = wxBoxSizer:new(?wxHORIZONTAL),


    TopLeftSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label,"Trace output options:"}]),
    ChkBox1 = wxCheckBox:new(Panel, ?wxID_ANY, "Trace send", []),
    check_box(ChkBox1, Opt#trace_options.send),
    ChkBox2 = wxCheckBox:new(Panel, ?wxID_ANY, "Trace receive", []),
    check_box(ChkBox2, Opt#trace_options.treceive),
    ChkBox3 = wxCheckBox:new(Panel, ?wxID_ANY, "Trace functions", []),
    check_box(ChkBox3, Opt#trace_options.functions),
    ChkBox4 = wxCheckBox:new(Panel, ?wxID_ANY, "Trace events", []),
    check_box(ChkBox4, Opt#trace_options.events),

    wxSizer:add(TopLeftSz, ChkBox1, []),
    wxSizer:add(TopLeftSz, ChkBox2, []),
    wxSizer:add(TopLeftSz, ChkBox3, []),
    wxSizer:add(TopLeftSz, ChkBox4, []),
    wxSizer:add(TopLeftSz, 150, -1),

    TopRightSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Inheritance options:"}]),

    {ChkBox5, Radio1, Radio2} = top_right(Panel, TopRightSz, [{flag, ?wxBOTTOM},{border, 5}], "spawn"),
    {ChkBox6, Radio3, Radio4} = top_right(Panel, TopRightSz, [{flag, ?wxBOTTOM},{border, 5}], "link"),
    wxRadioButton:setValue(Radio1, Opt#trace_options.on_all_spawn),
    wxRadioButton:setValue(Radio2, Opt#trace_options.on_1st_spawn),
    wxRadioButton:setValue(Radio3, Opt#trace_options.on_all_link),
    wxRadioButton:setValue(Radio4, Opt#trace_options.on_1st_link),



    Where = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label,"Trace output options:"}]),
    Radio5 = wxRadioButton:new(Panel, ?wxID_ANY, "In window", [{style, ?wxRB_GROUP}]),
    wxRadioButton:setValue(Radio5, Opt#trace_options.in_window),
    ToFile = wxBoxSizer:new(?wxHORIZONTAL),
    Radio6 = wxRadioButton:new(Panel, ?wxID_ANY, "To file    ", []),
    wxRadioButton:setValue(Radio6, Opt#trace_options.to_file),
    FileInput = wxTextCtrl:new(Panel, ?wxID_ANY, [{size, {142, -1}}]),
    BrowseButton = wxButton:new(Panel, ?wxID_ANY, [{label, "Browse"}]),

    wxSizer:add(Where, Radio5, []),
    wxSizer:add(ToFile, Radio6, [{proportion, 0}]),
    wxSizer:add(ToFile, FileInput, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(ToFile, BrowseButton, [{proportion, 0}]),
    wxSizer:add(Where, ToFile, [{flag, ?wxEXPAND}]),


    BottomSz = wxBoxSizer:new(?wxHORIZONTAL),
    OkButton = wxButton:new(Panel, ?OK, [{label, "OK"}]),
    CancelButton = wxButton:new(Panel, ?CANCEL, [{label, "Cancel"}]),
    SaveButton = wxButton:new(Panel, ?SAVE, [{label, "Save"}]),
    case Opt#trace_options.main_window of
	false ->
	    wxButton:enable(SaveButton, [{enable, false}]);
	true ->
	    ignore
    end,
    wxButton:connect(OkButton, command_button_clicked, [{userData, ok}]),
    wxButton:connect(CancelButton, command_button_clicked, [{userData, cancel}]),
    wxButton:connect(SaveButton, command_button_clicked, [{userData, ok}]),

    wxSizer:add(BottomSz, OkButton, []),
    wxSizer:add(BottomSz, CancelButton, []),
    wxSizer:add(BottomSz, SaveButton, []),


    {ProcessBox, ProcessInfoBoxes} =
	process_info_boxes(Panel, ProcessInfo),

    IntervalOption = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "Interval option"}]),
    IntervalSpinCtrl = wxSpinCtrl:new(Panel, []),
    wxSpinCtrl:setRange(IntervalSpinCtrl, 1, 100),
    wxSpinCtrl:setValue(IntervalSpinCtrl, Interval div 1000),

    wxSizer:add(TopSz, TopLeftSz, [{flag, ?wxALL},{border, 5}]),
    wxSizer:add(TopSz, TopRightSz, [{flag, ?wxALL},{border, 5}]),
    wxSizer:add(MainSz, TopSz, []),
    wxSizer:add(MainSz, Where, [{flag, ?wxALL bor ?wxEXPAND},{border, 5}]),
    wxSizer:add(MainSz, ProcessBox, [{flag, ?wxALL},{border, 5}]),

  
    case Opt#trace_options.main_window of
	true ->
	    wxSizer:add(IntervalOption, IntervalSpinCtrl, [{flag, ?wxEXPAND}]),
	    wxSizer:add(MainSz, IntervalOption, [{flag, ?wxALL bor ?wxEXPAND},{border, 5}]);
	false ->
	    wxStaticBoxSizer:destroy(IntervalOption),
	    wxSpinCtrl:destroy(IntervalSpinCtrl)
    end,

    wxSizer:add(MainSz, BottomSz, [{flag, ?wxALL},{border, 5}]),

    wxWindow:setSizer(Panel, MainSz),
    wxSizer:fit(MainSz, Frame),
    wxSizer:setSizeHints(MainSz, Frame),

    wxFrame:show(Frame),
    {ProcessInfoBoxes,
     #boxes{send = ChkBox1,
	    'receive' = ChkBox2,
	    functions = ChkBox3,
	    events = ChkBox4,
	    on_spawn = #on_spawn{checkbox = ChkBox5,
				 radio1 = Radio1,
				 radio2 = Radio2},
	    all_spawn = Radio1,
	    on_link = #on_link{checkbox = ChkBox6,
			       radio1 = Radio3,
			       radio2 = Radio4},
	    all_link = Radio3,
	    in_window = Radio5,
	    to_file = Radio6,
	    interval = IntervalSpinCtrl}}.

process_info_boxes(Panel, ProcessInfo) ->
    ProcessBox = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label,"Process info"}]),
    FirstColBoxes =
	create_check_boxes(Panel,
			   [{backtrace, "Backtrace"},
			    {binary, "Binary"},
			    {catchlevel, "Catchlevel"},
			    {current_function, "Current function"},
			    {dictionary, "Dictionary"},
			    {error_handler, "Error handler"},
			    {garbage_collection, "Garbage collection"},
			    {group_leader, "Group leader"},
			    {heap_size, "Heap size"},
			    {initial_call, "Initial call"}],
			   ProcessInfo,
			   ProcessBox),

    SecondColBoxes =
	create_check_boxes(Panel, 
			   [{last_calls, "Last calls"},
			    {links, "Links"},
			    {memory, "Memory"},
			    {message_binary, "Message binary"},
			    {message_queue_len, "Message queue length"},
			    {messages, "Message"},
			    {monitored_by, "Monitored by"},
			    {monitors, "Monitors"},
			    {priority, "Priority"},
			    {reductions, "Reductions"}],
			   ProcessInfo,
			   ProcessBox),

    ThirdColBoxes =
	create_check_boxes(Panel,
			   [{sequential_trace_token, "Sequential trace token"},
			    {stack_size, "Stack size"},
			    {status, "Status"},
			    {suspending, "Suspending"},
			    {total_heap_size,"Total heap size"},
			    {trace, "Trace"},
			    {trap_exit, "Trap exit"}],
			   ProcessInfo,
			   ProcessBox),
    {ProcessBox, FirstColBoxes ++ SecondColBoxes ++ ThirdColBoxes}.

create_check_boxes(Panel, TextList, ProcessInfo, ProcessInfoSz) ->
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    Boxes = lists:map(fun({Atom, Text}) ->
			      CheckBox = wxCheckBox:new(Panel, ?wxID_ANY, Text, []),
			      check_box(CheckBox, lists:member(Atom, ProcessInfo)),
			      wxSizer:add(Sizer, CheckBox, []),
			      {Atom, CheckBox}
		      end,
		      TextList),
    wxSizer:add(ProcessInfoSz, Sizer, []),
    Boxes.


check_box(ChkBox, Bool) ->
    case Bool of
	true ->
	    wxCheckBox:set3StateValue(ChkBox, ?wxCHK_CHECKED);
	false ->
	    ignore
    end.

top_right(Panel, TopRightSz, Options, Text) ->
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    ChkBox = wxCheckBox:new(Panel, ?wxID_ANY, "Inherit on " ++ Text, []),
    wxCheckBox:set3StateValue(ChkBox, ?wxCHK_CHECKED),
    RadioSz = wxBoxSizer:new(?wxVERTICAL),
    Radio1 = wxRadioButton:new(Panel, ?wxID_ANY, "All " ++ Text, [{style, ?wxRB_GROUP}]),
    Radio2 = wxRadioButton:new(Panel, ?wxID_ANY, "First " ++ Text ++ " only", []),
    wxSizer:add(Sizer, ChkBox, []),
    wxSizer:add(RadioSz, Radio1, []),
    wxSizer:add(RadioSz, Radio2, []),
    wxSizer:add(Sizer, RadioSz, [{flag, ?wxLEFT},{border, 20}]),
    wxSizer:add(TopRightSz, Sizer, Options),
    wxCheckBox:connect(ChkBox, command_checkbox_clicked, []),
    {ChkBox, Radio1, Radio2}.

read_trace_boxes(ChkBoxes = #boxes{on_spawn = OnSpawn, on_link = OnLink}, Options) ->
    {On1stSpawn2, OnAllSpawn2} =
	case wxCheckBox:isChecked(OnSpawn#on_spawn.checkbox) of
	    true ->
		OnAllSpawn = wxRadioButton:getValue(OnSpawn#on_spawn.radio1),
		On1stSpawn = wxRadioButton:getValue(OnSpawn#on_spawn.radio2),
		{On1stSpawn, OnAllSpawn};
	    false ->
		{false, false}
	end,
    {On1stLink2, OnAllLink2} =
	case wxCheckBox:isChecked(OnLink#on_link.checkbox) of
	    true ->
		OnAllLink = wxRadioButton:getValue(OnLink#on_link.radio1),
		On1stLink = wxRadioButton:getValue(OnLink#on_link.radio2),
		{On1stLink, OnAllLink};
	    false ->
		{false, false}
	end,
    Options#trace_options{send = wxCheckBox:isChecked(ChkBoxes#boxes.send),
			  treceive = wxCheckBox:isChecked(ChkBoxes#boxes.'receive'),
			  functions = wxCheckBox:isChecked(ChkBoxes#boxes.functions),
			  events = wxCheckBox:isChecked(ChkBoxes#boxes.events),
			  
			  on_all_spawn = OnAllSpawn2,
			  on_1st_spawn = On1stSpawn2,
			  on_all_link = OnAllLink2,
			  on_1st_link = On1stLink2,
			  
			  in_window = wxRadioButton:getValue(ChkBoxes#boxes.in_window),
			  to_file = wxRadioButton:getValue(ChkBoxes#boxes.to_file)}.

read_process_info_boxes(ProcInfoBoxes) ->
    Filter =
	fun({Atom, Box}) ->
		case wxCheckBox:isChecked(Box) of
		    false ->
			false;
		    true ->
			{true, Atom}
		end
	end,
    lists:zf(Filter, ProcInfoBoxes).
	     
enable(OnSpawn, OnLink) ->
    case wxCheckBox:isChecked(OnSpawn#on_spawn.checkbox) of
	false ->
	    wxWindow:disable(OnSpawn#on_spawn.radio1),
	    wxWindow:disable(OnSpawn#on_spawn.radio2);
	true ->
	    wxWindow:enable(OnSpawn#on_spawn.radio1),
	    wxWindow:enable(OnSpawn#on_spawn.radio2)
    end,
    case wxCheckBox:isChecked(OnLink#on_link.checkbox) of
	false ->
	    wxWindow:disable(OnLink#on_link.radio1),
	    wxWindow:disable(OnLink#on_link.radio2);
	true ->
	    wxWindow:enable(OnLink#on_link.radio1),
	    wxWindow:enable(OnLink#on_link.radio2)
    end.


loop(State = #state{boxes = Boxes}) ->
    receive
	#wx{event = #wxClose{type = close_window}} ->
	    exit(shutdown);
%%%-----------------------------------------------------------------
	#wx{event = #wxCommand{type = command_checkbox_clicked}} ->
	    wx:batch(fun() -> enable(Boxes#boxes.on_spawn, Boxes#boxes.on_link) end),
	   ?MODULE:loop(State);
%%%-----------------------------------------------------------------
	#wx{id = ?SAVE, event = #wxCommand{type = command_button_clicked}} ->
	    File = observer_pro_wx:save_options(State#state.trace_options, State#state.process_info),
	    State#state.parent ! {save, File},
	    ?MODULE:loop(State);
%%%-----------------------------------------------------------------
	#wx{id = ?CANCEL, event = #wxCommand{type = command_button_clicked},
	    userData = cancel} ->
	    io:format("Cancel\n", []),
	    exit(shutdown);
%%%-----------------------------------------------------------------
	#wx{id = ?OK, event = #wxCommand{type = command_button_clicked},
	    userData = ok} ->
	    Trace = wx:batch(fun() ->
				     read_trace_boxes(State#state.boxes, State#state.trace_options)
			     end),
	    Atoms = wx:batch(fun() -> read_process_info_boxes(State#state.process_info_boxes) end),
	    Interval = 
		case Trace#trace_options.main_window of
		    true ->
			wxSpinCtrl:getValue(Boxes#boxes.interval) * 1000;
		    false ->
			ignore
		end,

	    
	    case Trace#trace_options.to_file of
		true ->
		    File = filename:join(os:getenv("HOME"), ".erlang_tools/pman.log"),
		    State#state.parent ! {checked, Trace#trace_options{file = File}, Atoms, Interval};
		false ->
		    State#state.parent ! {checked, Trace, Atoms, Interval}
	    end,
	    exit(shutdown);
	Any ->
	    io:format("~p~p: received unexpected message: ~p\n", [?MODULE, self(), Any]),
	    ?MODULE:loop(State)
    end.
