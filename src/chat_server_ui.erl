%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Telecom Lille
%%% @doc
%%%
%%% @end
%%% Created : 29. oct. 2015 09:27
%%%-------------------------------------------------------------------
-module(chat_server_ui).

%% API
-include_lib("wx/include/wx.hrl").

-export([start/0,notify_absence/1,notify_presence/1]).
-define(PORT,13).

%% Start the
start()->
  register(server_ui_pid, spawn(fun()->
    Wx = wx:new(),
    State = wx:batch(fun() -> create_window(Wx) end),
    loop(State)
                         end)),
  ok.

create_window(Wx)->

  %% Create Frame

  Parent = wxFrame:new(Wx,
    -1,
    "Chat server",
    [{size, {220, 380}}]),

  %% Create panel
  Panel = wxScrolledWindow:new(Parent, []),
  %% Create menu bar
  Menubar = wxMenuBar:new(),
  %% Create menu
  Menuconnexion = wxMenu:new(),
  wxMenu:append(Menuconnexion,?wxID_OPEN, "&Connect"),
  wxMenu:append(Menuconnexion,?wxID_CLOSE, "&Disconnect"),
  %% create and add the status bar
  wxFrame:createStatusBar(Parent),

  %% Create the connected users list widget
  ConnectedUsers = [],
  Listboxwg = wxListBox:new(Panel, 2, [{size, {150,220}},
    {choices, [""| ConnectedUsers]},
    {style, ?wxLB_SINGLE}]),
  wxListBox:setToolTip(Listboxwg, "List of connected users"),

  %% Create the send button widget
  Kickbutton = wxButton:new(Panel, 4, [{label, "&Kick"}]),

  %% Create the exit button widget
  Exitbutton = wxButton:new(Panel, ?wxID_EXIT, [{label, "E&xit"}]),


  %% Setup sizers
  OuterSizer = wxBoxSizer:new(?wxHORIZONTAL),
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  ChatSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ListBoxSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "Connected users"}]),
  ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),

  %% Add sizers and widgets
  wxSizer:addSpacer(MainSizer, 5),
  wxSizer:add(ListBoxSizer, Listboxwg,[]),
  wxSizer:add(ChatSizer,ListBoxSizer,[]),
  wxSizer:add(MainSizer,ChatSizer,[]),
  wxSizer:addSpacer(MainSizer, 10),

  wxSizer:add(ButtonSizer, Kickbutton, []),

  wxSizer:addSpacer(ButtonSizer, 5),
  wxSizer:add(ButtonSizer, Exitbutton, []),
  wxSizer:add(MainSizer, ButtonSizer, []),

  wxSizer:addSpacer(OuterSizer, 10),
  wxSizer:add(OuterSizer, MainSizer, []),

  %% Set every element
  wxPanel:setSizer(Panel, OuterSizer),
  wxFrame:setMenuBar(Parent, Menubar),
  wxFrame:setStatusText(Parent,"Status: Started."),
  wxScrolledWindow:setScrollRate(Panel, 5, 5),

  %% Add menu bar
  wxMenuBar:append(Menubar, Menuconnexion, "Connexion"),

  %% Set events
  wxFrame:connect(Parent, close_window),
  wxFrame:connect(Parent, command_button_clicked),
  wxFrame:connect(Parent, command_menu_selected),

  wxFrame:show(Parent),

  {Parent, Listboxwg}.


loop(State) ->
  {Frame, Listboxwg} = State,

  receive
    {presence,Name} ->
      wxFrame:setStatusText(Frame,"Status: Receiving status..."),
      wxListBox:append(Listboxwg,Name),
      wxFrame:setStatusText(Frame,"Status: Finished."),
      loop(State);
    {absence,Name} ->
      wxFrame:setStatusText(Frame,"Status: Receiving status..."),
      IndexOfName = wxListBox:findString(Listboxwg,Name),
      if IndexOfName > -1 ->
        wxListBox:delete(Listboxwg,IndexOfName);
        true -> error_logger:error_msg("Unknown client name tried to disconnect from UI")
      end,
      wxFrame:setStatusText(Frame,"Status: Finished."),
      loop(State);
  %% the Send button is clicked
    #wx{id = 4, event = #wxCommand{type = command_button_clicked}} ->
      wxFrame:setStatusText(Frame,"Status: Kicking..."),
      ToNameindex = wxListBox:getSelection(Listboxwg),
      if ok;
        ToNameindex > 0 ->
          UserName = wxListBox:getStringSelection(Listboxwg),
        chat_server:kick(UserName);
        true ->
          ok
      end,
      wxFrame:setStatusText(Frame,"Status: User kicked."),
      loop(State);
  %% The connect menu item is clicked
    #wx{id = ?wxID_OPEN, event = #wxCommand{type = command_menu_selected}} ->
      chat_server:start(?PORT),
      loop(State);
  %% The disconnect menu item is clicked
    #wx{id = ?wxID_CLOSE, event = #wxCommand{type = command_menu_selected}} ->
      wxFrame:setStatusText(Frame,"Status: Quitting..."),
      %%TODO Add disconnect command from server
      wxFrame:setStatusText(Frame,"Status: Disconnected."),
      loop(State);
  %% The exit button is clicked
    #wx{id = ?wxID_EXIT, event = #wxCommand{type = command_button_clicked}} ->
      wxFrame:setStatusText(Frame,"Status: Quitting..."),
      wxWindow:destroy(Frame),
      ok;
  %% Window Close Event
    #wx{event=#wxClose{}} ->
      error_logger:info_msg("~p Closing window ~n",[self()]),
      wxWindow:destroy(Frame),
      ok;
  %% default handling
    _ ->
      loop(State)
  end.

notify_presence(Name) ->
  server_ui_pid ! {presence,Name},
  ok.

notify_absence(Name) ->
  server_ui_pid ! {absence, Name},
  ok.