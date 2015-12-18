%%%-------------------------------------------------------------------
%%% @author P.Alameda and J.Braun
%%% @copyright (C) 2015, Telecom Lille
%%% @doc
%%% The UI for chat_server. The UI is decorrelated from the back end, so you can open or close the UI at any
%%% moment without changing the state of the server itself. The UI can start or disconnect a server, or be started
%%% while the server is already launched.
%%% @end
%%%-------------------------------------------------------------------
-module(chat_server_ui).

%% API
-include_lib("wx/include/wx.hrl").

-define(PORT,13).

-export([start/0,notify_absence/1,notify_presence/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the UI by creating a window, then start the loop for event handling.
%% The UI callback functions can be called by using server_ui_pid.
%% @end
%%--------------------------------------------------------------------
start()->
  register(server_ui_pid, spawn(fun()->
    Wx = wx:new(),
    State = wx:batch(fun() -> create_window(Wx) end),
    loop(State)
                         end)),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Callback function called by the client to update the UI for a connecting user.
%% Sends an event to the loop.
%% @end
%%--------------------------------------------------------------------
notify_presence(Name) ->
  server_ui_pid ! {presence,Name},
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Callback function called by the client to update the UI for a disconnecting user.
%% Sends an event to the loop.
%% @end
%%--------------------------------------------------------------------
notify_absence(Name) ->
  server_ui_pid ! {absence, Name},
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Create the UI elements and returns a state with each interacting element.
%%
%% @end
%%--------------------------------------------------------------------
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
  %% create and add the status bar
  wxFrame:createStatusBar(Parent),

  %% Create the connected users list widget
  ConnectedUsers = [],
  Listboxwg = wxListBox:new(Panel, 2, [{size, {150,220}},
    {choices, [""| ConnectedUsers]},
    {style, ?wxLB_SINGLE}]),
  wxListBox:setToolTip(Listboxwg, "List of connected users"),

  %% Create the kick button widget
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

  %% Show the frame
  wxFrame:show(Parent),

  %% Returns the state for the loop
  {Parent, Listboxwg}.

%%--------------------------------------------------------------------
%% @doc
%% Loop to handle events used to update the UI.
%%
%% @end
%%--------------------------------------------------------------------
loop(State) ->
  {Frame, Listboxwg} = State,
  receive
  %% Handle connecting user display
    {presence,Name} ->
      wxFrame:setStatusText(Frame,"Status: Receiving status..."),
      wxListBox:append(Listboxwg,Name),
      wxFrame:setStatusText(Frame,"Status: Finished."),
      loop(State);
  %% Handle disconnecting user display
    {absence,Name} ->
      wxFrame:setStatusText(Frame,"Status: Receiving status..."),
      IndexOfName = wxListBox:findString(Listboxwg,Name),
      if IndexOfName > -1 ->
        wxListBox:delete(Listboxwg,IndexOfName);
        true -> error_logger:error_msg("Unknown client name tried to disconnect from UI")
      end,
      wxFrame:setStatusText(Frame,"Status: Finished."),
      loop(State);
  %% Handle click on the Kick button
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
  %% Handle click on the Connect submenu item
    #wx{id = ?wxID_OPEN, event = #wxCommand{type = command_menu_selected}} ->
      chat_server:start(?PORT),
      loop(State);
  %% Handle click on the exit button
    #wx{id = ?wxID_EXIT, event = #wxCommand{type = command_button_clicked}} ->
      wxFrame:setStatusText(Frame,"Status: Quitting..."),
      wxWindow:destroy(Frame),
      ok;
  %% Handle click on the X on the upper-right of the window
    #wx{event=#wxClose{}} ->
      error_logger:info_msg("~p Closing window ~n",[self()]),
      wxWindow:destroy(Frame),
      ok;
  %% Default handling
    _ ->
      loop(State)
  end.