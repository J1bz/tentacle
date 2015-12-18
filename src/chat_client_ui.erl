%%%-------------------------------------------------------------------
%%% @author P.Alameda and J.Braun
%%% @copyright (C) 2015, Telecom Lille
%%% @doc
%%% The UI for chat_client. The UI is decorrelated from the back end, so you can open or close the UI at any
%%% moment without changing the state of the client itself. The UI can start or disconnect a client, or be started
%%% while the client is already launched.
%%% @end
%%%-------------------------------------------------------------------
-module(chat_client_ui).

%% API
-include_lib("wx/include/wx.hrl").

-define(PORT,13).
-define(ADDRESS,"127.0.0.1").

-export([start/0, notify_data/2,notify_absence/1,notify_presence/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the UI by creating a window, then start the loop for event handling.
%% The UI callback functions can be called by using ui_pid.
%% @end
%%--------------------------------------------------------------------
start()->
  register(ui_pid, spawn(fun()->
    Wx = wx:new(),
    State = wx:batch(fun() -> create_window(Wx) end),
    loop(State)
                         end)),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Callback function called by the client to update the UI for a data message.
%% Sends an event to the loop.
%% @end
%%--------------------------------------------------------------------
notify_data(Name, Message) ->
  ui_pid ! {message,Name, Message},
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Callback function called by the client to update the UI for a connecting message.
%% Sends an event to the loop.
%% @end
%%--------------------------------------------------------------------
notify_presence(Name) ->
  ui_pid ! {presence,Name},
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Callback function called by the client to update the UI for a disconnecting message.
%% Sends an event to the loop.
%% @end
%%--------------------------------------------------------------------
notify_absence(Name) ->
  ui_pid ! {absence, Name},
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
    "Chat client",
    [{size, {500, 400}}]),

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

  %% Create the chat text widget
  Outputtextwg = wxTextCtrl:new(Panel,1, [{value, ""}, {size, {300, 250}},{style,?wxTE_MULTILINE}]),
  wxTextCtrl:setEditable(Outputtextwg,true),

  %% Create the connected users list widget
  ConnectedUsers = [],
  Listboxwg = wxListBox:new(Panel, 2, [{size, {150,220}},
    {choices, [""| ConnectedUsers]},
    {style, ?wxLB_SINGLE}]),
  wxListBox:setToolTip(Listboxwg, "List of connected users"),

  %% Create the chat text input widget
  Sendtextwg = wxTextCtrl:new(Panel,3, [{value, "Enter text here"}, {size, {300, 20}}]),

  %% Create the send button widget
  Sendbutton = wxButton:new(Panel, 4, [{label, "&Send"}]),

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
  wxSizer:add(ChatSizer, Outputtextwg, []),
  wxSizer:addSpacer(ChatSizer, 10),
  wxSizer:add(ListBoxSizer, Listboxwg,[]),
  wxSizer:add(ChatSizer,ListBoxSizer,[]),
  wxSizer:add(MainSizer,ChatSizer,[]),
  wxSizer:addSpacer(MainSizer, 10),
  wxSizer:add(MainSizer, Sendtextwg, []),
  wxSizer:addSpacer(MainSizer, 5),
  wxSizer:add(ButtonSizer, Sendbutton, []),
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
  wxFrame:connect(Sendtextwg, set_focus),
  wxFrame:connect(Parent, command_menu_selected),

  %% Show the frame
  wxFrame:show(Parent),

  %% Returns the state for the loop
  {Parent, Outputtextwg, Sendtextwg, Listboxwg}.

%%--------------------------------------------------------------------
%% @doc
%% Loop to handle events used to update the UI.
%%
%% @end
%%--------------------------------------------------------------------
loop(State) ->
  {Frame, Outputtextwg, Sendtextwg, Listboxwg} = State,
  receive
    %% Handle message display
    {message, Name, Message} ->
      wxFrame:setStatusText(Frame,"Status: Receiving status..."),
      Sendtextval = Name ++ ": " ++ Message ++ "\n",
      wxTextCtrl:setValue(Outputtextwg, wxTextCtrl:getValue(Outputtextwg) ++ Sendtextval),
      wxFrame:setStatusText(Frame,"Status: Finished."),
      loop(State);
    %% Handle connecting user display
    {presence,Name} ->
      wxFrame:setStatusText(Frame,"Status: Receiving status..."),
      wxTextCtrl:setValue(Outputtextwg, wxTextCtrl:getValue(Outputtextwg) ++ Name ++ " connected.\n"),
      wxListBox:append(Listboxwg,Name),
      wxFrame:setStatusText(Frame,"Status: Finished."),
      loop(State);
    %% Handle disconnecting user display
    {absence,Name} ->
      wxFrame:setStatusText(Frame,"Status: Receiving status..."),
      IndexOfName = wxListBox:findString(Listboxwg,Name),
      if IndexOfName > -1 ->
        wxListBox:delete(Listboxwg,IndexOfName),
        wxTextCtrl:setValue(Outputtextwg, wxTextCtrl:getValue(Outputtextwg) ++ Name ++ " disconnected.\n");
        true -> error_logger:error_msg("Unknown client name tried to disconnect from UI")
      end,
      wxFrame:setStatusText(Frame,"Status: Finished."),
      loop(State);
  %% Handle click on the Send button
    #wx{id = 4, event = #wxCommand{type = command_button_clicked}} ->
      wxFrame:setStatusText(Frame,"Status: Sending..."),
      Sendtextval = wxTextCtrl:getValue(Sendtextwg),
      ToNameindex = wxListBox:getSelection(Listboxwg),
      if ToNameindex =< 0 ->
        chat_client:send(Sendtextval);
        ToNameindex > 0 ->
          chat_client:send(Sendtextval,wxListBox:getStringSelection(Listboxwg));
        true ->
          chat_client:send(Sendtextval)
      end,
      wxTextCtrl:setValue(Outputtextwg, wxTextCtrl:getValue(Outputtextwg) ++ "You: " ++ Sendtextval ++ "\n"),
      wxFrame:setStatusText(Frame,"Status: Message sent."),
      wxTextCtrl:clear(Sendtextwg),
      wxTextCtrl:setFocus(Sendtextwg),
      loop(State);
  %% Handle focus on the input text control
    #wx{id = 3, event = #wxFocus{type = set_focus}} ->
      Sendtextval = wxTextCtrl:getValue(Sendtextwg),
      if Sendtextval =:= "Enter text here" ->
        wxTextCtrl:clear(Sendtextwg);
        true -> wxTextCtrl:setFocus(Sendtextwg)
      end,
      loop(State);
  %% Handle click on the Connect submenu item
    #wx{id = ?wxID_OPEN, event = #wxCommand{type = command_menu_selected}} ->
      chat_client:start(?ADDRESS,?PORT),
      loop(State);
  %% Handle click on the Disconnect submenu item
    #wx{id = ?wxID_CLOSE, event = #wxCommand{type = command_menu_selected}} ->
      wxFrame:setStatusText(Frame,"Status: Quitting..."),
      chat_client:disconnect(),
      wxTextCtrl:setValue(Outputtextwg, wxTextCtrl:getValue(Outputtextwg) ++ "You disconnected.\n"),
      wxFrame:setStatusText(Frame,"Status: Disconnected."),
      loop(State);
  %% Handle click on the exit button
    #wx{id = ?wxID_EXIT, event = #wxCommand{type = command_button_clicked}} ->
      wxFrame:setStatusText(Frame,"Status: Quitting..."),
      wxWindow:destroy(Frame),
      ok;
  %% Handle click on the X on the upper-right of the window
    #wx{event=#wxClose{}} ->
      error_logger:info_msg("~p Closing window ~n",[self()]),
      %%chat_client:disconnect(),
      wxWindow:destroy(Frame),
      ok;
  %% Default handling
    _ ->
      loop(State)
  end.