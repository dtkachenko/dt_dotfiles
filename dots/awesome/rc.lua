-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")

-- Load Debian menu entries
require("debian.menu")

-- sudo aptitude install awesome-extra
require("vicious")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init("/home/dtkachenko/.config/awesome/themes/zenburn/theme.lua")

-- This is used later as the default terminal and editor to run.
-- terminal = "x-terminal-emulator"
terminal = "terminator"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor
host = io.lines("/proc/sys/kernel/hostname")()

-- Default modkey.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier
}
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9 }, s, layouts[2])
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awful.util.getdir("config") .. "/rc.lua" },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "Debian", debian.menu.Debian_menu.Debian },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })
-- }}}

-- {{{ Wibox
-- }}}
-- Create a systray
-- {{{
mysystray = widget({ type = "systray" })
-- }}}
-- battery widget
-- {{{
has_battery = awful.util.file_readable("/sys/class/power_supply/BAT0")
batwidget = widget({ type = "textbox" })
vicious.register(batwidget, vicious.widgets.bat, "B: $1/$2", 10, "BAT0")
-- }}}
-- Gmailwidget
-- {{{
-- in .netrc login and password format
-- machine mail.google.com login user@mail password pass
mygmail = widget({ type = "textbox" })
mygmail_t = awful.tooltip({ objects = { mygmail }, })
vicious.register(mygmail, vicious.widgets.gmail,
      function (widget, args)
        mygmail_t:set_text(args["{subject}"])
        return ' G: <span color="white" weight="bold">'..args["{count}"]..'</span> '
        end, 10)
-- }}}

-- Create a wibox for each screen and add it
-- {{{
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, awful.tag.viewnext),
                    awful.button({ }, 5, awful.tag.viewprev)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))
-- }}}
-- Reusable separator
-- {{{
separator = widget({ type = "imagebox" })
separator.image = image(beautiful.separator)
-- }}}
-- Memory widget
-- {{{
memwidget = widget({ type = "textbox" })
vicious.register(memwidget, vicious.widgets.mem, "M: $1%", 13)
-- }}}
-- Cpu widget
-- {{{
cpuwidget = widget({ type = "textbox" })
vicious.register(cpuwidget, vicious.widgets.cpu, "C: $1%")
-- }}}
-- Date widget
-- {{{
datewidget = widget({ type = "textbox" })
vicious.register(datewidget, vicious.widgets.date, "%b %d, %R", 60)
-- }}}
-- Network usage widget
-- {{{
netwidgete = widget({ type = "textbox" })
vicious.register(netwidgete, vicious.widgets.net, 'W0: <span color="#CC9393">${wlan0 down_kb}</span> <span color="#7F9F7F">${wlan0 up_kb}</span>', 3)
netwidgetw = widget({ type = "textbox" })
vicious.register(netwidgetw, vicious.widgets.net, 'E0: <span color="#CC9393">${eth0 down_kb}</span> <span color="#7F9F7F">${eth0 up_kb}</span>', 3)
--if host == 'dtkachenko-laptop' then
--  vicious.register(netwidget, vicious.widgets.net, 'W0: <span color="#CC9393">${wlan0 down_kb}</span> <span color="#7F9F7F">${wlan0 up_kb}</span>', 3)
--else
--  vicious.register(netwidget, vicious.widgets.net, 'E0: <span color="#CC9393">${eth0 down_kb}</span> <span color="#7F9F7F">${eth0 up_kb}</span>', 3)
--end
-- }}}
-- Volume vidget
-- {{{
volumecfg = {}
volumecfg.cardid  = 0
volumecfg.channel = "Master"
volumecfg.widget = widget({ type = "textbox", name = "volumecfg.widget", align = "right" })
-- command must start with a space!
volumecfg.mixercommand = function (command)
       local fd = io.popen("amixer -c " .. volumecfg.cardid .. command)
       local status = fd:read("*all")
       fd:close()
       local volume = string.match(status, "(%d?%d?%d)%%")
       volume = string.format("S:% 3d", volume)
       status = string.match(status, "%[(o[^%]]*)%]")
       if string.find(status, "on", 1, true) then
               volume = volume .. "%"
       else
               volume = volume .. "M"
       end
       volumecfg.widget.text = volume
end
volumecfg.update = function ()
       volumecfg.mixercommand(" sget " .. volumecfg.channel)
end
volumecfg.up = function ()
       volumecfg.mixercommand(" sset " .. volumecfg.channel .. " 5%+")
end
volumecfg.down = function ()
       volumecfg.mixercommand(" sset " .. volumecfg.channel .. " 5%-")
end
volumecfg.toggle = function ()
       -- volumecfg.mixercommand(" sset " .. volumecfg.channel .. " toggle")
       volumecfg.mixercommand(" sset " .. volumecfg.channel .. " 0% ")
end
volumecfg.widget:buttons(awful.util.table.join(
       awful.button({ }, 4, function () volumecfg.up() end),
       awful.button({ }, 5, function () volumecfg.down() end),
       awful.button({ }, 1, function () volumecfg.toggle() end)
))
volumecfg.update()

awful.hooks.timer.register(60, function ()
      volumecfg.update()
end)
-- }}}

-- pomodoro timer widget
pomodoro = {}
-- tweak these values in seconds to your liking
pomodoro.pause_duration = 300
pomodoro.work_duration = 1500

pomodoro.pause_title = "Pause finished."
pomodoro.pause_text = "Get back to work!"
pomodoro.work_title = "Pomodoro finished."
pomodoro.work_text = "Time for a pause!"
pomodoro.working = true
pomodoro.left = pomodoro.work_duration
pomodoro.widget = widget({ type = "textbox" })
pomodoro.timer = timer { timeout = 1 }

function pomodoro:start()
pomodoro.last_time = os.time()
pomodoro.timer:start()
end

function pomodoro:stop()
pomodoro.timer:stop()
end

function pomodoro:reset()
pomodoro.timer:stop()
pomodoro.left = pomodoro.work_duration
pomodoro:settime(pomodoro.work_duration)
end

function pomodoro:settime(t)
  if t >= 3600 then -- more than one hour!
    t = os.date("%X", t-3600)
  else
    t = os.date("%M:%S", t)
  end
  self.widget.text = string.format("Pomodoro: <b>%s</b>", t)
end

function pomodoro:notify(title, text, duration, working)
  naughty.notify {
    bg = "#ff0000",
    fg = "#ffffff",
    font = "Verdana 20",
    screen = mouse.screen,
    title = title,
    text = text,
    timeout = 10,
--    icon = "/usr/share/app-install/icons/_usr_share_pixmaps_tomatoes_icon.png"
  }

  pomodoro.left = duration
  pomodoro:settime(duration)
  pomodoro.working = working
end

pomodoro:settime(pomodoro.work_duration)

pomodoro.widget:buttons(
  awful.util.table.join(
    awful.button({ }, 1, function() pomodoro:start() end),
    awful.button({ }, 2, function() pomodoro:stop() end),
    awful.button({ }, 3, function() pomodoro:reset() end)
))

pomodoro.timer:add_signal("timeout", function()
  local now = os.time()
  pomodoro.left = pomodoro.left - (now - pomodoro.last_time)
  pomodoro.last_time = now

  if pomodoro.left > 0 then
    pomodoro:settime(pomodoro.left)
  else
    if pomodoro.working then
      pomodoro:notify(pomodoro.work_title, pomodoro.work_text,
pomodoro.pause_duration, false)
    else
      pomodoro:notify(pomodoro.pause_title, pomodoro.pause_text,
        pomodoro.work_duration, true)
    end
    pomodoro.timer:stop()
  end
end)


---- Keyboard widget
---- {{{
--kbdcfg = {}
--kbdcfg.cmd = "setxkbmap"
--kbdcfg.layout = { "us","ru" }
--kbdcfg.current = 1
--kbdcfg.widget = widget({ type = "textbox", align = "right" })
--kbdcfg.widget.text = " " .. kbdcfg.layout[kbdcfg.current] .. " "
--kbdcfg.switch = function ()
--    kbdcfg.current = kbdcfg.current % #(kbdcfg.layout) + 1
--    local t = " " .. kbdcfg.layout[kbdcfg.current] .. " "
--    kbdcfg.widget.text = t
--    os.execute( kbdcfg.cmd .. t )
--end
--kbdcfg.widget:buttons(awful.util.table.join(
--    awful.button({ }, 1, function () kbdcfg.switch() end)
--    ))
---- }}}

-- Keyboard layout widget
kbdwidget = widget({type = "textbox", name = "kbdwidget"})
kbdwidget.border_width = 1
--kbdwidget.border_color = beautiful.fg_normal
kbdwidget.text = " Eng "

dbus.request_name("session", "ru.gentoo.kbdd")
dbus.add_match("session", "interface='ru.gentoo.kbdd',member='layoutChanged'")
dbus.add_signal("ru.gentoo.kbdd", function(...)
    local data = {...}
    local layout = data[2]
    lts = {[0] = "Eng", [1] = "Рус"}
    kbdwidget.text = " "..lts[layout].." "
    end
)


for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(function(c)
                                              return awful.widget.tasklist.label.currenttags(c, s)
                                          end, mytasklist.buttons)
    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            mylauncher,
            mytaglist[s],
            mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        mylayoutbox[s],
        datewidget,
        s == 1 and mysystray or nil,
        has_battery and separator,
        has_battery and batwidget,
        separator, volumecfg.widget,
        separator, mygmail,
        separator, memwidget,
        separator, cpuwidget,
        separator, netwidgete,
        separator, netwidgetw,
        --separator, kbdcfg.widget,
        separator, kbdwidget,
        separator, pomodoro.widget,
        separator, mytasklist[s],
        layout = awful.widget.layout.horizontal.rightleft
    }
end
-- }}}

function lock_screen()
  os.execute("gnome-screensaver-command --lock && sleep 5")
end

function suspend()
  lock_screen()
  os.execute("sudo pm-suspend")
end

function hibernate()
  lock_screen()
  os.execute("sudo pm-hibernate")
end

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "w", function () mymainmenu:show({keygrabber=true}) end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    -- Sound control
    awful.key({ }, "XF86AudioRaiseVolume", function () volumecfg.up() end),
    awful.key({ }, "XF86AudioLowerVolume", function () volumecfg.down() end),
    awful.key({ }, "XF86AudioMute", function () volumecfg.toggle() end),

    -- screen and power control
    awful.key({ modkey, "Mod1" }, "l", function () lock_screen() end ),
    awful.key({ }, "XF86Sleep", function () suspend() end ),

    -- Pomodoro
    awful.key({ modkey }, "F9", function () pomodoro:start() end),
    awful.key({ modkey }, "F10", function () pomodoro:stop() end),
    awful.key({ modkey }, "F11", function () pomodoro:reset() end),

--    -- layout widget key binding
--    awful.key({ "Mod1" }, "Shift_R", function () kbdcfg.switch() end ),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end)

)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     size_hints_honor = false,
                     floating = false} },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    -- Set Firefox to always map on tags number 2 of screen 1.
    { rule = { class = "Firefox" },
      properties = { tag = tags[1][2],
                     floating = false } },
    { rule = { class = "Google-chrome" },
      properties = { tag = tags[1][9],
                     floating = true } },
    { rule = { class = "Skype" },
      properties = { tag = tags[1][3] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

-- {{{ After startup
awful.util.spawn_with_shell("wmname LG3D")
awful.util.spawn_with_shell("if ! ps -ef | grep -v grep | grep wicd-client ; then  wicd-client -t  ; fi")
awful.util.spawn_with_shell("kbdd")
awful.util.spawn_with_shell("setxkbmap -layout \"us,ru\"")
awful.util.spawn_with_shell("setxkbmap -option \"grp:caps_toggle,grp_led:caps\"")

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- Screen lock command "gnome-screensaver-command --lock && sleep 5"
-- Sound configureation tool "pavucontrol"
-- Aperance tool "lxappearance"
-- Network management ""wicd-clinet -t""
