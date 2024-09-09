local wezterm = require 'wezterm'
local act = wezterm.action

local config = wezterm.config_builder()
-- config.color_scheme = 'Spring'
-- config.color_scheme = 'Tango Adapted'
config.color_scheme = 'Raycast_Light'
-- config.color_scheme = 'Modus-Operandi'
-- config.color_scheme = 'Modus-Vivendi'
config.force_reverse_video_cursor = true

config.font = wezterm.font({ family = 'GoMono Nerd Font Mono' })
config.font_size = 12
local TITLEBAR_COLOR = '#333333'
config.window_frame = {
  font = wezterm.font { family = 'GoMono Nerd Font Mono', weight = 'Bold' },
  font_size = 13.0,
  active_titlebar_bg = TITLEBAR_COLOR,
  inactive_titlebar_bg = TITLEBAR_COLOR,
}

-- config.front_end = "WebGpu"
-- config.front_end = "OpenGL"
-- config.front_end = "Software"
config.window_background_opacity = 0.85
config.native_macos_fullscreen_mode = true
config.macos_window_background_blur = 30
config.window_decorations = 'INTEGRATED_BUTTONS|RESIZE'
config.window_padding = { left = 2, right = 2, top = 2, bottom = 0 }
-- config.hide_tab_bar_if_only_one_tab = true
-- config.send_composed_key_when_left_alt_is_pressed = false
config.use_ime = false
config.enable_csi_u_key_encoding = true
config.enable_kitty_keyboard = true
config.disable_default_key_bindings = true
config.keys = {
    { key = "`", mods = "CTRL", action = act.SendString("\x18@c`") },

    -- Copy
    { key = "c", mods = "CMD", action = act.CopyTo("Clipboard") },

    -- Paste
    { key = "v", mods = "CMD", action = act.PasteFrom("Clipboard") },

    -- Leader stuff
    { key = "p", mods = "CMD", action = act.ActivateCommandPalette },

    -- Font Size
    { key = "=", mods = "CMD", action = act.IncreaseFontSize },
    { key = "-", mods = "CMD", action = act.DecreaseFontSize },

  {key = "`", mods = "CTRL", action = act.SendString("\x18@c`") },
  {key = "~", mods = "SHIFT|CTRL", action = act.SendString("\x18@c~") },
  {key = "`", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c`") },
  {key = "~", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c~") },
  {key = "1", mods = "CTRL", action = act.SendString("\x18@c1") },
  {key = "!", mods = "SHIFT|CTRL", action = act.SendString("\x18@c!") },
  {key = "1", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c1") },
  {key = "!", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c!") },
  {key = "2", mods = "CTRL", action = act.SendString("\x18@c2") },
  {key = "@", mods = "SHIFT|CTRL", action = act.SendString("\x18@c@") },
  {key = "2", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c2") },
  {key = "@", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c@") },
  {key = "3", mods = "CTRL", action = act.SendString("\x18@c3") },
  {key = "#", mods = "SHIFT|CTRL", action = act.SendString("\x18@c#") },
  {key = "3", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c3") },
  {key = "#", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c#") },
  {key = "4", mods = "CTRL", action = act.SendString("\x18@c4") },
  {key = "$", mods = "SHIFT|CTRL", action = act.SendString("\x18@c$") },
  {key = "4", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c4") },
  {key = "$", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c$") },
  {key = "5", mods = "CTRL", action = act.SendString("\x18@c5") },
  {key = "%", mods = "SHIFT|CTRL", action = act.SendString("\x18@c%") },
  {key = "5", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c5") },
  {key = "%", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c%") },
  {key = "6", mods = "CTRL", action = act.SendString("\x18@c6") },
  {key = "^", mods = "SHIFT|CTRL", action = act.SendString("\x18@c^") },
  {key = "6", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c6") },
  {key = "^", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c^") },
  {key = "7", mods = "CTRL", action = act.SendString("\x18@c7") },
  {key = "&", mods = "SHIFT|CTRL", action = act.SendString("\x18@c&") },
  {key = "7", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c7") },
  {key = "&", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c&") },
  {key = "8", mods = "CTRL", action = act.SendString("\x18@c8") },
  {key = "*", mods = "SHIFT|CTRL", action = act.SendString("\x18@c*") },
  {key = "8", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c8") },
  {key = "*", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c*") },
  {key = "9", mods = "CTRL", action = act.SendString("\x18@c9") },
  {key = "(", mods = "SHIFT|CTRL", action = act.SendString("\x18@c(") },
  {key = "9", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c9") },
  {key = "(", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c(") },
  {key = "0", mods = "CTRL", action = act.SendString("\x18@c0") },
  {key = ")", mods = "SHIFT|CTRL", action = act.SendString("\x18@c)") },
  {key = "0", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c0") },
  {key = ")", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c)") },
  {key = "-", mods = "CTRL", action = act.SendString("\x18@c-") },
  {key = "_", mods = "SHIFT|CTRL", action = act.SendString("\x18@c_") },
  {key = "-", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c-") },
  {key = "_", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c_") },
  {key = "=", mods = "CTRL", action = act.SendString("\x18@c=") },
  {key = "+", mods = "SHIFT|CTRL", action = act.SendString("\x18@c+") },
  {key = "=", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c=") },
  {key = "+", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c+") },
  {key = "Q", mods = "SHIFT|CTRL", action = act.SendString("\x18@cQ") },
  {key = "Q", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cQ") },
  {key = "W", mods = "SHIFT|CTRL", action = act.SendString("\x18@cW") },
  {key = "W", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cW") },
  {key = "E", mods = "SHIFT|CTRL", action = act.SendString("\x18@cE") },
  {key = "E", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cE") },
  {key = "R", mods = "SHIFT|CTRL", action = act.SendString("\x18@cR") },
  {key = "R", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cR") },
  {key = "T", mods = "SHIFT|CTRL", action = act.SendString("\x18@cT") },
  {key = "T", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cT") },
  {key = "Y", mods = "SHIFT|CTRL", action = act.SendString("\x18@cY") },
  {key = "Y", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cY") },
  {key = "U", mods = "SHIFT|CTRL", action = act.SendString("\x18@cU") },
  {key = "U", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cU") },
  {key = "I", mods = "SHIFT|CTRL", action = act.SendString("\x18@cI") },
  {key = "I", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cI") },
  {key = "O", mods = "SHIFT|CTRL", action = act.SendString("\x18@cO") },
  {key = "O", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cO") },
  {key = "P", mods = "SHIFT|CTRL", action = act.SendString("\x18@cP") },
  {key = "P", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cP") },
  {key = "[", mods = "CTRL", action = act.SendString("\x18@c[") },
  {key = "{", mods = "SHIFT|CTRL", action = act.SendString("\x18@c{") },
  {key = "[", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c[") },
  {key = "{", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c{") },
  {key = "]", mods = "CTRL", action = act.SendString("\x18@c]") },
  {key = "}", mods = "SHIFT|CTRL", action = act.SendString("\x18@c}") },
  {key = "]", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c]") },
  {key = "}", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c}") },
  {key = "A", mods = "SHIFT|CTRL", action = act.SendString("\x18@cA") },
  {key = "A", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cA") },
  {key = "S", mods = "SHIFT|CTRL", action = act.SendString("\x18@cS") },
  {key = "S", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cS") },
  {key = "D", mods = "SHIFT|CTRL", action = act.SendString("\x18@cD") },
  {key = "D", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cD") },
  {key = "F", mods = "SHIFT|CTRL", action = act.SendString("\x18@cF") },
  {key = "F", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cF") },
  {key = "G", mods = "SHIFT|CTRL", action = act.SendString("\x18@cG") },
  {key = "G", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@cG") },
  {key = "G", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cG") },
  {key = "H", mods = "SHIFT|CTRL", action = act.SendString("\x18@cH") },
  {key = "H", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cH") },
  {key = "J", mods = "SHIFT|CTRL", action = act.SendString("\x18@cJ") },
  {key = "J", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cJ") },
  {key = "K", mods = "SHIFT|CTRL", action = act.SendString("\x18@cK") },
  {key = "K", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cK") },
  {key = "L", mods = "SHIFT|CTRL", action = act.SendString("\x18@cL") },
  {key = "L", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cL") },
  {key = ";", mods = "CTRL", action = act.SendString("\x18@c;") },
  {key = ":", mods = "SHIFT|CTRL", action = act.SendString("\x18@c:") },
  {key = ";", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c;") },
  {key = ":", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c:") },
  {key = "'", mods = "CTRL", action = act.SendString("\x18@c'") },
  {key = "\"", mods = "SHIFT|CTRL", action = act.SendString("\x18@c\"") },
  {key = "'", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c'") },
  {key = "\"", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c\"") },
  {key = "\\", mods = "CTRL", action = act.SendString("\x18@c\\") },
  {key = "|", mods = "SHIFT|CTRL", action = act.SendString("\x18@c|") },
  {key = "\\", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c\\") },
  {key = "|", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c|") },
  {key = "Z", mods = "SHIFT|CTRL", action = act.SendString("\x18@cZ") },
  {key = "Z", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cZ") },
  {key = "X", mods = "SHIFT|CTRL", action = act.SendString("\x18@cX") },
  {key = "X", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cX") },
  {key = "C", mods = "SHIFT|CTRL", action = act.SendString("\x18@cC") },
  {key = "C", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cC") },
  {key = "V", mods = "SHIFT|CTRL", action = act.SendString("\x18@cV") },
  {key = "V", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cV") },
  {key = "B", mods = "SHIFT|CTRL", action = act.SendString("\x18@cB") },
  {key = "B", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cB") },
  {key = "N", mods = "SHIFT|CTRL", action = act.SendString("\x18@cN") },
  {key = "N", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cN") },
  {key = "M", mods = "SHIFT|CTRL", action = act.SendString("\x18@cM") },
  {key = "M", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@cM") },
  {key = ",", mods = "CTRL", action = act.SendString("\x18@c,") },
  {key = "<", mods = "SHIFT|CTRL", action = act.SendString("\x18@c<") },
  {key = ",", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c,") },
  {key = "<", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c<") },
  {key = ".", mods = "CTRL", action = act.SendString("\x18@c.") },
  {key = ">", mods = "SHIFT|CTRL", action = act.SendString("\x18@c>") },
  {key = ".", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c.") },
  {key = ">", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c>") },
  {key = "/", mods = "CTRL", action = act.SendString("\x18@c/") },
  {key = "?", mods = "SHIFT|CTRL", action = act.SendString("\x18@c?") },
  {key = "/", mods = "CTRL|ALT", action = act.SendString("\x1b\x18@c/") },
  {key = "?", mods = "SHIFT|CTRL|ALT", action = act.SendString("\x1b\x18@c?") },
}

return config
