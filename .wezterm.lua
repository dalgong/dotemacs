local wezterm = require 'wezterm'
local act = wezterm.action

local config = wezterm.config_builder()
-- config.color_scheme = 'Tokyo Night Day'
config.color_scheme = 'Chalk (light) (terminal.sexy)'
config.font = wezterm.font({ family = 'GoMono Nerd Font Mono' })
config.font_size = 12
local TITLEBAR_COLOR = '#333333'
config.window_frame = {
  font = wezterm.font { family = 'GoMono Nerd Font Mono', weight = 'Bold' },
  font_size = 13.0,
  active_titlebar_bg = TITLEBAR_COLOR,
  inactive_titlebar_bg = TITLEBAR_COLOR,
}

config.window_background_opacity = 0.85
config.native_macos_fullscreen_mode = true
config.macos_window_background_blur = 30
config.window_decorations = 'INTEGRATED_BUTTONS|RESIZE'
config.window_padding = { left = 2, right = 2, top = 2, bottom = 0 }
config.hide_tab_bar_if_only_one_tab = true
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
}

return config
