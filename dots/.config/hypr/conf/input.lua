-- Input settings
-- See https://wiki.hypr.land/Configuring/Variables/#input

hl.config({
    input = {
        kb_layout  = "us",
        kb_variant = "intl",
        kb_model   = "",
        kb_options = "ctrl:nocaps",
        kb_rules   = "",

        follow_mouse = 0,
        sensitivity  = 0,
        scroll_factor = 0.4,

        touchpad = {
            natural_scroll = false,
        },
    },
})

hl.gesture({
    fingers    = 3,
    direction  = "horizontal",
    action     = "workspace",
})

hl.device({
    name        = "epic-mouse-v1",
    sensitivity = -0.5,
})
