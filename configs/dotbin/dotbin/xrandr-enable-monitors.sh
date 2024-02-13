#!/bin/sh

# Check for HDMI-A-0 and DisplayPort-1 availability
MON_0_AVAILABLE=$(xrandr | grep 'DisplayPort-0 connected')
MON_1_AVAILABLE=$(xrandr | grep 'DisplayPort-1 connected')

if [ -n "$MON_0_AVAILABLE" ] && [ -n "$MON_1_AVAILABLE" ]; then
    # HDMI-A-0 and DisplayPort-1 are available, set them up and disable eDP
    xrandr --output eDP --off \
           --output DisplayPort-0 --mode 3840x2160 --pos 3840x0 --rotate normal \
           --output DisplayPort-1 --mode 3840x2160 --pos 0x0 --rotate normal \
           --output DisplayPort-2 --off \
           --output DisplayPort-3 --off \
           --output DisplayPort-4 --off \
           --output DisplayPort-5 --off
else
    # HDMI-A-0 or DisplayPort-1 not available, enable eDP and disable others
    xrandr --output eDP --auto \
           --output DisplayPort-0 --off \
           --output DisplayPort-1 --off \
           --output DisplayPort-2 --off \
           --output DisplayPort-3 --off \
           --output DisplayPort-4 --off \
           --output DisplayPort-5 --off
fi
