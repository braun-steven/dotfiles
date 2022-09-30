#!/usr/bin/env sh

# Instantly show dock with mouse-over
defaults write com.apple.dock "autohide-delay" -float "0" && killall Dock
