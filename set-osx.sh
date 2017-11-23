#!/usr/bin/env bash

sudo -v

# menubar
defaults write NSGlobalDomain AppleEnableMenuBarTransparency -bool false

# Disable “natural” (Lion-style) scrolling
defaults write NSGlobalDomain com.apple.swipescrolldirection -bool false

# Screenshot
defaults write com.apple.screencapture type -string "png"
defaults write com.apple.screencapture disable-shadow -bool true

# Enable subpixel font rendering on non-Apple LCDs
defaults write NSGlobalDomain AppleFontSmoothing -int 2

# Avoid creating .DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Show the ~/Library folder
chflags nohidden ~/Library

# Set the icon size of Dock items to blow pixels
defaults write com.apple.dock tilesize -int 48

# Use plain text mode for new TextEdit documents
defaults write com.apple.TextEdit RichText -int 0

# Open and save files as UTF-8 in TextEdit
defaults write com.apple.TextEdit PlainTextEncoding -int 4
defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4

# Prevent Photos from opening automatically when devices are plugged in
defaults -currentHost write com.apple.ImageCapture disableHotPlug -bool true

# Debug Menu
defaults write com.apple.DiskUtility DUDebugMenuEnabled -bool true
defaults write com.apple.DiskUtility advanced-image-options -bool true
defaults write com.apple.addressbook ABShowDebugMenu -bool true
defaults write com.apple.Safari IncludeInternalDebugMenu -bool true
defaults write com.apple.appstore ShowDebugMenu -bool true
defaults write com.apple.appstore WebKitDeveloperExtras -bool true
