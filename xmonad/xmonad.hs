-- required packages:
-- xmonad
-- xmonad-contrib
-- xmobar

-- https://wiki.archlinux.org/index.php/Xmonad#Using_xmobar_with_xmonad
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen%27s_Configuration#Configuring_xmonad_to_use_xmobar

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe,runProcessWithInput)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Actions.SpawnOn(spawnOn)
import System.IO

yellow = xmobarColor "#F7F383" ""

xmobarPPOptions :: Handle -> PP
xmobarPPOptions handle = xmobarPP { ppOutput = hPutStrLn handle
                                  , ppTitle = xmobarColor "#7DF58F" "" . shorten 25
                                  , ppCurrent = yellow . (wrap  "**" "**")
                                  , ppVisible = yellow
                                  , ppHidden = xmobarColor "grey" ""
                                  , ppHiddenNoWindows = xmobarColor "#65BBF7" ""
                                  }

layout = avoidStruts
    (   tallLayout
    ||| Mirror (tallLayout)
    ||| renamed [Replace "Maximized"] Full
    )
    ||| fullLayout
    where
        tallLayout = Tall 1 (3/100) (1/2)
        fullLayout = noBorders $ fullscreenFull Full

myWorkspaces = [ "1:chrome(work)"
               , "2:chrome(home)"
               , "3:planning"
               , "4:test space"
               , "5:"
               , "6:"
               , "7:vmdev"
               , "8:vim"
               , "9:dotfiles"
               ]

pianobarCmd :: String -> String
pianobarCmd cmd = "pianobar-ctl '" ++ cmd ++ "'"

main = do
    dbproc <- spawnPipe "dropbox start"
    runProcessWithInput "xrandr" ["--auto", "--output", "VGA1", "--left-of", "HDMI1"] ""
    xmprocLeft <- spawnPipe "xmobar --screen=0"
    xmprocRight <- spawnPipe "xmobar --screen=1"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = layout
        , focusFollowsMouse = False

        -- BEGIN work-branch-specific config
        , logHook = (dynamicLogWithPP $ xmobarPPOptions xmprocLeft)
                 >> (dynamicLogWithPP $ xmobarPPOptions xmprocRight)
        , workspaces = myWorkspaces
        -- END work-branch-specific config

        }
        `additionalKeysP`
        [ ("M-S-s", spawn "gksudo shutdown -P now")

        -- BEGIN work branch shortcuts
        , ("M-s", spawn "gnome-screensaver-command -l")
        , ("M-v", spawn "gnome-terminal -x ssh rjmdash@vmdev")
        , ("M-S-v", spawn "gnome-terminal -x ssh rjmdash@vmdev -t 'cd rjm && vim'")
        , ("M-<KP_Enter>",    spawn $ pianobarCmd "start")
        , ("M-<KP_Delete>",   spawn $ pianobarCmd "q")
        , ("M-<KP_Insert>",   spawn $ pianobarCmd "p") -- keypad 0 = pause
        , ("M-<KP_Right>",    spawn $ pianobarCmd "n") -- keypad 6 = next
        , ("M-<KP_Up>",       spawn $ pianobarCmd "+") -- keypad 8 = thumbs up
        , ("M-<KP_Down>",     spawn $ pianobarCmd "-") -- keypad 2 = thumbs down
        , ("M-<KP_Add>",      spawn $ pianobarCmd ")") -- keypad + = increase volume
        , ("M-<KP_Subtract>", spawn $ pianobarCmd "(") -- keypad - = decrease volume
        -- END work branch shortcuts

        ]
