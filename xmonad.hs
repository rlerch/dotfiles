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
import System.IO

surround :: String -> String -> String
surround extra text = extra ++ text ++ extra

yellow = xmobarColor "yellow" ""

xmobarPPOptions :: Handle -> PP
xmobarPPOptions handle = xmobarPP { ppOutput = hPutStrLn handle
                                  , ppTitle = xmobarColor "green" "" . shorten 50
                                  , ppCurrent = yellow . (surround "**")
                                  , ppVisible = yellow
                                  , ppHidden = xmobarColor "grey" ""
                                  , ppHiddenNoWindows = xmobarColor "blue" ""
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

main = do
    runProcessWithInput "xrandr" ["--auto", "--output", "VGA1", "--left-of", "HDMI1"] ""
    xmprocLeft <- spawnPipe "xmobar --screen=0"
    xmprocRight <- spawnPipe "xmobar --screen=1"
    xmonad $ defaultConfig
        { workspaces = [ "1:chrome-work"
                       , "2:chrome-home"
                       , "3:vm"
                       , "4"
                       , "5"
                       , "6"
                       , "7"
                       , "8:time-tracking"
                       , "9:dotfiles"
                       ]
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = layout
        , logHook = (dynamicLogWithPP $ xmobarPPOptions xmprocLeft) >> (dynamicLogWithPP $ xmobarPPOptions xmprocRight)
        , focusFollowsMouse = False
        }
        `additionalKeysP`
        [ ("M-S-s", spawn "gksudo shutdown -P now")
        , ("M-v", spawn "gnome-terminal -x r vm")
        ]
