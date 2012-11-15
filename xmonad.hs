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
import System.IO

xmobarPPOptions :: Handle -> PP
xmobarPPOptions handle = xmobarPP { ppOutput = hPutStrLn handle
                                  , ppTitle = xmobarColor "green" "" . shorten 50
                                  , ppHidden = xmobarColor "lightgrey" ""
                                  , ppHiddenNoWindows = xmobarColor "grey" ""
                                  }
main = do
    runProcessWithInput "xrandr" ["--auto", "--output", "VGA1", "--left-of", "HDMI1"] ""
    xmproc <- spawnPipe "xmobar --screen=0"
    xmonad $ defaultConfig
        { workspaces = [ "1:chrome-work"
                       , "2:chrome-home"
                       , "3:vim"
                       , "4:dotfiles"
                       , "5:support-workspace"
                       , "6"
                       , "7"
                       , "8"
                       , "9"
                       ]
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP $ xmobarPPOptions xmproc
        }
