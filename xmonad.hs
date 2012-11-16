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
import XMonad.Layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.MagicFocus
import System.IO

xmobarPPOptions :: Handle -> PP
xmobarPPOptions handle = xmobarPP { ppOutput = hPutStrLn handle
                                  , ppTitle = xmobarColor "green" "" . shorten 50
                                  , ppHidden = xmobarColor "lightgrey" ""
                                  , ppHiddenNoWindows = xmobarColor "grey" ""
                                  }

layout = avoidStruts
    (   tallLayout
    ||| Mirror (tallLayout)
    ||| magicFocus (tallLayout)
    ||| Full
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
                       , "3:vim"
                       , "4:dotfiles"
                       , "5:local-rjm"
                       , "6"
                       , "7"
                       , "8"
                       , "9"
                       ]
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = layout
        , logHook = (dynamicLogWithPP $ xmobarPPOptions xmprocLeft) >> (dynamicLogWithPP $ xmobarPPOptions xmprocRight)
        , focusFollowsMouse = False
        }
