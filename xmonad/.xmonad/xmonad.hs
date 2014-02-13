import XMonad
import XMonad.Prompt
import qualified XMonad.StackSet as S

-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.WindowBringer
import XMonad.Actions.WithAll
import XMonad.Actions.RotSlaves
import qualified XMonad.Actions.FlexibleResize as Flex

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Place
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

-- layouts
import XMonad.Layout
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutHints
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders (noBorders, smartBorders)

-- misc
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xlib.Misc
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad

myLayout = layoutHints $ avoidStruts $ mkToggle (single FULL) $ smartBorders $
  tiled ||| (Mirror tiled) ||| Full ||| simplestFloat
  where
    tiled = smartSpacing 5 $ ResizableTall nmaster delta ratio []
    nmaster = 1
    ratio = 1/2
    delta = 5/100

myBar =  "xmobar && ~/.xmonad/scripts/volume.sh establish"

myGreen  = "#718c00"
myRed    = "#b32d47"
myYellow = "#eab700"
myBlue   = "#3a89c9"

myPP = xmobarPP
  { ppCurrent = xmobarColor "white" myBlue . pad
  , ppVisible = xmobarColor "white" myGreen . pad
  , ppHidden = xmobarColor "white" myRed . pad
  , ppHiddenNoWindows = xmobarColor "white" myRed . pad
  , ppWsSep = " "
  , ppSep = "  "
  , ppTitle = shorten 80
  , ppUrgent = clickableWS "black" myYellow
  , ppLayout = pad . layoutIcon
  }
  where clickableWS fg bg ws = "<action=xdotool key super+" ++ ws ++ ">" ++
          (xmobarColor fg bg . pad $ ws) ++ "</action>"

xmobarIcon icon = "<icon=/home/jorge/.xmonad/icons/" ++ icon ++ ".xbm/>"

layoutIcon layout =
  case layout of
    "Hinted SimplestFloat"         -> xmobarIcon "mouse"
    "Hinted SmartSpacing 5 ResizableTall"        -> xmobarIcon "layout_tall"
    "Hinted Mirror SmartSpacing 5 ResizableTall" -> xmobarIcon "layout_mirror_tall"
    "Hinted Full"                  -> xmobarIcon "layout_full"
    _                       -> layout

toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask .|. shiftMask, xK_b)

myWorkspaces = map show [1..5]

myXPConfig = defaultXPConfig
  { font = "xft:DejaVu Sans:pixelsize=14:antialias=true:hinting=true"
  , bgColor = "#2f2f2f"
  , fgColor = "#c5c8c6"
  , position = Bottom
  }

spawnWithSound cmd sound =
  spawn $ cmd ++ " && mplayer2 /usr/share/sounds/freedesktop/stereo/" ++ sound ++ ".oga"

yeganesh = "exe=`dmenu_path_c | yeganesh -- " ++ yeganeshOptions ++ "` && eval \"exec $exe\""
 where yeganeshOptions = L.intercalate " " $
         [ "-fn 'DejaVu Sans Mono-11'"
         , "-p '>'"
         , "-nf '#c5c8c6'"
         , "-nb '#2f2f2f'"
         ]

bringerOptions =
          [ "-fn", "DejaVu Sans Mono-11"
          , "-p", ">"
          , "-nf", "#c5c8c6"
          , "-nb", "#2f2f2f"
          , "-l", "48"
          ]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) =
  M.fromList $ [
    -- misc bindings
    ((modMask, xK_p), spawn yeganesh),
    ((modMask, xK_Print), spawnWithSound "scrot" "screen-capture"),
    ((modMask .|. shiftMask, xK_Print), spawnWithSound "sleep 0.2; scrot -s" "screen-capture"),

    -- volume keys
    ((0, xF86XK_AudioMute), spawn "~/.xmonad/scripts/volume.sh toggle"),
    ((0, xF86XK_AudioLowerVolume), spawn "~/.xmonad/scripts/volume.sh down"),
    ((0, xF86XK_AudioRaiseVolume), spawn "~/.xmonad/scripts/volume.sh up"),

    -- cycleWS: exclude other screens
    -- h/l: move left/right between workspaces
    -- u/i: move windows to left/right between workspaces
    ((modMask .|. shiftMask, xK_h), moveTo Prev (WSIs currentScreen)),
    ((modMask .|. shiftMask, xK_l), moveTo Next (WSIs currentScreen)),
    ((modMask .|. shiftMask, xK_u), shiftTo Prev (WSIs currentScreen)),
    ((modMask .|. shiftMask, xK_i), shiftTo Next (WSIs currentScreen)),
    ((modMask, xK_Tab), toggleWS),

    -- dynamic workspaces
    ((modMask .|. shiftMask, xK_a), addWorkspacePrompt myXPConfig), -- add
    ((modMask .|. shiftMask, xK_BackSpace), removeWorkspace), -- delete
    ((modMask .|. shiftMask, xK_v), selectWorkspace myXPConfig), -- switch to
    ((modMask, xK_m), withWorkspace myXPConfig (windows . S.shift)), -- move
    ((modMask .|. shiftMask, xK_r), renameWorkspace myXPConfig), -- rename
    ((modMask .|. shiftMask, xK_w), shiftNextScreen), -- move to other screen
    ((modMask .|. shiftMask, xK_e), shiftPrevScreen),

    -- resizable tile
    ((modMask, xK_u), sendMessage MirrorShrink),
    ((modMask, xK_i), sendMessage MirrorExpand),

    -- window bringer
    ((modMask, xK_g), gotoMenuArgs bringerOptions),
    ((modMask, xK_b), bringMenuArgs bringerOptions),

    -- cycle all windows
    ((modMask .|. shiftMask .|. controlMask, xK_space), rotAllDown),

    -- focus urgent
    ((modMask .|. shiftMask, xK_u), focusUrgent),

    -- jump to full
    ((modMask, xK_f), sendMessage $ Toggle FULL),

    -- float all quickly
    ((modMask .|. shiftMask, xK_t), sinkAll)
  ] ++ [
    -- proper ordering of screens
    -- this is broken on tiled layouts, just floats it and puts it back?
    ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
     | (key, sc) <- zip [xK_e, xK_w] [0..]
      , (f, m) <- [(S.view, 0), (S.shift, shiftMask)]
  ] ++ [
    ((modMask .|. controlMask, k), windows $ swapWithCurrent i)
     | (i, k) <- zip (workspaces conf) [xK_1 ..]
  ]

currentScreen :: X (WindowSpace -> Bool)
currentScreen = do
  ws <- gets windowset
  let visibles = map (S.tag . S.workspace) $ S.visible ws
  return $ not . (`elem` visibles) . S.tag

-- button8/9 back/forward
myMouse :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouse conf@(XConfig {XMonad.modMask = modMask, XMonad.mouseBindings = defaultBinds}) =
  M.fromList [
    -- flexible resizing
    ((modMask, button3), \w -> focus w >> Flex.mouseResizeWindow w >> windows S.shiftMaster),
    -- repeat now with alt/mod1mask: more natural window movements (as in other DE/WMs)
    ((mod1Mask, button1), \w -> focus w >> mouseMoveWindow w >> windows S.shiftMaster),
    ((mod1Mask, button2), windows . (S.shiftMaster .) . S.focusWindow),
    ((mod1Mask, button3), \w -> focus w >> Flex.mouseResizeWindow w >> windows S.shiftMaster)
    ]

mergedKeys layout = myKeys layout `M.union` keys defaultConfig layout
mergedMouse layout = myMouse layout `M.union`mouseBindings defaultConfig layout

myStartupHook = do
  setWMName "LG3D"

myEventHook = do
  composeAll
    [ handleEventHook defaultConfig
    , hintsEventHook
    , fullscreenEventHook
    ]

myManageHook = placeHook simpleSmart <+> manageDocks <+> composeOne [
  -- withDisplay $ \d -> io $ setWindowBorderWidth d w 0
  -- appName =? "plugin-container.exe" -?> doFullFloat, -- (doF S.focusDown <+> doFullFloat),
  isFullscreen -?> (doF S.focusDown <+> doFullFloat),
  className =? "Steam" -?> doFloat, -- steam
  className =? "Wine" -?> doFloat, -- steam
  className =? "mplayer2" -?> doFloat -- mplayer
  ]

isFloating :: Query Bool
isFloating = ask >>= \w -> liftX . gets $ M.member w . S.floating . windowset

-- configuration
myConfig = defaultConfig {
    terminal = "urxvt",
    modMask = mod4Mask,
    keys = mergedKeys,
    mouseBindings = mergedMouse,
    clickJustFocuses = False,
    focusFollowsMouse = False,
    borderWidth = 3,
    normalBorderColor = "#181512",
    focusedBorderColor = "#b32d47",
    workspaces = myWorkspaces,
    logHook = dynamicLog,
    startupHook = myStartupHook,
    handleEventHook = myEventHook,
    layoutHook = myLayout,
    manageHook = myManageHook
  }

main = xmonad =<< statusBar myBar myPP toggleStrutsKey (withUrgencyHook NoUrgencyHook myConfig)

