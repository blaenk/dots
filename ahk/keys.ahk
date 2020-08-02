#NoEnv
; #Warn
SendMode Input
SetWorkingDir %A_ScriptDir%
SetTitleMatchMode RegEx

#Include <VA>
#include <Common>

Menu, Tray, Tip, Keys
Menu, Tray, Icon , icon.ico,, 1

; Emit backtick (`) when the side mouse buttons are pressed (forward/backward).
;
; GFE currently only supports one push-to-talk keybind, creating
; an issue if we want to use PTT but we have separate PTT keys for both in-game
; and Discord. To resolve this, we set GFE's PTT to backtick, then have
; mouse4 and mouse5 also emit backtick so that GFE picks it up.
;
; It's important that this remap employs certain AHK behaviors:
;   - Pass-through the original key (accomplished via ~)
;   - Work regardless of any modifier (e.g. Ctrl) that may be pressed concurrently
;     (accomplished via *)
;   - Not reset the state of any modifier that may already be pressed concurrently
;     (accomplished via the {Blind} option for Send, which is implicit in a remap)
#If ShouldOverridePTT()
~*XButton1::`
~*XButton2::`

; Reset conditional directive
#If

; Toggle that swaps the default recording device to Setero Mix and back.
^!XButton2::
  Toggle := !Toggle
  If Toggle {
    VA_SetDefaultEndpoint("Stereo Mix", 0)
    VA_SetDefaultEndpoint("Stereo Mix", 1)
    VA_SetDefaultEndpoint("Stereo Mix", 2)
  } Else {
    VA_SetDefaultEndpoint("Microphone", 0)
    VA_SetDefaultEndpoint("Microphone", 1)
    VA_SetDefaultEndpoint("Microphone", 2)
  }
  Return

; Media keys 'home row' are u/i/o
#u::Volume_Down
!#u::Media_Prev
; ~MButton & WheelDown::Volume_Down

#i::Volume_Mute
!#i::Media_Play_Pause
; ~MButton & RButton::Volume_Mute

#o::Volume_Up
!#o::Media_Next
; ~MButton & WheelUp::Volume_Up

; Volume down-step for the active application.
+#u::
  EnumerateAudioSessions(Func("ActiveStepDown"))
  Return

; Mute all audio sessions except for the one associated with the current application.
+#i::
  EnumerateAudioSessions(Func("MuteAllExceptActive"))
  Return

; Maximize all per-application volume levels.
+#o::
  EnumerateAudioSessions(Func("MaximizeAudio"))
  Return

; Allow scrolling the AHK taskbar icon to control volume.
; NOTE: This is triggered by global wheel movements, not just those over the task bar icon.
; $WheelUp:: 
; $WheelDown:: 
;   MouseGetPos VarX, VarY, CurrentWindow
;   WinGetClass Class, ahk_id %CurrentWindow%

;   Vol := SubStr(A_ThisHotkey, 7)
;   Key := SubStr(A_ThisHotkey, 2)

;   If Class = Shell_TrayWnd
;     Send {Volume_%Vol%}
;   Else
;     Send {%Key%}
;   Return
