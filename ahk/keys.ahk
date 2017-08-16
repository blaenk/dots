#NoEnv
; #Warn
SendMode Input
SetWorkingDir %A_ScriptDir%
SetTitleMatchMode RegEx

#Include <VA>
#include <Common>

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

; Maximize all per-application volume levels.
#Volume_Up::
  EnumerateAudioSessions(Func("MaximizeAudio"))
  Return

; Mute all audio sessions except for the one associated with the current application.
#Volume_Mute::
  EnumerateAudioSessions(Func("MuteAllExceptActive"))
  Return

; Rewire Everything's Ctrl+Shift+Alt+Space to Win+Space
#Space::
  SendInput ^+!{Space}
  Return

; Rewire Wox' Ctrl+Shift+Alt+/ to Win+/
#/::
  SendInput ^+!{/}
  Return