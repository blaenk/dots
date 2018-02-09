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

; Maximize all per-application volume levels.
#Volume_Up::
  EnumerateAudioSessions(Func("MaximizeAudio"))
  Return

; Mute all audio sessions except for the one associated with the current application.
#Volume_Mute::
  EnumerateAudioSessions(Func("MuteAllExceptActive"))
  Return

!Volume_Mute::Media_Play_Pause
!Volume_Down::Media_Prev
!Volume_Up::Media_Next
