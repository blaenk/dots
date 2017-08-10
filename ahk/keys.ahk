#NoEnv
; #Warn
SendMode Input
SetWorkingDir %A_ScriptDir%
SetTitleMatchMode RegEx

#Include <VA>

; Determine whether the side buttons on the mouse (back and forward buttons)
; should also emit the backtick key. This is used by the remappings specified
; below. The predicate is encapsulated in a function to make it reusable.
;
; The predicate simply checks if Discord and any game is running.
; Most running games can be detected by the presence of the Steam overlay,
; denoted by the existence of the GameOverlayUI.exe process. Other games
; such as Overwatch are added manually.
;
; The match mode is set to RegEx in order to specify multiple needles per
; call to WinExist() so as to minimize its impact.

Games := "Overwatch"
GamesPattern := "ahk_exe (" . Games  . ")\.exe$"

ShouldOverridePTT()
{
  global GamesPattern

  Process, Exist, GameOverlayUI.exe
  SteamOverlayPresent := ErrorLevel

  return WinExist("ahk_exe Discord(PTB)?.exe")
    and (SteamOverlayPresent or WinExist(GamesPattern))
}

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

#If ShouldOverridePTT()
~*XButton2::`

VA_ISimpleAudioVolume_SetMasterVolume(this, ByRef fLevel, GuidEventContext="")
{
  return DllCall(NumGet(NumGet(this+0)+3*A_PtrSize)
                , "ptr", this
                , "float", fLevel
                , "ptr", VA_GUID(GuidEventContext))
}

; Maximize every audio session's volume.
;
; Adapted from:
; https://gist.github.com/G33kDude/5b7ba418e685e52c3e6507e5c6972959
MaxAllAppVolumes()
{
  static IID_IASM2 := "{77AA99A0-1BD6-484F-8BC7-2C654C9A9B6F}"
       , IID_IASC2 := "{bfb7ff88-7239-4fc9-8fa2-07c950be9c6d}"
       , IID_ISAV  := "{87CE5498-68D6-44E5-9215-6DA47EF883D8}"

  ; GetDefaultAudioEndpoint
  DAE := VA_GetDevice()

  ; activate the session manager
  VA_IMMDevice_Activate(DAE, IID_IASM2, 0, 0, IASM2)

  ; enumerate sessions for on this device
  VA_IAudioSessionManager2_GetSessionEnumerator(IASM2, IASE)
  VA_IAudioSessionEnumerator_GetCount(IASE, Count)

  ; search for an audio session with the required name
  Loop, % Count
  {
    ; Get the IAudioSessionControl object
    VA_IAudioSessionEnumerator_GetSession(IASE, A_Index-1, IASC)

    ; Query the IAudioSessionControl for an IAudioSessionControl2 object
    IASC2 := ComObjQuery(IASC, IID_IASC2)
    ObjRelease(IASC)

    ; Get the session's process ID
    VA_IAudioSessionControl2_GetProcessID(IASC2, SPID)

    ; Query for the ISimpleAudioVolume
    ISAV := ComObjQuery(IASC2, IID_ISAV)

    ; Maximize the app's volume.
    VA_ISimpleAudioVolume_SetMasterVolume(ISAV, 1)

    ObjRelease(ISAV)
    ObjRelease(IASC2)
  }

  ObjRelease(IASE)
  ObjRelease(IASM2)
  ObjRelease(DAE)
}

; Maximize all per-application volume levels.
;
; This iterates through each of the existing audio sessions and maximizes their volume.
;
; It's purpose is to "reset" any drift that may have occured through user-changed
; per-application volumes, or through round-off error incurred by attenuation
; functionality in various VoIP programs such as Discord.
#Volume_Up::
  MaxAllAppVolumes()
  Return

; Mute all audio sessions except for the one associated with the current application,
; if any exists.
;
; Acts as a toggle.
;
; NOTE
; is that possible? would it have to be a per-process toggle?
; how would it interact with switching apps?
;
; TODO
; * get active window PID: WinGet, active_pid, PID, A
; * enumerate audio sessions
; * mute those where SPID != PID
; * max the one where SPID == PID?

; #Volume_Mute::

; Reset per-application volume levels.
;
; This clears out the saved per-application volume levels from the registry,
; then resets the windows audio sound-system, forcing each application to assume
; a new default volume based on the master device volume.
;
; NOTE
; This has the effect of destroying any existing audio sessions, usually requiring
; them to be restarted for sound to continue working in them.
;
; NOTE
; This needs admin permissions.
;
; NOTE
; This is adapted from a batch script:
; https://gist.github.com/blaenk/283c218e9c721ca8932dd1efd9f0c2a1

#If 0
^+!Volume_Mute::
  RunWait, %comspec% /c net stop Audiosrv
  RunWait, %comspec% /c net stop AudioEndpointBuilder

  RegDelete, HKCU\Software\Microsoft\Internet Explorer\LowRegistry\Audio\PolicyConfig\PropertyStore

  RegWrite, REG_SZ, HKCU\Software\Microsoft\Internet Explorer\LowRegistry\Audio\PolicyConfig\PropertyStore

  RunWait, %comspec% /c net start Audiosrv
  Return
