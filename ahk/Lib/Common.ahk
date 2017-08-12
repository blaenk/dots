Games := "Overwatch"
GamesPattern := "ahk_exe (" . Games  . ")\.exe$"

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
ShouldOverridePTT()
{
  global GamesPattern

  Process, Exist, GameOverlayUI.exe
  SteamOverlayPresent := ErrorLevel

  return WinExist("ahk_exe Discord(PTB)?.exe")
    and (SteamOverlayPresent or WinExist(GamesPattern))
}

; Enumerate audio sessions and call `Handler` function for each.
;
; Adapted from:
; https://gist.github.com/G33kDude/5b7ba418e685e52c3e6507e5c6972959
EnumerateAudioSessions(Handler)
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

  ; get the active window's PID
  WinGet, ActivePID, PID, A

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

    %Handler%(ActivePID, SPID, ISAV)

    ObjRelease(ISAV)
    ObjRelease(IASC2)
  }

  ObjRelease(IASE)
  ObjRelease(IASM2)
  ObjRelease(DAE)
}

MaximizeAudio(ActivePID, SPID, ISAV)
{
  ; Unmute first
  VA_ISimpleAudioVolume_SetMute(ISAV, False)

  ; Maximize the app's volume.
  VA_ISimpleAudioVolume_SetMasterVolume(ISAV, 1)
}

MuteAllExceptActive(ActivePID, SPID, ISAV)
{
  if (ActivePID != SPID) {
    ; Mute all apps that aren't the currently focused one.
    VA_ISimpleAudioVolume_SetMute(ISAV, True)
  } else {
    ; Ensure it's unmuted.
    VA_ISimpleAudioVolume_SetMute(ISAV, False)

    ; Maximize the currently focused one.
    VA_ISimpleAudioVolume_SetMasterVolume(ISAV, 1)
  }
}
