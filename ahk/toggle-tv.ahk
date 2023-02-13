#NoEnv
#SingleInstance, Force
SendMode, Input
SetBatchLines, -1
SetWorkingDir, %A_ScriptDir%

#Include <VA>
#include <Common>

Muted := VA_GetMasterVolume("", "SPDIF In") == 0

UpdateMenu()
{
  global Muted

  If (Muted)
  {
    Menu, Tray, Tip, Unmute TV
    Menu, Tray, Icon , tv.ico,, 1
  }
  Else
  {
    Menu, Tray, Tip, Mute TV
    Menu, Tray, Icon , pc.ico,, 1
  }
}

ToggleTV()
{
  global Muted
  Muted := !Muted

  VA_SetMasterVolume(Muted ? 0 : 100, "", "SPDIF In")

  UpdateMenu()
}

UpdateMenu()

AHK_NOTIFYICON(wParam, lParam, uMsg, hWnd)
{
  ; global Muted
  ; global IsOver

  ; If (lParam = 0x200)
  ; {
  ;   IsOver := TRUE
  ; }

  If (lParam = 0x201) ;WM_LBUTTONDOWN := 0x201
  {
    ToggleTV()
  }
}

OnMessage(0x404, "AHK_NOTIFYICON")

^Volume_Up::
^+!M::
  ToggleTV()
Return

; Menu, Tray, NoStandard

; TrayHwnd := MenuGetHandle("Tray")

; Muted := VA_GetMasterMute("SPDIF In")
; IsOver := FALSE

; SoundSet, +2, Digital-In

; OnMessage(0x020A, "WM_MOUSEWHEEL")
; Return

; WM_MOUSEWHEEL(wParam, lParam, msg) {
;    VA_SetMasterVolume()
; }

; OnMessage(0x020A, "WM_MOUSEWHEEL")
; Return
; ; Return
; ; GuiClose:
; ; ExitApp

; WM_MOUSEWHEEL(wParam, lParam, msg) {
;   ToolTip, % "wParam loword: " . wParam & 0xffff . "`n"
;   . "wParam hiword: " . (wParam >> 31 ? -~wParam : wParam) >> 16
; }

; ~WheelUp::
; ~WheelDown::
;   MouseGetPos,,, Hwnd
;   if (Hwnd = TrayHwnd)
;     MsgBox, "Title"
; Return

; #If MouseIsOver("ahk_class Shell_TrayWnd")
; WheelUp::
;   SoundSet, +2, Digital-In
; Return

; WheelDown::
;   SoundSet, -2, Digital-In
; Return
