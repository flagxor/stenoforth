#define UNICODE

#include "stenoforth.h"
#include "windows.h"
#include "richedit.h"
#include <stdio.h>
#include <fcntl.h>
#include <io.h>

#define ID_TERMINAL 101
#define CLASS_NAME TEXT("ForthClass")
#define FONT_SIZE 24
#define KEY_BUFFER_SIZE 256

static cell_t key_buffer[KEY_BUFFER_SIZE];
static int key_writer = 0;
static int key_reader = 0;

static HWND hwndTerminal = 0;

static HFONT TerminalFont(void) {
  LOGFONT lf;
  memset(&lf, 0, sizeof(lf));
  lf.lfHeight = FONT_SIZE;
  lf.lfPitchAndFamily = FIXED_PITCH;
  return CreateFontIndirect(&lf);
}

void emit(cell_t ch) {
  if (ch == 8) {
    SendMessage(hwndTerminal, EM_SETSEL, GetWindowTextLength(hwndTerminal) - 1, -1);
    SendMessage(hwndTerminal, EM_REPLACESEL, 0, (LPARAM) TEXT(""));
    return;
  }
  if (ch == 12) {
    SendMessage(hwndTerminal, WM_SETTEXT, 0, (LPARAM) TEXT(""));
    return;
  }
  TCHAR text[2];
  text[0] = (TCHAR) ch;
  text[1] = 0;
  SendMessage(hwndTerminal, EM_SETSEL, -1, -1);
  SendMessage(hwndTerminal, EM_REPLACESEL, 0, (LPARAM) text);
}

cell_t qkey(void) {
  int old = key_reader;
  if (key_reader != key_writer) {
    key_reader = (key_reader + 1) % KEY_BUFFER_SIZE;
    return key_buffer[old];
  } else {
    return 0;
  }
}

void color(cell_t c) {
  (void) c;
}

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
  switch (uMsg) {
    case WM_CREATE:
      {
#if 1
        hwndTerminal = CreateWindowEx(
            0, TEXT("EDIT"), NULL,
            WS_CHILD | WS_VISIBLE | ES_LEFT | ES_MULTILINE | ES_AUTOVSCROLL | ES_READONLY,
            // | WS_VSCROLL | ES_AUTOVSCROLL,
            // | WS_BORDER | ES_READONLY,
            0, 0, 0, 0,
            hwnd,
            (HMENU) ID_TERMINAL,
            (HINSTANCE) GetWindowLongPtr(hwnd, GWLP_HINSTANCE),
            NULL);
#else
        LoadLibrary(TEXT("riched20.dll"));
        hwndTerminal = CreateWindowEx(0, MSFTEDIT_CLASS, TEXT("Type here"),
            WS_CHILD | WS_VISIBLE | ES_LEFT | ES_MULTILINE | ES_AUTOVSCROLL,
            0, 0, 0, 0,
            hwnd, (HMENU) ID_TERMINAL,
            (HINSTANCE) GetWindowLongPtr(hwnd, GWLP_HINSTANCE), NULL);
        SendMessage(hwndTerminal, EM_SETSEL, -2, -1);
        SendMessage(hwndTerminal, EM_SETBKGNDCOLOR, 0, RGB(0, 128, 0));
        CHARFORMAT cf;
        SendMessage(hwndTerminal, EM_GETCHARFORMAT, SCF_DEFAULT, (LPARAM) &cf);
        cf.cbSize = sizeof(cf);
        cf.crTextColor = RGB(255, 255, 0);
        cf.dwMask = CFM_COLOR;
        SendMessage(hwndTerminal, EM_SETCHARFORMAT, SCF_DEFAULT, (LPARAM) &cf);
#endif
        SendMessage(hwndTerminal, WM_SETFONT, (WPARAM) TerminalFont(), TRUE);
/*
        TCHAR lpszLatin[] =  TEXT("Lorem ipsum dolor sit amet, consectetur ");
        SendMessage(hwndTerminal, EM_REPLACESEL, 0, (LPARAM) lpszLatin);
*/
      }
      return 0;

    case WM_DESTROY:
      PostQuitMessage(0);
      return 0;

    case WM_CHAR:
      if ((key_writer + 1) % KEY_BUFFER_SIZE != key_reader) {
        key_buffer[key_writer] = (cell_t) wParam;
        key_writer = (key_writer + 1) % KEY_BUFFER_SIZE;
      }
      return 0;

    case WM_CTLCOLOREDIT:
    case WM_CTLCOLORSTATIC:
      {
        HDC hdc = (HDC) wParam;
        SetTextColor(hdc, RGB(255, 255, 255));
        SetBkColor(hdc, RGB(0, 0, 0));
      }
      return (LRESULT) GetStockObject(BLACK_BRUSH);

    case WM_PAINT:
      {
        PAINTSTRUCT ps;
        HDC hdc = BeginPaint(hwnd, &ps);
        //HBRUSH color = CreateSolidBrush(RGB(255, 255, 200));
        FillRect(hdc, &ps.rcPaint, (HBRUSH) GetStockObject(BLACK_BRUSH));
        //DeleteObject(color);
        //SelectObject(hdc, GetStockObject(BLACK_BRUSH));
        SetBkColor(hdc, RGB(0, 0, 0));
        SetTextColor(hdc, RGB(255, 255, 100));
        //TextOutA(hdc, 0, 0, (char *) boot, 20);
        EndPaint(hwnd, &ps);
      }
      return 0;

    case WM_SIZE:
      {
        WORD width = LOWORD(lParam);
        WORD height = HIWORD(lParam);
        //MoveWindow(hwndTerminal, 0, height * 5 / 6, width, height / 6, TRUE);
        MoveWindow(hwndTerminal, 0, 0, width, height, TRUE);
      }
      return 0;

    case WM_SETFOCUS:
      SetFocus(hwndTerminal);
      return 0;
  }
  return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

static void DebugConsole(void) {
#if DEBUG_MODE
  AllocConsole();

  HANDLE handle_out = GetStdHandle(STD_OUTPUT_HANDLE);
  int hCrt = _open_osfhandle((long) handle_out, _O_TEXT);
  FILE* hf_out = _fdopen(hCrt, "w");
  setvbuf(hf_out, NULL, _IONBF, 1);
  *stdout = *hf_out;

  HANDLE handle_err = GetStdHandle(STD_ERROR_HANDLE);
  hCrt = _open_osfhandle((long) handle_err, _O_TEXT);
  FILE* hf_err = _fdopen(hCrt, "w");
  setvbuf(hf_err, NULL, _IONBF, 1);
  *stderr = *hf_err;

  HANDLE handle_in = GetStdHandle(STD_INPUT_HANDLE);
  hCrt = _open_osfhandle((long) handle_in, _O_TEXT);
  FILE* hf_in = _fdopen(hCrt, "r");
  setvbuf(hf_in, NULL, _IONBF, 128);
  *stdin = *hf_in;
#endif
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrev, PSTR pCmdLine, int nCmdShow) {
  DebugConsole();

  char szFilename[MAX_PATH];
  GetModuleFileNameA(NULL, szFilename, MAX_PATH);
  cell_t *rp = vm_load(szFilename);

  WNDCLASS wc = { };
  wc.lpfnWndProc   = WindowProc;
  wc.hInstance     = hInstance;
  wc.lpszClassName = CLASS_NAME;
  wc.hIcon         = LoadIcon(hInstance, TEXT("IDI_MAIN_ICON"));
  RegisterClass(&wc);

  HWND hwnd = CreateWindowEx(
      0, CLASS_NAME, TEXT("stenoforth"), WS_OVERLAPPEDWINDOW,
      CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
      NULL, NULL, hInstance, NULL);
  if (hwnd == NULL) {
    return 0;
  }
  ShowWindow(hwnd, nCmdShow);

  MSG msg;
  for (;;) {
    while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
      if (msg.message == WM_QUIT) {
        return msg.wParam;
      }
      if (msg.message == WM_CHAR) {
        if ((key_writer + 1) % KEY_BUFFER_SIZE != key_reader) {
          key_buffer[key_writer] = (cell_t) msg.wParam;
          key_writer = (key_writer + 1) % KEY_BUFFER_SIZE;
        }
        continue;
      }
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
    if (hwndTerminal) {
      rp = vm(rp);
    }
    Sleep(16);
  }
  return 0;
}

