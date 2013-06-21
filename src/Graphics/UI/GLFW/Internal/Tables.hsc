#include <GLFW/glfw3.h>

module Graphics.UI.GLFW.Internal.Tables where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Foreign.C.Types (CInt, CUChar)

import Graphics.UI.GLFW.Types

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

table_C_CInt_Bool :: [(CInt, Bool)]
table_C_CInt_Bool =
    [ ( (#const GL_FALSE), False )
    , ( (#const GL_TRUE),  True  )
    ]

table_C_CInt_CursorInputMode :: [(CInt, CursorInputMode)]
table_C_CInt_CursorInputMode =
    [ ( (#const GLFW_CURSOR_NORMAL),   CursorNormal   )
    , ( (#const GLFW_CURSOR_HIDDEN),   CursorHidden   )
    , ( (#const GLFW_CURSOR_DISABLED), CursorDisabled )
    ]

table_C_CInt_KeysInputMode :: [(CInt, KeysInputMode)]
table_C_CInt_KeysInputMode =
    [ ( (#const GL_TRUE),  KeysSticky )
    , ( (#const GL_FALSE), KeysNormal )
    ]

table_C_CInt_MouseButtonsInputMode :: [(CInt, MouseButtonsInputMode)]
table_C_CInt_MouseButtonsInputMode =
    [ ( (#const GL_TRUE),  MouseButtonsSticky )
    , ( (#const GL_FALSE), MouseButtonsNormal )
    ]

table_C_CInt_ClientApi :: [(CInt, ClientApi)]
table_C_CInt_ClientApi =
    [ ( (#const GLFW_OPENGL_API),    OpenglApi   )
    , ( (#const GLFW_OPENGL_ES_API), OpenglEsApi )
    ]

table_C_CInt_ContextRobustness :: [(CInt, ContextRobustness)]
table_C_CInt_ContextRobustness =
    [ ( (#const GLFW_NO_ROBUSTNESS),         NoRobustness        )
    , ( (#const GLFW_NO_RESET_NOTIFICATION), NoResetNotification )
    , ( (#const GLFW_LOSE_CONTEXT_ON_RESET), LoseContextOnReset  )
    ]

table_C_CInt_OpenglProfile :: [(CInt, OpenglProfile)]
table_C_CInt_OpenglProfile =
    [ ( (#const GLFW_OPENGL_ANY_PROFILE),    OpenglAnyProfile    )
    , ( (#const GLFW_OPENGL_COMPAT_PROFILE), OpenglCompatProfile )
    , ( (#const GLFW_OPENGL_CORE_PROFILE),   OpenglCoreProfile   )
    ]

table_C_CInt_Error :: [(CInt, Error)]
table_C_CInt_Error =
    [ ( (#const GLFW_NOT_INITIALIZED),     NotInitialized     )
    , ( (#const GLFW_NO_CURRENT_CONTEXT),  NoCurrentContext   )
    , ( (#const GLFW_INVALID_ENUM),        InvalidEnum        )
    , ( (#const GLFW_INVALID_VALUE),       InvalidValue       )
    , ( (#const GLFW_OUT_OF_MEMORY),       OutOfMemory        )
    , ( (#const GLFW_API_UNAVAILABLE),     ApiUnavailable     )
    , ( (#const GLFW_VERSION_UNAVAILABLE), VersionUnavailable )
    , ( (#const GLFW_PLATFORM_ERROR),      PlatformError      )
    , ( (#const GLFW_FORMAT_UNAVAILABLE),  FormatUnavailable  )
    ]

table_C_CInt_CursorAction :: [(CInt, CursorAction)]
table_C_CInt_CursorAction =
    [ ( (#const GL_TRUE),  CursorEnter )
    , ( (#const GL_FALSE), CursorLeave )
    ]

table_C_CInt_FocusAction :: [(CInt, FocusAction)]
table_C_CInt_FocusAction =
    [ ( (#const GL_TRUE),  Focus   )
    , ( (#const GL_FALSE), Defocus )
    ]

table_C_CInt_IconifyAction :: [(CInt, IconifyAction)]
table_C_CInt_IconifyAction =
    [ ( (#const GL_TRUE),  Iconify )
    , ( (#const GL_FALSE), Restore )
    ]

table_C_CInt_KeyAction :: [(CInt, KeyAction)]
table_C_CInt_KeyAction =
    [ ( (#const GLFW_PRESS),   KeyPress   )
    , ( (#const GLFW_RELEASE), KeyRelease )
    , ( (#const GLFW_REPEAT),  KeyRepeat  )
    ]

table_C_CUChar_JoystickButtonAction :: [(CUChar, JoystickButtonAction)]
table_C_CUChar_JoystickButtonAction =
    [ ( (#const GLFW_PRESS),   JoystickButtonPress   )
    , ( (#const GLFW_RELEASE), JoystickButtonRelease )
    ]

table_C_CInt_MouseButtonAction :: [(CInt, MouseButtonAction)]
table_C_CInt_MouseButtonAction =
    [ ( (#const GLFW_PRESS),   MouseButtonPress   )
    , ( (#const GLFW_RELEASE), MouseButtonRelease )
    ]

table_C_CInt_MonitorAction :: [(CInt, MonitorAction)]
table_C_CInt_MonitorAction =
    [ ( (#const GL_TRUE),  Connect    )
    , ( (#const GL_FALSE), Disconnect )
    ]

table_C_CInt_Key :: [(CInt, Key)]
table_C_CInt_Key =
   [ ( (#const GLFW_KEY_UNKNOWN),       KeyUnknown      )
   , ( (#const GLFW_KEY_SPACE),         KeySpace        )
   , ( (#const GLFW_KEY_APOSTROPHE),    KeyApostrophe   )
   , ( (#const GLFW_KEY_COMMA),         KeyComma        )
   , ( (#const GLFW_KEY_MINUS),         KeyMinus        )
   , ( (#const GLFW_KEY_PERIOD),        KeyPeriod       )
   , ( (#const GLFW_KEY_SLASH),         KeySlash        )
   , ( (#const GLFW_KEY_0),             Key0            )
   , ( (#const GLFW_KEY_1),             Key1            )
   , ( (#const GLFW_KEY_2),             Key2            )
   , ( (#const GLFW_KEY_3),             Key3            )
   , ( (#const GLFW_KEY_4),             Key4            )
   , ( (#const GLFW_KEY_5),             Key5            )
   , ( (#const GLFW_KEY_6),             Key6            )
   , ( (#const GLFW_KEY_7),             Key7            )
   , ( (#const GLFW_KEY_8),             Key8            )
   , ( (#const GLFW_KEY_9),             Key9            )
   , ( (#const GLFW_KEY_SEMICOLON),     KeySemicolon    )
   , ( (#const GLFW_KEY_EQUAL),         KeyEqual        )
   , ( (#const GLFW_KEY_A),             KeyA            )
   , ( (#const GLFW_KEY_B),             KeyB            )
   , ( (#const GLFW_KEY_C),             KeyC            )
   , ( (#const GLFW_KEY_D),             KeyD            )
   , ( (#const GLFW_KEY_E),             KeyE            )
   , ( (#const GLFW_KEY_F),             KeyF            )
   , ( (#const GLFW_KEY_G),             KeyG            )
   , ( (#const GLFW_KEY_H),             KeyH            )
   , ( (#const GLFW_KEY_I),             KeyI            )
   , ( (#const GLFW_KEY_J),             KeyJ            )
   , ( (#const GLFW_KEY_K),             KeyK            )
   , ( (#const GLFW_KEY_L),             KeyL            )
   , ( (#const GLFW_KEY_M),             KeyM            )
   , ( (#const GLFW_KEY_N),             KeyN            )
   , ( (#const GLFW_KEY_O),             KeyO            )
   , ( (#const GLFW_KEY_P),             KeyP            )
   , ( (#const GLFW_KEY_Q),             KeyQ            )
   , ( (#const GLFW_KEY_R),             KeyR            )
   , ( (#const GLFW_KEY_S),             KeyS            )
   , ( (#const GLFW_KEY_T),             KeyT            )
   , ( (#const GLFW_KEY_U),             KeyU            )
   , ( (#const GLFW_KEY_V),             KeyV            )
   , ( (#const GLFW_KEY_W),             KeyW            )
   , ( (#const GLFW_KEY_X),             KeyX            )
   , ( (#const GLFW_KEY_Y),             KeyY            )
   , ( (#const GLFW_KEY_Z),             KeyZ            )
   , ( (#const GLFW_KEY_LEFT_BRACKET),  KeyLeftBracket  )
   , ( (#const GLFW_KEY_BACKSLASH),     KeyBackslash    )
   , ( (#const GLFW_KEY_RIGHT_BRACKET), KeyRightBracket )
   , ( (#const GLFW_KEY_GRAVE_ACCENT),  KeyGraveAccent  )
   , ( (#const GLFW_KEY_WORLD_1),       KeyWorld1       )
   , ( (#const GLFW_KEY_WORLD_2),       KeyWorld2       )
   , ( (#const GLFW_KEY_ESCAPE),        KeyEscape       )
   , ( (#const GLFW_KEY_ENTER),         KeyEnter        )
   , ( (#const GLFW_KEY_TAB),           KeyTab          )
   , ( (#const GLFW_KEY_BACKSPACE),     KeyBackspace    )
   , ( (#const GLFW_KEY_INSERT),        KeyInsert       )
   , ( (#const GLFW_KEY_DELETE),        KeyDelete       )
   , ( (#const GLFW_KEY_RIGHT),         KeyRight        )
   , ( (#const GLFW_KEY_LEFT),          KeyLeft         )
   , ( (#const GLFW_KEY_DOWN),          KeyDown         )
   , ( (#const GLFW_KEY_UP),            KeyUp           )
   , ( (#const GLFW_KEY_PAGE_UP),       KeyPageUp       )
   , ( (#const GLFW_KEY_PAGE_DOWN),     KeyPageDown     )
   , ( (#const GLFW_KEY_HOME),          KeyHome         )
   , ( (#const GLFW_KEY_END),           KeyEnd          )
   , ( (#const GLFW_KEY_CAPS_LOCK),     KeyCapsLock     )
   , ( (#const GLFW_KEY_SCROLL_LOCK),   KeyScrollLock   )
   , ( (#const GLFW_KEY_NUM_LOCK),      KeyNumLock      )
   , ( (#const GLFW_KEY_PRINT_SCREEN),  KeyPrintScreen  )
   , ( (#const GLFW_KEY_PAUSE),         KeyPause        )
   , ( (#const GLFW_KEY_F1),            KeyF1           )
   , ( (#const GLFW_KEY_F2),            KeyF2           )
   , ( (#const GLFW_KEY_F3),            KeyF3           )
   , ( (#const GLFW_KEY_F4),            KeyF4           )
   , ( (#const GLFW_KEY_F5),            KeyF5           )
   , ( (#const GLFW_KEY_F6),            KeyF6           )
   , ( (#const GLFW_KEY_F7),            KeyF7           )
   , ( (#const GLFW_KEY_F8),            KeyF8           )
   , ( (#const GLFW_KEY_F9),            KeyF9           )
   , ( (#const GLFW_KEY_F10),           KeyF10          )
   , ( (#const GLFW_KEY_F11),           KeyF11          )
   , ( (#const GLFW_KEY_F12),           KeyF12          )
   , ( (#const GLFW_KEY_F13),           KeyF13          )
   , ( (#const GLFW_KEY_F14),           KeyF14          )
   , ( (#const GLFW_KEY_F15),           KeyF15          )
   , ( (#const GLFW_KEY_F16),           KeyF16          )
   , ( (#const GLFW_KEY_F17),           KeyF17          )
   , ( (#const GLFW_KEY_F18),           KeyF18          )
   , ( (#const GLFW_KEY_F19),           KeyF19          )
   , ( (#const GLFW_KEY_F20),           KeyF20          )
   , ( (#const GLFW_KEY_F21),           KeyF21          )
   , ( (#const GLFW_KEY_F22),           KeyF22          )
   , ( (#const GLFW_KEY_F23),           KeyF23          )
   , ( (#const GLFW_KEY_F24),           KeyF24          )
   , ( (#const GLFW_KEY_F25),           KeyF25          )
   , ( (#const GLFW_KEY_KP_0),          KeyPad0         )
   , ( (#const GLFW_KEY_KP_1),          KeyPad1         )
   , ( (#const GLFW_KEY_KP_2),          KeyPad2         )
   , ( (#const GLFW_KEY_KP_3),          KeyPad3         )
   , ( (#const GLFW_KEY_KP_4),          KeyPad4         )
   , ( (#const GLFW_KEY_KP_5),          KeyPad5         )
   , ( (#const GLFW_KEY_KP_6),          KeyPad6         )
   , ( (#const GLFW_KEY_KP_7),          KeyPad7         )
   , ( (#const GLFW_KEY_KP_8),          KeyPad8         )
   , ( (#const GLFW_KEY_KP_9),          KeyPad9         )
   , ( (#const GLFW_KEY_KP_DECIMAL),    KeyPadDecimal   )
   , ( (#const GLFW_KEY_KP_DIVIDE),     KeyPadDivide    )
   , ( (#const GLFW_KEY_KP_MULTIPLY),   KeyPadMultiply  )
   , ( (#const GLFW_KEY_KP_SUBTRACT),   KeyPadSubtract  )
   , ( (#const GLFW_KEY_KP_ADD),        KeyPadAdd       )
   , ( (#const GLFW_KEY_KP_ENTER),      KeyPadEnter     )
   , ( (#const GLFW_KEY_KP_EQUAL),      KeyPadEqual     )
   , ( (#const GLFW_KEY_LEFT_SHIFT),    KeyLeftShift    )
   , ( (#const GLFW_KEY_LEFT_CONTROL),  KeyLeftControl  )
   , ( (#const GLFW_KEY_LEFT_ALT),      KeyLeftAlt      )
   , ( (#const GLFW_KEY_LEFT_SUPER),    KeyLeftSuper    )
   , ( (#const GLFW_KEY_RIGHT_SHIFT),   KeyRightShift   )
   , ( (#const GLFW_KEY_RIGHT_CONTROL), KeyRightControl )
   , ( (#const GLFW_KEY_RIGHT_ALT),     KeyRightAlt     )
   , ( (#const GLFW_KEY_RIGHT_SUPER),   KeyRightSuper   )
   , ( (#const GLFW_KEY_MENU),          KeyMenu         )
   ]

table_C_CInt_MouseButton :: [(CInt, MouseButton)]
table_C_CInt_MouseButton =
    [ ( (#const GLFW_MOUSE_BUTTON_1), MouseButton1 )
    , ( (#const GLFW_MOUSE_BUTTON_2), MouseButton2 )
    , ( (#const GLFW_MOUSE_BUTTON_3), MouseButton3 )
    , ( (#const GLFW_MOUSE_BUTTON_4), MouseButton4 )
    , ( (#const GLFW_MOUSE_BUTTON_5), MouseButton5 )
    , ( (#const GLFW_MOUSE_BUTTON_6), MouseButton6 )
    , ( (#const GLFW_MOUSE_BUTTON_7), MouseButton7 )
    , ( (#const GLFW_MOUSE_BUTTON_8), MouseButton8 )
    ]

table_C_CInt_WindowAttribute :: [(CInt, WindowAttribute)]
table_C_CInt_WindowAttribute =
    [ ( (#const GLFW_FOCUSED),   Focused   )
    , ( (#const GLFW_ICONIFIED), Iconified )
    , ( (#const GLFW_VISIBLE),   Visible   )
    , ( (#const GLFW_RESIZABLE), Resizable )
    ]

table_C_CInt_Joystick :: [(CInt, Joystick)]
table_C_CInt_Joystick =
    [ ( (#const GLFW_JOYSTICK_1),  Joystick1  )
    , ( (#const GLFW_JOYSTICK_2),  Joystick2  )
    , ( (#const GLFW_JOYSTICK_3),  Joystick3  )
    , ( (#const GLFW_JOYSTICK_4),  Joystick4  )
    , ( (#const GLFW_JOYSTICK_5),  Joystick5  )
    , ( (#const GLFW_JOYSTICK_6),  Joystick6  )
    , ( (#const GLFW_JOYSTICK_7),  Joystick7  )
    , ( (#const GLFW_JOYSTICK_8),  Joystick8  )
    , ( (#const GLFW_JOYSTICK_9),  Joystick9  )
    , ( (#const GLFW_JOYSTICK_10), Joystick10 )
    , ( (#const GLFW_JOYSTICK_11), Joystick11 )
    , ( (#const GLFW_JOYSTICK_12), Joystick12 )
    , ( (#const GLFW_JOYSTICK_13), Joystick13 )
    , ( (#const GLFW_JOYSTICK_14), Joystick14 )
    , ( (#const GLFW_JOYSTICK_15), Joystick15 )
    , ( (#const GLFW_JOYSTICK_16), Joystick16 )
    ]

{-# ANN module "HLint: ignore Use camelCase" #-}
