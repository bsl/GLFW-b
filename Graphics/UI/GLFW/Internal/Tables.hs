-- Lists of pairs for use with deriveC.

module Graphics.UI.GLFW.Internal.Tables where

--------------------------------------------------------------------------------

import Foreign.C.Types (CInt, CUChar)

import Bindings.GLFW
import Graphics.UI.GLFW.Types

--------------------------------------------------------------------------------

table_C_CInt_Bool :: [(CInt, Bool)]
table_C_CInt_Bool =
    [ ( c'GL_FALSE, False )
    , ( c'GL_TRUE,  True  )
    ]

--------------------------------------------------------------------------------

table_C_CInt_Error :: [(CInt, Error)]
table_C_CInt_Error =
    [ ( c'GLFW_NOT_INITIALIZED,     Error'NotInitialized     )
    , ( c'GLFW_NO_CURRENT_CONTEXT,  Error'NoCurrentContext   )
    , ( c'GLFW_INVALID_ENUM,        Error'InvalidEnum        )
    , ( c'GLFW_INVALID_VALUE,       Error'InvalidValue       )
    , ( c'GLFW_OUT_OF_MEMORY,       Error'OutOfMemory        )
    , ( c'GLFW_API_UNAVAILABLE,     Error'ApiUnavailable     )
    , ( c'GLFW_VERSION_UNAVAILABLE, Error'VersionUnavailable )
    , ( c'GLFW_PLATFORM_ERROR,      Error'PlatformError      )
    , ( c'GLFW_FORMAT_UNAVAILABLE,  Error'FormatUnavailable  )
    ]

--------------------------------------------------------------------------------

table_C_CInt_MonitorState :: [(CInt, MonitorState)]
table_C_CInt_MonitorState =
    [ ( c'GL_TRUE,  MonitorState'Connected    )
    , ( c'GL_FALSE, MonitorState'Disconnected )
    ]

--------------------------------------------------------------------------------

table_C_CInt_FocusState :: [(CInt, FocusState)]
table_C_CInt_FocusState =
    [ ( c'GL_TRUE,  FocusState'Focused   )
    , ( c'GL_FALSE, FocusState'Defocused )
    ]

table_C_CInt_IconifyState :: [(CInt, IconifyState)]
table_C_CInt_IconifyState =
    [ ( c'GL_TRUE,  IconifyState'Iconified    )
    , ( c'GL_FALSE, IconifyState'NotIconified )
    ]

table_C_CInt_ContextRobustness :: [(CInt, ContextRobustness)]
table_C_CInt_ContextRobustness =
    [ ( c'GLFW_NO_ROBUSTNESS,         ContextRobustness'NoRobustness        )
    , ( c'GLFW_NO_RESET_NOTIFICATION, ContextRobustness'NoResetNotification )
    , ( c'GLFW_LOSE_CONTEXT_ON_RESET, ContextRobustness'LoseContextOnReset  )
    ]

table_C_CInt_OpenGLProfile :: [(CInt, OpenGLProfile)]
table_C_CInt_OpenGLProfile =
    [ ( c'GLFW_OPENGL_ANY_PROFILE,    OpenGLProfile'Any    )
    , ( c'GLFW_OPENGL_COMPAT_PROFILE, OpenGLProfile'Compat )
    , ( c'GLFW_OPENGL_CORE_PROFILE,   OpenGLProfile'Core   )
    ]

table_C_CInt_ClientAPI :: [(CInt, ClientAPI)]
table_C_CInt_ClientAPI =
    [ ( c'GLFW_OPENGL_API,    ClientAPI'OpenGL   )
    , ( c'GLFW_OPENGL_ES_API, ClientAPI'OpenGLES )
    ]

--------------------------------------------------------------------------------

table_C_CInt_Key :: [(CInt, Key)]
table_C_CInt_Key =
   [ ( c'GLFW_KEY_UNKNOWN,       Key'Unknown      )
   , ( c'GLFW_KEY_SPACE,         Key'Space        )
   , ( c'GLFW_KEY_APOSTROPHE,    Key'Apostrophe   )
   , ( c'GLFW_KEY_COMMA,         Key'Comma        )
   , ( c'GLFW_KEY_MINUS,         Key'Minus        )
   , ( c'GLFW_KEY_PERIOD,        Key'Period       )
   , ( c'GLFW_KEY_SLASH,         Key'Slash        )
   , ( c'GLFW_KEY_0,             Key'0            )
   , ( c'GLFW_KEY_1,             Key'1            )
   , ( c'GLFW_KEY_2,             Key'2            )
   , ( c'GLFW_KEY_3,             Key'3            )
   , ( c'GLFW_KEY_4,             Key'4            )
   , ( c'GLFW_KEY_5,             Key'5            )
   , ( c'GLFW_KEY_6,             Key'6            )
   , ( c'GLFW_KEY_7,             Key'7            )
   , ( c'GLFW_KEY_8,             Key'8            )
   , ( c'GLFW_KEY_9,             Key'9            )
   , ( c'GLFW_KEY_SEMICOLON,     Key'Semicolon    )
   , ( c'GLFW_KEY_EQUAL,         Key'Equal        )
   , ( c'GLFW_KEY_A,             Key'A            )
   , ( c'GLFW_KEY_B,             Key'B            )
   , ( c'GLFW_KEY_C,             Key'C            )
   , ( c'GLFW_KEY_D,             Key'D            )
   , ( c'GLFW_KEY_E,             Key'E            )
   , ( c'GLFW_KEY_F,             Key'F            )
   , ( c'GLFW_KEY_G,             Key'G            )
   , ( c'GLFW_KEY_H,             Key'H            )
   , ( c'GLFW_KEY_I,             Key'I            )
   , ( c'GLFW_KEY_J,             Key'J            )
   , ( c'GLFW_KEY_K,             Key'K            )
   , ( c'GLFW_KEY_L,             Key'L            )
   , ( c'GLFW_KEY_M,             Key'M            )
   , ( c'GLFW_KEY_N,             Key'N            )
   , ( c'GLFW_KEY_O,             Key'O            )
   , ( c'GLFW_KEY_P,             Key'P            )
   , ( c'GLFW_KEY_Q,             Key'Q            )
   , ( c'GLFW_KEY_R,             Key'R            )
   , ( c'GLFW_KEY_S,             Key'S            )
   , ( c'GLFW_KEY_T,             Key'T            )
   , ( c'GLFW_KEY_U,             Key'U            )
   , ( c'GLFW_KEY_V,             Key'V            )
   , ( c'GLFW_KEY_W,             Key'W            )
   , ( c'GLFW_KEY_X,             Key'X            )
   , ( c'GLFW_KEY_Y,             Key'Y            )
   , ( c'GLFW_KEY_Z,             Key'Z            )
   , ( c'GLFW_KEY_LEFT_BRACKET,  Key'LeftBracket  )
   , ( c'GLFW_KEY_BACKSLASH,     Key'Backslash    )
   , ( c'GLFW_KEY_RIGHT_BRACKET, Key'RightBracket )
   , ( c'GLFW_KEY_GRAVE_ACCENT,  Key'GraveAccent  )
   , ( c'GLFW_KEY_WORLD_1,       Key'World1       )
   , ( c'GLFW_KEY_WORLD_2,       Key'World2       )
   , ( c'GLFW_KEY_ESCAPE,        Key'Escape       )
   , ( c'GLFW_KEY_ENTER,         Key'Enter        )
   , ( c'GLFW_KEY_TAB,           Key'Tab          )
   , ( c'GLFW_KEY_BACKSPACE,     Key'Backspace    )
   , ( c'GLFW_KEY_INSERT,        Key'Insert       )
   , ( c'GLFW_KEY_DELETE,        Key'Delete       )
   , ( c'GLFW_KEY_RIGHT,         Key'Right        )
   , ( c'GLFW_KEY_LEFT,          Key'Left         )
   , ( c'GLFW_KEY_DOWN,          Key'Down         )
   , ( c'GLFW_KEY_UP,            Key'Up           )
   , ( c'GLFW_KEY_PAGE_UP,       Key'PageUp       )
   , ( c'GLFW_KEY_PAGE_DOWN,     Key'PageDown     )
   , ( c'GLFW_KEY_HOME,          Key'Home         )
   , ( c'GLFW_KEY_END,           Key'End          )
   , ( c'GLFW_KEY_CAPS_LOCK,     Key'CapsLock     )
   , ( c'GLFW_KEY_SCROLL_LOCK,   Key'ScrollLock   )
   , ( c'GLFW_KEY_NUM_LOCK,      Key'NumLock      )
   , ( c'GLFW_KEY_PRINT_SCREEN,  Key'PrintScreen  )
   , ( c'GLFW_KEY_PAUSE,         Key'Pause        )
   , ( c'GLFW_KEY_F1,            Key'F1           )
   , ( c'GLFW_KEY_F2,            Key'F2           )
   , ( c'GLFW_KEY_F3,            Key'F3           )
   , ( c'GLFW_KEY_F4,            Key'F4           )
   , ( c'GLFW_KEY_F5,            Key'F5           )
   , ( c'GLFW_KEY_F6,            Key'F6           )
   , ( c'GLFW_KEY_F7,            Key'F7           )
   , ( c'GLFW_KEY_F8,            Key'F8           )
   , ( c'GLFW_KEY_F9,            Key'F9           )
   , ( c'GLFW_KEY_F10,           Key'F10          )
   , ( c'GLFW_KEY_F11,           Key'F11          )
   , ( c'GLFW_KEY_F12,           Key'F12          )
   , ( c'GLFW_KEY_F13,           Key'F13          )
   , ( c'GLFW_KEY_F14,           Key'F14          )
   , ( c'GLFW_KEY_F15,           Key'F15          )
   , ( c'GLFW_KEY_F16,           Key'F16          )
   , ( c'GLFW_KEY_F17,           Key'F17          )
   , ( c'GLFW_KEY_F18,           Key'F18          )
   , ( c'GLFW_KEY_F19,           Key'F19          )
   , ( c'GLFW_KEY_F20,           Key'F20          )
   , ( c'GLFW_KEY_F21,           Key'F21          )
   , ( c'GLFW_KEY_F22,           Key'F22          )
   , ( c'GLFW_KEY_F23,           Key'F23          )
   , ( c'GLFW_KEY_F24,           Key'F24          )
   , ( c'GLFW_KEY_F25,           Key'F25          )
   , ( c'GLFW_KEY_KP_0,          Key'Pad0         )
   , ( c'GLFW_KEY_KP_1,          Key'Pad1         )
   , ( c'GLFW_KEY_KP_2,          Key'Pad2         )
   , ( c'GLFW_KEY_KP_3,          Key'Pad3         )
   , ( c'GLFW_KEY_KP_4,          Key'Pad4         )
   , ( c'GLFW_KEY_KP_5,          Key'Pad5         )
   , ( c'GLFW_KEY_KP_6,          Key'Pad6         )
   , ( c'GLFW_KEY_KP_7,          Key'Pad7         )
   , ( c'GLFW_KEY_KP_8,          Key'Pad8         )
   , ( c'GLFW_KEY_KP_9,          Key'Pad9         )
   , ( c'GLFW_KEY_KP_DECIMAL,    Key'PadDecimal   )
   , ( c'GLFW_KEY_KP_DIVIDE,     Key'PadDivide    )
   , ( c'GLFW_KEY_KP_MULTIPLY,   Key'PadMultiply  )
   , ( c'GLFW_KEY_KP_SUBTRACT,   Key'PadSubtract  )
   , ( c'GLFW_KEY_KP_ADD,        Key'PadAdd       )
   , ( c'GLFW_KEY_KP_ENTER,      Key'PadEnter     )
   , ( c'GLFW_KEY_KP_EQUAL,      Key'PadEqual     )
   , ( c'GLFW_KEY_LEFT_SHIFT,    Key'LeftShift    )
   , ( c'GLFW_KEY_LEFT_CONTROL,  Key'LeftControl  )
   , ( c'GLFW_KEY_LEFT_ALT,      Key'LeftAlt      )
   , ( c'GLFW_KEY_LEFT_SUPER,    Key'LeftSuper    )
   , ( c'GLFW_KEY_RIGHT_SHIFT,   Key'RightShift   )
   , ( c'GLFW_KEY_RIGHT_CONTROL, Key'RightControl )
   , ( c'GLFW_KEY_RIGHT_ALT,     Key'RightAlt     )
   , ( c'GLFW_KEY_RIGHT_SUPER,   Key'RightSuper   )
   , ( c'GLFW_KEY_MENU,          Key'Menu         )
   ]

table_C_CInt_KeyState :: [(CInt, KeyState)]
table_C_CInt_KeyState =
    [ ( c'GLFW_PRESS,   KeyState'Pressed   )
    , ( c'GLFW_RELEASE, KeyState'Released  )
    , ( c'GLFW_REPEAT,  KeyState'Repeating )
    ]

table_C_CInt_Joystick :: [(CInt, Joystick)]
table_C_CInt_Joystick =
    [ ( c'GLFW_JOYSTICK_1,  Joystick'1  )
    , ( c'GLFW_JOYSTICK_2,  Joystick'2  )
    , ( c'GLFW_JOYSTICK_3,  Joystick'3  )
    , ( c'GLFW_JOYSTICK_4,  Joystick'4  )
    , ( c'GLFW_JOYSTICK_5,  Joystick'5  )
    , ( c'GLFW_JOYSTICK_6,  Joystick'6  )
    , ( c'GLFW_JOYSTICK_7,  Joystick'7  )
    , ( c'GLFW_JOYSTICK_8,  Joystick'8  )
    , ( c'GLFW_JOYSTICK_9,  Joystick'9  )
    , ( c'GLFW_JOYSTICK_10, Joystick'10 )
    , ( c'GLFW_JOYSTICK_11, Joystick'11 )
    , ( c'GLFW_JOYSTICK_12, Joystick'12 )
    , ( c'GLFW_JOYSTICK_13, Joystick'13 )
    , ( c'GLFW_JOYSTICK_14, Joystick'14 )
    , ( c'GLFW_JOYSTICK_15, Joystick'15 )
    , ( c'GLFW_JOYSTICK_16, Joystick'16 )
    ]

table_C_CUChar_JoystickButtonState :: [(CUChar, JoystickButtonState)]
table_C_CUChar_JoystickButtonState =
    [ ( c'GLFW_PRESS,   JoystickButtonState'Pressed  )
    , ( c'GLFW_RELEASE, JoystickButtonState'Released )
    ]

table_C_CInt_MouseButton :: [(CInt, MouseButton)]
table_C_CInt_MouseButton =
    [ ( c'GLFW_MOUSE_BUTTON_1, MouseButton'1 )
    , ( c'GLFW_MOUSE_BUTTON_2, MouseButton'2 )
    , ( c'GLFW_MOUSE_BUTTON_3, MouseButton'3 )
    , ( c'GLFW_MOUSE_BUTTON_4, MouseButton'4 )
    , ( c'GLFW_MOUSE_BUTTON_5, MouseButton'5 )
    , ( c'GLFW_MOUSE_BUTTON_6, MouseButton'6 )
    , ( c'GLFW_MOUSE_BUTTON_7, MouseButton'7 )
    , ( c'GLFW_MOUSE_BUTTON_8, MouseButton'8 )
    ]

table_C_CInt_MouseButtonState :: [(CInt, MouseButtonState)]
table_C_CInt_MouseButtonState =
    [ ( c'GLFW_PRESS,   MouseButtonState'Pressed  )
    , ( c'GLFW_RELEASE, MouseButtonState'Released )
    ]

table_C_CInt_CursorState :: [(CInt, CursorState)]
table_C_CInt_CursorState =
    [ ( c'GL_TRUE,  CursorState'InWindow    )
    , ( c'GL_FALSE, CursorState'NotInWindow )
    ]

table_C_CInt_CursorInputMode :: [(CInt, CursorInputMode)]
table_C_CInt_CursorInputMode =
    [ ( c'GLFW_CURSOR_NORMAL,   CursorInputMode'Normal   )
    , ( c'GLFW_CURSOR_HIDDEN,   CursorInputMode'Hidden   )
    , ( c'GLFW_CURSOR_DISABLED, CursorInputMode'Disabled )
    ]

table_C_CInt_StickyKeysInputMode :: [(CInt, StickyKeysInputMode)]
table_C_CInt_StickyKeysInputMode =
    [ ( c'GL_TRUE,  StickyKeysInputMode'Enabled )
    , ( c'GL_FALSE, StickyKeysInputMode'Disabled )
    ]

table_C_CInt_StickyMouseButtonsInputMode :: [(CInt, StickyMouseButtonsInputMode)]
table_C_CInt_StickyMouseButtonsInputMode =
    [ ( c'GL_TRUE,  StickyMouseButtonsInputMode'Enabled )
    , ( c'GL_FALSE, StickyMouseButtonsInputMode'Disabled )
    ]

--------------------------------------------------------------------------------

{-# ANN module "HLint: ignore Use camelCase" #-}
