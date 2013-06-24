#include <GLFW/glfw3.h>

module Graphics.UI.GLFW.Internal.Tables where

--------------------------------------------------------------------------------

import Foreign.C.Types (CInt, CUChar)

import Graphics.UI.GLFW.Types

--------------------------------------------------------------------------------

table_C_CInt_Bool :: [(CInt, Bool)]
table_C_CInt_Bool =
    [ ( (#const GL_FALSE), False )
    , ( (#const GL_TRUE),  True  )
    ]

--------------------------------------------------------------------------------

table_C_CInt_Error :: [(CInt, Error)]
table_C_CInt_Error =
    [ ( (#const GLFW_NOT_INITIALIZED),     Error'NotInitialized     )
    , ( (#const GLFW_NO_CURRENT_CONTEXT),  Error'NoCurrentContext   )
    , ( (#const GLFW_INVALID_ENUM),        Error'InvalidEnum        )
    , ( (#const GLFW_INVALID_VALUE),       Error'InvalidValue       )
    , ( (#const GLFW_OUT_OF_MEMORY),       Error'OutOfMemory        )
    , ( (#const GLFW_API_UNAVAILABLE),     Error'ApiUnavailable     )
    , ( (#const GLFW_VERSION_UNAVAILABLE), Error'VersionUnavailable )
    , ( (#const GLFW_PLATFORM_ERROR),      Error'PlatformError      )
    , ( (#const GLFW_FORMAT_UNAVAILABLE),  Error'FormatUnavailable  )
    ]

--------------------------------------------------------------------------------

table_C_CInt_MonitorState :: [(CInt, MonitorState)]
table_C_CInt_MonitorState =
    [ ( (#const GL_TRUE),  MonitorState'Connected    )
    , ( (#const GL_FALSE), MonitorState'Disconnected )
    ]

--------------------------------------------------------------------------------

table_C_CInt_FocusState :: [(CInt, FocusState)]
table_C_CInt_FocusState =
    [ ( (#const GL_TRUE),  FocusState'Focused   )
    , ( (#const GL_FALSE), FocusState'Defocused )
    ]

table_C_CInt_IconifyState :: [(CInt, IconifyState)]
table_C_CInt_IconifyState =
    [ ( (#const GL_TRUE),  IconifyState'Iconified    )
    , ( (#const GL_FALSE), IconifyState'NotIconified )
    ]

table_C_CInt_ContextRobustness :: [(CInt, ContextRobustness)]
table_C_CInt_ContextRobustness =
    [ ( (#const GLFW_NO_ROBUSTNESS),         ContextRobustness'NoRobustness        )
    , ( (#const GLFW_NO_RESET_NOTIFICATION), ContextRobustness'NoResetNotification )
    , ( (#const GLFW_LOSE_CONTEXT_ON_RESET), ContextRobustness'LoseContextOnReset  )
    ]

table_C_CInt_OpenGLProfile :: [(CInt, OpenGLProfile)]
table_C_CInt_OpenGLProfile =
    [ ( (#const GLFW_OPENGL_ANY_PROFILE),    OpenGLProfile'Any    )
    , ( (#const GLFW_OPENGL_COMPAT_PROFILE), OpenGLProfile'Compat )
    , ( (#const GLFW_OPENGL_CORE_PROFILE),   OpenGLProfile'Core   )
    ]

table_C_CInt_ClientAPI :: [(CInt, ClientAPI)]
table_C_CInt_ClientAPI =
    [ ( (#const GLFW_OPENGL_API),    ClientAPI'OpenGL   )
    , ( (#const GLFW_OPENGL_ES_API), ClientAPI'OpenGLES )
    ]

--------------------------------------------------------------------------------

table_C_CInt_Key :: [(CInt, Key)]
table_C_CInt_Key =
   [ ( (#const GLFW_KEY_UNKNOWN),       Key'Unknown      )
   , ( (#const GLFW_KEY_SPACE),         Key'Space        )
   , ( (#const GLFW_KEY_APOSTROPHE),    Key'Apostrophe   )
   , ( (#const GLFW_KEY_COMMA),         Key'Comma        )
   , ( (#const GLFW_KEY_MINUS),         Key'Minus        )
   , ( (#const GLFW_KEY_PERIOD),        Key'Period       )
   , ( (#const GLFW_KEY_SLASH),         Key'Slash        )
   , ( (#const GLFW_KEY_0),             Key'0            )
   , ( (#const GLFW_KEY_1),             Key'1            )
   , ( (#const GLFW_KEY_2),             Key'2            )
   , ( (#const GLFW_KEY_3),             Key'3            )
   , ( (#const GLFW_KEY_4),             Key'4            )
   , ( (#const GLFW_KEY_5),             Key'5            )
   , ( (#const GLFW_KEY_6),             Key'6            )
   , ( (#const GLFW_KEY_7),             Key'7            )
   , ( (#const GLFW_KEY_8),             Key'8            )
   , ( (#const GLFW_KEY_9),             Key'9            )
   , ( (#const GLFW_KEY_SEMICOLON),     Key'Semicolon    )
   , ( (#const GLFW_KEY_EQUAL),         Key'Equal        )
   , ( (#const GLFW_KEY_A),             Key'A            )
   , ( (#const GLFW_KEY_B),             Key'B            )
   , ( (#const GLFW_KEY_C),             Key'C            )
   , ( (#const GLFW_KEY_D),             Key'D            )
   , ( (#const GLFW_KEY_E),             Key'E            )
   , ( (#const GLFW_KEY_F),             Key'F            )
   , ( (#const GLFW_KEY_G),             Key'G            )
   , ( (#const GLFW_KEY_H),             Key'H            )
   , ( (#const GLFW_KEY_I),             Key'I            )
   , ( (#const GLFW_KEY_J),             Key'J            )
   , ( (#const GLFW_KEY_K),             Key'K            )
   , ( (#const GLFW_KEY_L),             Key'L            )
   , ( (#const GLFW_KEY_M),             Key'M            )
   , ( (#const GLFW_KEY_N),             Key'N            )
   , ( (#const GLFW_KEY_O),             Key'O            )
   , ( (#const GLFW_KEY_P),             Key'P            )
   , ( (#const GLFW_KEY_Q),             Key'Q            )
   , ( (#const GLFW_KEY_R),             Key'R            )
   , ( (#const GLFW_KEY_S),             Key'S            )
   , ( (#const GLFW_KEY_T),             Key'T            )
   , ( (#const GLFW_KEY_U),             Key'U            )
   , ( (#const GLFW_KEY_V),             Key'V            )
   , ( (#const GLFW_KEY_W),             Key'W            )
   , ( (#const GLFW_KEY_X),             Key'X            )
   , ( (#const GLFW_KEY_Y),             Key'Y            )
   , ( (#const GLFW_KEY_Z),             Key'Z            )
   , ( (#const GLFW_KEY_LEFT_BRACKET),  Key'LeftBracket  )
   , ( (#const GLFW_KEY_BACKSLASH),     Key'Backslash    )
   , ( (#const GLFW_KEY_RIGHT_BRACKET), Key'RightBracket )
   , ( (#const GLFW_KEY_GRAVE_ACCENT),  Key'GraveAccent  )
   , ( (#const GLFW_KEY_WORLD_1),       Key'World1       )
   , ( (#const GLFW_KEY_WORLD_2),       Key'World2       )
   , ( (#const GLFW_KEY_ESCAPE),        Key'Escape       )
   , ( (#const GLFW_KEY_ENTER),         Key'Enter        )
   , ( (#const GLFW_KEY_TAB),           Key'Tab          )
   , ( (#const GLFW_KEY_BACKSPACE),     Key'Backspace    )
   , ( (#const GLFW_KEY_INSERT),        Key'Insert       )
   , ( (#const GLFW_KEY_DELETE),        Key'Delete       )
   , ( (#const GLFW_KEY_RIGHT),         Key'Right        )
   , ( (#const GLFW_KEY_LEFT),          Key'Left         )
   , ( (#const GLFW_KEY_DOWN),          Key'Down         )
   , ( (#const GLFW_KEY_UP),            Key'Up           )
   , ( (#const GLFW_KEY_PAGE_UP),       Key'PageUp       )
   , ( (#const GLFW_KEY_PAGE_DOWN),     Key'PageDown     )
   , ( (#const GLFW_KEY_HOME),          Key'Home         )
   , ( (#const GLFW_KEY_END),           Key'End          )
   , ( (#const GLFW_KEY_CAPS_LOCK),     Key'CapsLock     )
   , ( (#const GLFW_KEY_SCROLL_LOCK),   Key'ScrollLock   )
   , ( (#const GLFW_KEY_NUM_LOCK),      Key'NumLock      )
   , ( (#const GLFW_KEY_PRINT_SCREEN),  Key'PrintScreen  )
   , ( (#const GLFW_KEY_PAUSE),         Key'Pause        )
   , ( (#const GLFW_KEY_F1),            Key'F1           )
   , ( (#const GLFW_KEY_F2),            Key'F2           )
   , ( (#const GLFW_KEY_F3),            Key'F3           )
   , ( (#const GLFW_KEY_F4),            Key'F4           )
   , ( (#const GLFW_KEY_F5),            Key'F5           )
   , ( (#const GLFW_KEY_F6),            Key'F6           )
   , ( (#const GLFW_KEY_F7),            Key'F7           )
   , ( (#const GLFW_KEY_F8),            Key'F8           )
   , ( (#const GLFW_KEY_F9),            Key'F9           )
   , ( (#const GLFW_KEY_F10),           Key'F10          )
   , ( (#const GLFW_KEY_F11),           Key'F11          )
   , ( (#const GLFW_KEY_F12),           Key'F12          )
   , ( (#const GLFW_KEY_F13),           Key'F13          )
   , ( (#const GLFW_KEY_F14),           Key'F14          )
   , ( (#const GLFW_KEY_F15),           Key'F15          )
   , ( (#const GLFW_KEY_F16),           Key'F16          )
   , ( (#const GLFW_KEY_F17),           Key'F17          )
   , ( (#const GLFW_KEY_F18),           Key'F18          )
   , ( (#const GLFW_KEY_F19),           Key'F19          )
   , ( (#const GLFW_KEY_F20),           Key'F20          )
   , ( (#const GLFW_KEY_F21),           Key'F21          )
   , ( (#const GLFW_KEY_F22),           Key'F22          )
   , ( (#const GLFW_KEY_F23),           Key'F23          )
   , ( (#const GLFW_KEY_F24),           Key'F24          )
   , ( (#const GLFW_KEY_F25),           Key'F25          )
   , ( (#const GLFW_KEY_KP_0),          Key'Pad0         )
   , ( (#const GLFW_KEY_KP_1),          Key'Pad1         )
   , ( (#const GLFW_KEY_KP_2),          Key'Pad2         )
   , ( (#const GLFW_KEY_KP_3),          Key'Pad3         )
   , ( (#const GLFW_KEY_KP_4),          Key'Pad4         )
   , ( (#const GLFW_KEY_KP_5),          Key'Pad5         )
   , ( (#const GLFW_KEY_KP_6),          Key'Pad6         )
   , ( (#const GLFW_KEY_KP_7),          Key'Pad7         )
   , ( (#const GLFW_KEY_KP_8),          Key'Pad8         )
   , ( (#const GLFW_KEY_KP_9),          Key'Pad9         )
   , ( (#const GLFW_KEY_KP_DECIMAL),    Key'PadDecimal   )
   , ( (#const GLFW_KEY_KP_DIVIDE),     Key'PadDivide    )
   , ( (#const GLFW_KEY_KP_MULTIPLY),   Key'PadMultiply  )
   , ( (#const GLFW_KEY_KP_SUBTRACT),   Key'PadSubtract  )
   , ( (#const GLFW_KEY_KP_ADD),        Key'PadAdd       )
   , ( (#const GLFW_KEY_KP_ENTER),      Key'PadEnter     )
   , ( (#const GLFW_KEY_KP_EQUAL),      Key'PadEqual     )
   , ( (#const GLFW_KEY_LEFT_SHIFT),    Key'LeftShift    )
   , ( (#const GLFW_KEY_LEFT_CONTROL),  Key'LeftControl  )
   , ( (#const GLFW_KEY_LEFT_ALT),      Key'LeftAlt      )
   , ( (#const GLFW_KEY_LEFT_SUPER),    Key'LeftSuper    )
   , ( (#const GLFW_KEY_RIGHT_SHIFT),   Key'RightShift   )
   , ( (#const GLFW_KEY_RIGHT_CONTROL), Key'RightControl )
   , ( (#const GLFW_KEY_RIGHT_ALT),     Key'RightAlt     )
   , ( (#const GLFW_KEY_RIGHT_SUPER),   Key'RightSuper   )
   , ( (#const GLFW_KEY_MENU),          Key'Menu         )
   ]

table_C_CInt_KeyState :: [(CInt, KeyState)]
table_C_CInt_KeyState =
    [ ( (#const GLFW_PRESS),   KeyState'Pressed   )
    , ( (#const GLFW_RELEASE), KeyState'Released  )
    , ( (#const GLFW_REPEAT),  KeyState'Repeating )
    ]

table_C_CInt_Joystick :: [(CInt, Joystick)]
table_C_CInt_Joystick =
    [ ( (#const GLFW_JOYSTICK_1),  Joystick'1  )
    , ( (#const GLFW_JOYSTICK_2),  Joystick'2  )
    , ( (#const GLFW_JOYSTICK_3),  Joystick'3  )
    , ( (#const GLFW_JOYSTICK_4),  Joystick'4  )
    , ( (#const GLFW_JOYSTICK_5),  Joystick'5  )
    , ( (#const GLFW_JOYSTICK_6),  Joystick'6  )
    , ( (#const GLFW_JOYSTICK_7),  Joystick'7  )
    , ( (#const GLFW_JOYSTICK_8),  Joystick'8  )
    , ( (#const GLFW_JOYSTICK_9),  Joystick'9  )
    , ( (#const GLFW_JOYSTICK_10), Joystick'10 )
    , ( (#const GLFW_JOYSTICK_11), Joystick'11 )
    , ( (#const GLFW_JOYSTICK_12), Joystick'12 )
    , ( (#const GLFW_JOYSTICK_13), Joystick'13 )
    , ( (#const GLFW_JOYSTICK_14), Joystick'14 )
    , ( (#const GLFW_JOYSTICK_15), Joystick'15 )
    , ( (#const GLFW_JOYSTICK_16), Joystick'16 )
    ]

table_C_CUChar_JoystickButtonState :: [(CUChar, JoystickButtonState)]
table_C_CUChar_JoystickButtonState =
    [ ( (#const GLFW_PRESS),   JoystickButtonState'Pressed  )
    , ( (#const GLFW_RELEASE), JoystickButtonState'Released )
    ]

table_C_CInt_MouseButton :: [(CInt, MouseButton)]
table_C_CInt_MouseButton =
    [ ( (#const GLFW_MOUSE_BUTTON_1), MouseButton'1 )
    , ( (#const GLFW_MOUSE_BUTTON_2), MouseButton'2 )
    , ( (#const GLFW_MOUSE_BUTTON_3), MouseButton'3 )
    , ( (#const GLFW_MOUSE_BUTTON_4), MouseButton'4 )
    , ( (#const GLFW_MOUSE_BUTTON_5), MouseButton'5 )
    , ( (#const GLFW_MOUSE_BUTTON_6), MouseButton'6 )
    , ( (#const GLFW_MOUSE_BUTTON_7), MouseButton'7 )
    , ( (#const GLFW_MOUSE_BUTTON_8), MouseButton'8 )
    ]

table_C_CInt_MouseButtonState :: [(CInt, MouseButtonState)]
table_C_CInt_MouseButtonState =
    [ ( (#const GLFW_PRESS),   MouseButtonState'Pressed  )
    , ( (#const GLFW_RELEASE), MouseButtonState'Released )
    ]

table_C_CInt_CursorState :: [(CInt, CursorState)]
table_C_CInt_CursorState =
    [ ( (#const GL_TRUE),  CursorState'InWindow    )
    , ( (#const GL_FALSE), CursorState'NotInWindow )
    ]

table_C_CInt_CursorInputMode :: [(CInt, CursorInputMode)]
table_C_CInt_CursorInputMode =
    [ ( (#const GLFW_CURSOR_NORMAL),   CursorInputMode'Normal   )
    , ( (#const GLFW_CURSOR_HIDDEN),   CursorInputMode'Hidden   )
    , ( (#const GLFW_CURSOR_DISABLED), CursorInputMode'Disabled )
    ]

table_C_CInt_StickyKeysInputMode :: [(CInt, StickyKeysInputMode)]
table_C_CInt_StickyKeysInputMode =
    [ ( (#const GL_TRUE),  StickyKeysInputMode'Enabled )
    , ( (#const GL_FALSE), StickyKeysInputMode'Disabled )
    ]

table_C_CInt_StickyMouseButtonsInputMode :: [(CInt, StickyMouseButtonsInputMode)]
table_C_CInt_StickyMouseButtonsInputMode =
    [ ( (#const GL_TRUE),  StickyMouseButtonsInputMode'Enabled )
    , ( (#const GL_FALSE), StickyMouseButtonsInputMode'Disabled )
    ]

{-# ANN module "HLint: ignore Use camelCase" #-}
