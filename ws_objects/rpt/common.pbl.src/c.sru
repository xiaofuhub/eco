$PBExportHeader$c.sru
$PBExportComments$constant object
forward
global type c from nonvisualobject
end type
end forward

global type c from nonvisualobject
end type
global c c

type variables
//Commonly Used Variables
CONSTANT integer CANCEL = - 1
CONSTANT integer FAILURE = - 1
CONSTANT integer SUCCESS =  1
CONSTANT integer NO_ACTION = 0
CONSTANT integer CLOSEWINDOW = 0
CONSTANT integer NOTCLOSEWINDOW =  1
CONSTANT integer RETAIN_DATA = 2
CONSTANT boolean FOCUSED = TRUE
CONSTANT boolean UNFOCUSED = FALSE

//Color
constant long BUTTONFACE =  78682240
constant long WINDOW_BACKGROUND = 1087434968
constant long WINDOW_TEXT = 33554592
constant long APPLICATION_WORKSPACE = 268435456
constant long TRANSPARENT = 553648127

constant long BLACK = RGB(0, 0, 0)
constant long WHITE = RGB(255, 255, 255)
constant long LIGHT_GRAY = RGB(192, 192, 192)
constant long DARK_GRAY = RGB(128, 128, 128)
constant long RED = RGB(255, 0, 0)
constant long DARK_RED = RGB(128, 0, 0)
constant long GREEN = RGB(0, 255, 0)
constant long DARK_GREEN = RGB(0, 128, 0)
constant long BLUE = RGB(0, 0, 255)
constant long DARK_BLUE = RGB(0, 0, 128)
constant long MAGENTA = RGB(255, 0, 255)
constant long DARK_MAGENTA = RGB(128, 0, 128)
constant long CYAN = RGB(0, 255, 255)
constant long DARK_CYAN = RGB(0, 128, 128)
constant long YELLOW = RGB(255, 255, 0)
constant long BROWN = RGB(128, 128, 0)

//color used for modified data
constant long MODIFIED = rgb(251,187,253)

//For Feedback AND Task approval
CONSTANT integer YES =  1
CONSTANT integer PENDING = 0
CONSTANT integer NO = -1
constant string RESULT = 'R'
constant string CHANGES = 'C'
constant INTEGER COMPLETED = 1
//constant INTEGER PENDING = 0
constant INTEGER OUTSTANDING = -1

//For Task Assignment
constant integer RESET = 0 
constant integer NO_DELETE = 1 
constant integer NO_INSERT = 2

//For Jaws
constant string JAWSWINDOW = 'JAWS'
constant string JAWSCLASS = 'JFWUI2'

//positions in tcb
CONSTANT integer  EXECUTIVE_DIRECTOR_LEVEL = 5
CONSTANT integer  DEPUTY_DIRECTOR_LEVEL = 4
CONSTANT integer  ASSISTANT_DIRECTOR_LEVEL = 3
CONSTANT integer  REGIONAL_DIRECTOR_LEVEL = 2
CONSTANT integer  STAFF_LEVEL = 1
CONSTANT integer  INVALID_ID = -1

//Report Category
constant string BUSINESS_PLANNING = 'Business Planning'
constant string PERFORMANCE_PROJECTION = 'Performance Projection'

//Group in Security
constant string Administrator = 'admin'
CONSTANT string  EXECUTIVE_DIRECTOR = 'exedirector'
CONSTANT string  DEPUTY_DIRECTOR = 'depdirector'
CONSTANT string  ASSISTANT_DIRECTOR = 'asstdirector'
CONSTANT string  REGIONAL_DIRECTOR = 'regdirector'
CONSTANT string  STAFF = 'bpstaff'
CONSTANT string  CASE_WORKER = 'caseworker'
CONSTANT string  EXECUTIVE_GROUP= 'exgroup'
CONSTANT string  MANAGER = 'Directors'
constant string CASEWORKER = 'caseworker'

//For the entry of all in dropdown datawindow
constant string ALL_DISPLAY = '<ALL>'
constant integer ALL_VALUE = 32767








end variables

on c.create
call super::create
TriggerEvent( this, "constructor" )
end on

on c.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

