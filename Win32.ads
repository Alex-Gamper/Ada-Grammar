--------------------------------------------------------------------------------
--                                                                            --
--    Copyright (c) 2018 Alexander Gamper, All Rights Reserved.               --
--                                                                            --
--    This program is free software: you can redistribute it and/or modify    --
--    it under the terms of the GNU General Public License as published by    --
--    the Free Software Foundation, either version 3 of the License, or       --
--    (at your option) any later version.                                     --
--                                                                            --
--    This program is distributed in the hope that it will be useful,         --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of          --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           --
--    GNU General Public License for more details.                            --
--                                                                            --
--    You should have received a copy of the GNU General Public License       --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.   --
--                                                                            --
--------------------------------------------------------------------------------
with Interfaces.C;                      use Interfaces.C;
with System;                            use System;
--------------------------------------------------------------------------------
package Win32 is
    
    -- Character Types
    type CHAR is new Interfaces.C.char;
    type LPSTR is access all CHAR;
    type LPCSTR is access constant CHAR;

    -- Integer Types
    type INT is new Interfaces.C.int;
    type BOOL is new INT;

    -- Unsigned Integer Types
    type DWORD is new Interfaces.C.unsigned_long;
    type LPDWORD is access all DWORD;

    -- Handle Types
    type HANDLE_Type is null record;
    type HANDLE is access HANDLE_Type;	
    subtype HINSTANCE is HANDLE;
    subtype HMODULE is HANDLE;

    type LPVOID is new System.Address;

    type SECURITY_ATTRIBUTES is record
        nLength                 : DWORD;
        lpSecurityDescriptor    : LPVOID;
        bInheritHandle          : BOOL;
    end record;
    type LPSECURITY_ATTRIBUTES is access all SECURITY_ATTRIBUTES;

    -- Conversion Functions
    function Addr (S : String) return LPSTR;
    function Addr (S : String) return LPCSTR;

    -- Imports
    function GetModuleFileName(h_Module : HMODULE; lpFilename : LPSTR; nSize : DWORD ) return DWORD;
    procedure OutputDebugString(OutputString : LPSTR);

    function CreateEvent
        (lpEventAttributes  : LPSECURITY_ATTRIBUTES;
        bManualReset        : BOOL;
        bInitialState       : BOOL;
        lpName              : LPCSTR)
    return HANDLE;

    function SetEvent
        (hEvent : HANDLE)
    return BOOL;
   
    function WaitForSingleObject
        (hHandle        : HANDLE;
        dwMilliseconds  : DWORD)
    return DWORD;

    function CloseHandle (hObject : HANDLE) return BOOL;
    
    pragma Import (Stdcall , GetModuleFileName , "GetModuleFileNameA");
    pragma import (stdcall , OutputDebugString , "OutputDebugStringA");
    pragma import (stdcall , CreateEvent , "CreateEventA");
    pragma import (stdcall , SetEvent , "SetEvent");
    pragma import (stdcall , CloseHandle , "CloseHandle");
    pragma import (stdcall , WaitForSingleObject , "WaitForSingleObject");

end Win32;