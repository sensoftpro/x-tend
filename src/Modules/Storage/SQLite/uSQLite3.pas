unit uSQLite3;

interface

uses
  Classes, SysUtils, windows, Generics.Collections;

const
  cSqlite3Lib = 'sqlite3.dll';

type
  qword = int64;
  ptruint = cardinal;
  pptruint = ^ptruint;

  { the following type definitions are compiler dependant }
  { and system dependant                                  }

  cint8                  = shortint;           pcint8                 = ^cint8;
  cuint8                 = byte;               pcuint8                = ^cuint8;
  cchar                  = cint8;              pcchar                 = ^cchar;
  cschar                 = cint8;              pcschar                = ^cschar;
  cuchar                 = cuint8;             pcuchar                = ^cuchar;

  cint16                 = smallint;           pcint16                = ^cint16;
  cuint16                = word;               pcuint16               = ^cuint16;
  cshort                 = cint16;             pcshort                = ^cshort;
  csshort                = cint16;             pcsshort               = ^csshort;
  cushort                = cuint16;            pcushort               = ^cushort;

  cint32                 = longint;            pcint32                = ^cint32;
  cuint32                = longword;           pcuint32               = ^cuint32;

  cint64                 = int64;              pcint64                = ^cint64;
  cuint64                = qword;              pcuint64               = ^cuint64;
  clonglong              = cint64;             pclonglong             = ^clonglong;
  cslonglong             = cint64;             pcslonglong            = ^cslonglong;
  culonglong             = cuint64;            pculonglong            = ^culonglong;

  cbool                  = longbool;           pcbool                 = ^cbool;

  cint                   = cint32;             pcint                  = ^cint;              { minimum range is : 32-bit    }
  csint                  = cint32;             pcsint                 = ^csint;             { minimum range is : 32-bit    }
  cuint                  = cuint32;            pcuint                 = ^cuint;             { minimum range is : 32-bit    }
  clong                  = longint;            pclong                 = ^clong;
  cslong                 = longint;            pcslong                = ^cslong;
  culong                 = cardinal;           pculong                = ^culong;

  csigned                = cint;               pcsigned               = ^csigned;
  cunsigned              = cuint;              pcunsigned             = ^cunsigned;

  csize_t                = ptruint;            pcsize_t               = pptruint;

  cdouble                = double;             pcdouble               = ^cdouble;

  { Zero - terminated strings }
  PChar               = ^Char;
  PPChar              = ^PChar;
  PPPChar             = ^PPChar;

  { AnsiChar is equivalent of Char, so we need to use type renamings }
  PPAnsiChar          = ^PAnsiChar;
  PPPAnsiChar         = ^PPAnsiChar;

  UTF8Char            = AnsiChar;
  PUTF8Char           = PAnsiChar;

type
  ppsqlite3 = ^psqlite3;
  psqlite3 = ^_sqlite3;
  _sqlite3 = record end;

  TLibHandle = THandle;

  sqlite3_destructor_type = procedure(user: pointer); cdecl;

  psqlite_int64 = ^sqlite_int64;
  sqlite_int64 = Int64;

  psqlite_uint64 = ^sqlite_uint64;
  sqlite_uint64 = QWord;

  psqlite3_int64 = ^sqlite3_int64;
  sqlite3_int64 = sqlite_int64;

  psqlite3_uint64 = ^sqlite3_uint64;
  sqlite3_uint64 = sqlite_uint64;

  sqlite3_callback = function(user: pointer; cols: cint; values, name: ppansichar): cint; cdecl;

  psqlite3_io_methods = ^sqlite3_io_methods;

  psqlite3_file = ^sqlite3_file;
  sqlite3_file = record
    pMethods: psqlite3_io_methods;  (* Methods for an open file *)
  end;

  sqlite3_io_methods = record
     iVersion          : cint;
     Close             : function(f: psqlite3_file): cint; stdcall;
     Read              : function(f: psqlite3_file; addr: pointer; iAmt: cint; iOfst: sqlite3_int64): cint; stdcall;
     Write             : function(f: psqlite3_file; size: sqlite3_int64): cint; stdcall;
     Truncate          : function(f: psqlite3_file; size: sqlite3_int64): cint; stdcall;
     Sync              : function(f: psqlite3_file; flags: cint): cint; stdcall;
     FileSize          : function(f: psqlite3_file; pSize: psqlite3_int64): cint; stdcall;
     Lock              : function(f: psqlite3_file; flags: cint): cint; stdcall;
     Unlock            : function(f: psqlite3_file; flags: cint): cint; stdcall;
     CheckReservedLock : function(f: psqlite3_file): cint; stdcall;
     FileControl       : function(f: psqlite3_file; op: cint; pArg: pointer): cint; stdcall;
     SectorSize        : function(f: psqlite3_file): cint; stdcall;
     DeviceCharacteristics: function(f: psqlite3_file): cint; stdcall;
     xShmMap : function(f : psqlite3_file; iPg: cint; pgsz: cint; volatile : pointer) : cint;stdcall;
     xShmLock : function(f : psqlite3_file; offset: cint; n : cint; flags : cint) : cint; stdcall;
     xShmBarrier  : procedure (f : psqlite3_file); stdcall;
     xShmUnmap : function(f : psqlite3_file; deleteFlag : cint) : cint; stdcall;
     (* Methods above are valid for version 2 *)
     xFetch : function(f: psqlite3_file; iOfst: sqlite3_int64; iAmt: cint; pp: PPointer) : cint; stdcall;
     xUnfetch : function(f: psqlite3_file; iOfst: sqlite3_int64; p: Pointer) : cint; stdcall;
     (* Methods above are valid for version 3 *)
     (* Additional methods may be added in future releases *)
  end;

  psqlite3_mutex = ^sqlite3_mutex;
  sqlite3_mutex = record end;

  psqlite3_vfs = ^sqlite3_vfs;
  sqlite3_vfs = record
    iVersion     : cint;          (* Structure version number *)
    szOsFile     : cint;          (* Size of subclassed sqlite3_file *)
    mxPathname   : cint;          (* Maximum file pathname length *)
    pNext        : psqlite3_vfs;  (* Next registered VFS *)
    zName        : pansichar;     (* Name of this virtual file system *)
    pAppData     : ppointer;      (* Pointer to application-specific *)
    Open         : function(vfs: psqlite3_vfs; zName: pansichar; f: psqlite3_file; flags: cint; pOutFlags: pcint): cint; cdecl;
    Delete       : function(vfs: psqlite3_vfs; zName: pansichar; syncDir: cint): cint; cdecl;
    Access       : function(vfs: psqlite3_vfs; zName: pansichar; flags: cint): cint; cdecl;
    FullPathname : function(vfs: psqlite3_vfs; zName: pansichar; nOut: cint; zOut: pansichar): cint; cdecl;
    DlOpen       : function(vfs: psqlite3_vfs; zFilename: pansichar): pointer; cdecl;
    DlError      : procedure(vfs: psqlite3_vfs; nByte: cint; zErrMsg: pansichar); cdecl;
    DlSym        : function(vfs: psqlite3_vfs; addr: pointer; zSymbol: pansichar): pointer; cdecl;
    DlClose      : procedure(vfs: psqlite3_vfs; addr: pointer); cdecl;
    Randomness   : function(vfs: psqlite3_vfs; nByte: cint; zOut: pansichar): cint; cdecl;
    Sleep        : function(vfs: psqlite3_vfs; microseconds: cint): cint; cdecl;
    CurrentTime  : function(vfs: psqlite3_vfs; time: pcdouble): cint; cdecl;
    GetLastError : function(vfs: psqlite3_vfs; code: cint; msg: pansichar): cint; cdecl;
    CurrentTimeInt64 : function(vfs: psqlite3_vfs; time: psqlite3_int64): cint; cdecl;
    xSetSystemCall : function(vfs: psqlite3_vfs; zName: pansichar; sqlite3_syscall_ptr : pointer) : cint; cdecl;
    xGetSystemCall : function(vfs: psqlite3_vfs; zName: pansichar) : pointer; cdecl;
    xNextSystemCall : function(vfs: psqlite3_vfs; zName: pansichar) : pansichar; cdecl;
  end;

  xAuth = function(pUserData: pointer; code: cint; s1, s2, s3, s4: pansichar): cint; cdecl;

  sqlite3_mem_methods = record
    xMalloc : function(size : cint) : pointer;cdecl;
    xFree : procedure(p : pointer); cdecl;
    xRealloc : function(p : pointer;size : cint) : pointer;cdecl;
    xSize : function(p : pointer) : cint; cdecl;
    xRoundup : function(size : cint) : cint; cdecl;
    xInit : function(): cint; cdecl;
    xShutdown : procedure(p : pointer); cdecl;
    pAppData: pointer;
  end;
  Tsqlite3_mem_methods = sqlite3_mem_methods;

  busyhandler_callback = function(user: pointer; cnt: cint): cint; cdecl;

  xTrace = procedure(user: pointer; s: pansichar); cdecl;
  xProfile = procedure(user: pointer; s: pansichar; i: sqlite3_uint64); cdecl;
  progress_callback = function(user: pointer): cint; cdecl;

  ppsqlite3_stmt = ^psqlite3_stmt;
  psqlite3_stmt = ^sqlite3_stmt;
  sqlite3_stmt = record end;

  ppsqlite3_value = ^psqlite3_value;
  psqlite3_value = ^sqlite3_value;
  sqlite3_value = record end;

  psqlite3_context = ^sqlite3_context;
  sqlite3_context = record end;

  xFunc = procedure(ctx: psqlite3_context; N: cint; V: ppsqlite3_value); cdecl;
  xStep = procedure(ctx: psqlite3_context; N: cint; V: ppsqlite3_value); cdecl;
  xFinal = procedure(ctx: psqlite3_context); cdecl;
  xDestroy = sqlite3_destructor_type;

  set_auxdata_cb = function(p: pointer): pointer; cdecl;

  xCompare = function(user: pointer; A: cint; B: pointer; C: cint; D: pointer): cint; cdecl;

  collation_needed_cb = function(user: pointer; db: psqlite3; eTextRep: cint; s: pansichar): pointer; cdecl;

  commit_callback = function(user: pointer): cint; cdecl;
  update_callback = procedure(user: pointer; event: cint; database, table: pansichar; rowid: sqlite3_int64); cdecl;

  psqlite3_module = ^sqlite3_module;
  sqlite3_module = record{
    iVersion     : cint;
   // xCreate      : function(db: psqlite3; pAux: pointer; argc: cint;): cint; cdecl;
  cint (*xCreate)(db: psqlite3; void *pAux;
               cint argc; const char *const*argv;
               sqlite3_vtab **ppVTab; char**);
  cint (*xConnect)(db: psqlite3; void *pAux;
               cint argc; const char *const*argv;
               sqlite3_vtab **ppVTab; char**);
  cint (*xBestIndex)(sqlite3_vtab *pVTab; sqlite3_index_info*);
  cint (*xDisconnect)(sqlite3_vtab *pVTab);
  cint (*xDestroy)(sqlite3_vtab *pVTab);
  cint (*xOpen)(sqlite3_vtab *pVTab; sqlite3_vtab_cursor **ppCursor);
  cint (*xClose)(sqlite3_vtab_cursor*);
  cint (*xFilter)(sqlite3_vtab_cursor*; cint idxNum; const char *idxStr;
                cint argc; sqlite3_value **argv);
    xNext         : function(pVCurs: sqlite3_vtab_cursor): cint; cdecl;
    xEof          : function(pVCurs: sqlite3_vtab_cursor): cint; cdecl;
    xColumn       : function(pVCurs: sqlite3_vtab_cursor; ctx: psqlite3_context; i: cint): cint; cdecl;
    xRowid        : function(pVCurs: sqlite3_vtab_cursor; var pRowid: sqlite3_int64): cint; cdecl;
    xUpdate       : function(pVtab: psqlite3_vtab; var v: psqlite3_value; var p: sqlite3_int64): cint; cdecl;
    xBegin        : function(pVtab: psqlite3_vtab): cint; cdecl;
    xSync         : function(pVtab: psqlite3_vtab): cint; cdecl;
    xCommit       : function(pVtab: psqlite3_vtab): cint; cdecl;
    xRollback     : function(pVtab: psqlite3_vtab): cint; cdecl;
    xFindFunction : function(pVtab: psqlite3_vtab; nArg: cint; zName: pansichar; var pxFunc: xFunc; var ppArg: pointer): cint; cdecl;
    xRename       : function(pVtab: psqlite3_vtab; zNew: pansichar): cint; cdecl;
  }end;
{.$WARNING TODO}

  psqlite3_index_constracint = ^sqlite3_index_constracint;
  sqlite3_index_constracint = record
     iColumn: cint;              (* Column constrained.  -1 for ROWID *)
     op: char;                   (* Constracint operator *)
     usable: char;               (* True if this constracint is usable *)
     iTermOffset: cint;          (* Used cinternally - xBestIndex should ignore *)
  end;

  psqlite3_index_orderby = ^sqlite3_index_orderby;
  sqlite3_index_orderby = record
     iColumn: cint;              (* Column number *)
     desc: char;                 (* True for DESC.  False for ASC. *)
  end;

  psqlite3_index_constracint_usage = ^sqlite3_index_constracint_usage;
  sqlite3_index_constracint_usage = record
    argvIndex: cint;             (* if >0; constracint is part of argv to xFilter *)
    omit: char;                  (* Do not code a test for this constracint *)
  end;

  psqlite3_index_info = ^sqlite3_index_info;
  sqlite3_index_info = record
    (* Inputs *)
    nConstracint: cint;         (* Number of entries in aConstracint *)
    aConstracint: psqlite3_index_constracint;
    nOrderBy: cint;             (* Number of terms in the ORDER BY clause *)
    aOrderBy: psqlite3_index_orderby;

    (* Outputs *)
    aConstracintUsage: psqlite3_index_constracint_usage;

    idxNum: cint;                 (* Number used to identify the index *)
    idxStr: pansichar;            (* String; possibly obtained from sqlite3_malloc *)
    needToFreeIdxStr: cint;       (* Free idxStr using sqlite3_free() if true *)
    orderByConsumed: cint;        (* True if output is already ordered *)
    estimatedCost: cdouble;       (* Estimated cost of using this index *)
    (* Fields below are only available in SQLite 3.8.2 and later *)
    estimatedRows: sqlite3_int64; (* Estimated number of rows returned *)
    (* Fields below are only available in SQLite 3.9.0 and later *)
    idxFlags: cint ;              (* Mask of SQLITE_INDEX_SCAN_* flags *)
    (* Fields below are only available in SQLite 3.10.0 and later *)
    colUsed: sqlite3_uint64;      (* Input: Mask of columns used by statement *)
  end;

  psqlite3_vtab = ^sqlite3_vtab;
  sqlite3_vtab = record
    pModule: psqlite3_module;        (* The module for this virtual table *)
    nRef: cint;                      (* Used cinternally *)
    zErrMsg: pansichar;              (* Error message from sqlite3_mprcintf() *)
  (* Virtual table implementations will typically add additional fields *)
  end;

  psqlite3_vtab_cursor = ^sqlite3_vtab_cursor;
  sqlite3_vtab_cursor = record
    pVtab: psqlite3_vtab;      (* Virtual table of this cursor *)
  (* Virtual table implementations will typically add additional fields *)
  end;

  ppsqlite3_blob = ^psqlite3_blob;
  psqlite3_blob = ^sqlite3_blob;
  sqlite3_blob = record end;

  psqlite3backup = Pointer;
  xNotifycb = procedure (Argp: pointer; narg : cint);cdecl;
  wal_hook_cb = function (p : pointer; db :psqlite3; c : pansichar; d: cint): cint;cdecl;

{$IFDEF SQLITE_OBSOLETE}
  memory_alarm_cb = function(user: pointer; i64: sqlite3_int64; i: cint): pointer; cdecl;
{$ENDIF}

const
  SQLITE_STATIC      = nil;
  SQLITE_TRANSIENT   = pointer(-1); //sqlite3_destructor_type(-1);

  //NilHandle = 0;

const
  SQLITE_OK         =  0;   (* Successful Result *)
(* beginning-of-error-codes *)
  SQLITE_ERROR      =  1;   (* SQL error or missing database *)
  SQLITE_INTERNAL   =  2;   (* Internal logic error in SQLite *)
  SQLITE_PERM       =  3;   (* Access permission denied *)
  SQLITE_ABORT      =  4;   (* Callback routine requested an abort *)
  SQLITE_BUSY       =  5;   (* The database file is locked *)
  SQLITE_LOCKED     =  6;   (* A table in the database is locked *)
  SQLITE_NOMEM      =  7;   (* A malloc() failed *)
  SQLITE_READONLY   =  8;   (* Attempt to write a readonly database *)
  SQLITE_INTERRUPT  =  9;   (* Operation terminated by sqlite3_interrupt()*)
  SQLITE_IOERR      = 10;   (* Some kind of disk I/O error occurred *)
  SQLITE_CORRUPT    = 11;   (* The database disk image is malformed *)
  SQLITE_NOTFOUND   = 12;   (* NOT USED. Table or record not found *)
  SQLITE_FULL       = 13;   (* Insertion failed because database is full *)
  SQLITE_CANTOPEN   = 14;   (* Unable to open the database file *)
  SQLITE_PROTOCOL   = 15;   (* NOT USED. Database lock protocol error *)
  SQLITE_EMPTY      = 16;   (* Database is empty *)
  SQLITE_SCHEMA     = 17;   (* The database schema changed *)
  SQLITE_TOOBIG     = 18;   (* String or BLOB exceeds size limit *)
  SQLITE_CONSTRAINT = 19;   (* Abort due to constraint violation *)
  SQLITE_MISMATCH   = 20;   (* Data type mismatch *)
  SQLITE_MISUSE     = 21;   (* Library used incorrectly *)
  SQLITE_NOLFS      = 22;   (* Uses OS features not supported on host *)
  SQLITE_AUTH       = 23;   (* Authorization denied *)
  SQLITE_FORMAT     = 24;   (* Auxiliary database format error *)
  SQLITE_RANGE      = 25;   (* 2nd parameter to sqlite3_bind out of range *)
  SQLITE_NOTADB     = 26;   (* File opened that is not a database file *)
  SQLITE_NOTICE     = 27;   (* Notifications from sqlite3_log() *)
  SQLITE_WARNING    = 28;   (* Warnings from sqlite3_log() *)
  SQLITE_ROW        = 100;  (* sqlite3_step() has another row ready *)
  SQLITE_DONE       = 101;  (* sqlite3_step() has finished executing *)

  SQLITE_IOERR_READ          = (SQLITE_IOERR or (1 shl 8));
  SQLITE_IOERR_SHORT_READ    = (SQLITE_IOERR or (2 shl 8));
  SQLITE_IOERR_WRITE         = (SQLITE_IOERR or (3 shl 8));
  SQLITE_IOERR_FSYNC         = (SQLITE_IOERR or (4 shl 8));
  SQLITE_IOERR_DIR_FSYNC     = (SQLITE_IOERR or (5 shl 8));
  SQLITE_IOERR_TRUNCATE      = (SQLITE_IOERR or (6 shl 8));
  SQLITE_IOERR_FSTAT         = (SQLITE_IOERR or (7 shl 8));
  SQLITE_IOERR_UNLOCK        = (SQLITE_IOERR or (8 shl 8));
  SQLITE_IOERR_RDLOCK        = (SQLITE_IOERR or (9 shl 8));
  SQLITE_IOERR_DELETE        = (SQLITE_IOERR or (10 shl 8));
  SQLITE_IOERR_BLOCKED       = (SQLITE_IOERR or (11 shl 8));
  SQLITE_IOERR_NOMEM         = (SQLITE_IOERR or (12 shl 8));
  SQLITE_IOERR_ACCESS            = (SQLITE_IOERR or (13 shl 8));
  SQLITE_IOERR_CHECKRESERVEDLOCK = (SQLITE_IOERR or (14 shl 8));
  SQLITE_IOERR_LOCK              = (SQLITE_IOERR or (15 shl 8));
  SQLITE_IOERR_CLOSE             = (SQLITE_IOERR or (16 shl 8));
  SQLITE_IOERR_DIR_CLOSE         = (SQLITE_IOERR or (17 shl 8));
  SQLITE_IOERR_SHMOPEN           = (SQLITE_IOERR or (18 shl 8));
  SQLITE_IOERR_SHMSIZE           = (SQLITE_IOERR or (19 shl 8));
  SQLITE_IOERR_SHMLOCK           = (SQLITE_IOERR or (20 shl 8));
  SQLITE_IOERR_SHMMAP            = (SQLITE_IOERR or (21 shl 8));
  SQLITE_IOERR_SEEK              = (SQLITE_IOERR or (22 shl 8));
  SQLITE_IOERR_DELETE_NOENT      = (SQLITE_IOERR or (23 shl 8));
  SQLITE_IOERR_MMAP              = (SQLITE_IOERR or (24 shl 8));
  SQLITE_IOERR_GETTEMPPATH       = (SQLITE_IOERR or (25 shl 8));
  SQLITE_IOERR_CONVPATH          = (SQLITE_IOERR or (26 shl 8));
  SQLITE_IOERR_VNODE             = (SQLITE_IOERR or (27 shl 8));
  SQLITE_IOERR_AUTH              = (SQLITE_IOERR or (28 shl 8));
  SQLITE_LOCKED_SHAREDCACHE      = (SQLITE_LOCKED or  (1 shl 8));
  SQLITE_BUSY_RECOVERY           = (SQLITE_BUSY   or  (1 shl 8));
  SQLITE_BUSY_SNAPSHOT           = (SQLITE_BUSY   or  (2 shl 8));
  SQLITE_CANTOPEN_NOTEMPDIR      = (SQLITE_CANTOPEN or (1 shl 8));
  SQLITE_CANTOPEN_ISDIR          = (SQLITE_CANTOPEN or (2 shl 8));
  SQLITE_CANTOPEN_FULLPATH       = (SQLITE_CANTOPEN or (3 shl 8));
  SQLITE_CANTOPEN_CONVPATH       = (SQLITE_CANTOPEN or (4 shl 8));
  SQLITE_CORRUPT_VTAB            = (SQLITE_CORRUPT or (1 shl 8));
  SQLITE_READONLY_RECOVERY       = (SQLITE_READONLY or (1 shl 8));
  SQLITE_READONLY_CANTLOCK       = (SQLITE_READONLY or (2 shl 8));
  SQLITE_READONLY_ROLLBACK       = (SQLITE_READONLY or (3 shl 8));
  SQLITE_READONLY_DBMOVED        = (SQLITE_READONLY or (4 shl 8));
  SQLITE_ABORT_ROLLBACK          = (SQLITE_ABORT or (2 shl 8));
  SQLITE_CONSTRAINT_CHECK        = (SQLITE_CONSTRAINT or (1 shl 8));
  SQLITE_CONSTRAINT_COMMITHOOK   = (SQLITE_CONSTRAINT or (2 shl 8));
  SQLITE_CONSTRAINT_FOREIGNKEY   = (SQLITE_CONSTRAINT or (3 shl 8));
  SQLITE_CONSTRAINT_FUNCTION     = (SQLITE_CONSTRAINT or (4 shl 8));
  SQLITE_CONSTRAINT_NOTNULL      = (SQLITE_CONSTRAINT or (5 shl 8));
  SQLITE_CONSTRAINT_PRIMARYKEY   = (SQLITE_CONSTRAINT or (6 shl 8));
  SQLITE_CONSTRAINT_TRIGGER      = (SQLITE_CONSTRAINT or (7 shl 8));
  SQLITE_CONSTRAINT_UNIQUE       = (SQLITE_CONSTRAINT or (8 shl 8));
  SQLITE_CONSTRAINT_VTAB         = (SQLITE_CONSTRAINT or (9 shl 8));
  SQLITE_CONSTRAINT_ROWID        = (SQLITE_CONSTRAINT or (10 shl 8));
  SQLITE_NOTICE_RECOVER_WAL      = (SQLITE_NOTICE or (1 shl 8));
  SQLITE_NOTICE_RECOVER_ROLLBACK = (SQLITE_NOTICE or (2 shl 8));
  SQLITE_WARNING_AUTOINDEX       = (SQLITE_WARNING or (1 shl 8));
  SQLITE_AUTH_USER               = (SQLITE_AUTH or (1 shl 8));
  SQLITE_OK_LOAD_PERMANENTLY     = (SQLITE_OK or (1 shl 8));

  SQLITE_OPEN_READONLY         = $00000001;
  SQLITE_OPEN_READWRITE        = $00000002;
  SQLITE_OPEN_CREATE           = $00000004;
  SQLITE_OPEN_DELETEONCLOSE    = $00000008;
  SQLITE_OPEN_EXCLUSIVE        = $00000010;
  SQLITE_OPEN_AUTOPROXY        = $00000020;  (* VFS only *)
  SQLITE_OPEN_URI              = $00000040;  (* Ok for sqlite3_open_v2() *)
  SQLITE_OPEN_MEMORY           = $00000080;  (* Ok for sqlite3_open_v2() *)
  SQLITE_OPEN_MAIN_DB          = $00000100;
  SQLITE_OPEN_TEMP_DB          = $00000200;
  SQLITE_OPEN_TRANSIENT_DB     = $00000400;
  SQLITE_OPEN_MAIN_JOURNAL     = $00000800;
  SQLITE_OPEN_TEMP_JOURNAL     = $00001000;
  SQLITE_OPEN_SUBJOURNAL       = $00002000;
  SQLITE_OPEN_MASTER_JOURNAL   = $00004000;
  SQLITE_OPEN_NOMUTEX          = $00008000;
  SQLITE_OPEN_FULLMUTEX        = $00010000;
  SQLITE_OPEN_SHAREDCACHE      = $00020000;
  SQLITE_OPEN_PRIVATECACHE     = $00040000;
  SQLITE_OPEN_WAL              = $00080000;

  SQLITE_IOCAP_ATOMIC          = $00000001;
  SQLITE_IOCAP_ATOMIC512       = $00000002;
  SQLITE_IOCAP_ATOMIC1K        = $00000004;
  SQLITE_IOCAP_ATOMIC2K        = $00000008;
  SQLITE_IOCAP_ATOMIC4K        = $00000010;
  SQLITE_IOCAP_ATOMIC8K        = $00000020;
  SQLITE_IOCAP_ATOMIC16K       = $00000040;
  SQLITE_IOCAP_ATOMIC32K       = $00000080;
  SQLITE_IOCAP_ATOMIC64K       = $00000100;
  SQLITE_IOCAP_SAFE_APPEND     = $00000200;
  SQLITE_IOCAP_SEQUENTIAL      = $00000400;
  SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN  = $00000800;
  SQLITE_IOCAP_POWERSAFE_OVERWRITE    = $00001000;
  SQLITE_IOCAP_IMMUTABLE              = $00002000;

  SQLITE_LOCK_NONE          = 0;
  SQLITE_LOCK_SHARED        = 1;
  SQLITE_LOCK_RESERVED      = 2;
  SQLITE_LOCK_PENDING       = 3;
  SQLITE_LOCK_EXCLUSIVE     = 4;

  SQLITE_SYNC_NORMAL        = $00002;
  SQLITE_SYNC_FULL          = $00003;
  SQLITE_SYNC_DATAONLY      = $00010;

const
  SQLITE_FCNTL_LOCKSTATE           = 1;
  SQLITE_FCNTL_GET_LOCKPROXYFILE   = 2;
  SQLITE_FCNTL_SET_LOCKPROXYFILE   = 3;
  SQLITE_FCNTL_LAST_ERRNO          = 4;
  SQLITE_FCNTL_SIZE_HINT           = 5;
  SQLITE_FCNTL_CHUNK_SIZE          = 6;
  SQLITE_FCNTL_FILE_POINTER        = 7;
  SQLITE_FCNTL_SYNC_OMITTED        = 8;
  SQLITE_FCNTL_WIN32_AV_RETRY      = 9;
  SQLITE_FCNTL_PERSIST_WAL         = 10;
  SQLITE_FCNTL_OVERWRITE           = 11;
  SQLITE_FCNTL_VFSNAME             = 12;
  SQLITE_FCNTL_POWERSAFE_OVERWRITE = 13;
  SQLITE_FCNTL_PRAGMA              = 14;
  SQLITE_FCNTL_BUSYHANDLER         = 15;
  SQLITE_FCNTL_TEMPFILENAME        = 16;
  SQLITE_FCNTL_MMAP_SIZE           = 18;
  SQLITE_FCNTL_TRACE               = 19;
  SQLITE_FCNTL_HAS_MOVED           = 20;
  SQLITE_FCNTL_SYNC                = 21;
  SQLITE_FCNTL_COMMIT_PHASETWO     = 22;
  SQLITE_FCNTL_WIN32_SET_HANDLE    = 23;
  SQLITE_FCNTL_WAL_BLOCK           = 24;
  SQLITE_FCNTL_ZIPVFS              = 25;
  SQLITE_FCNTL_RBU                 = 26;
  SQLITE_FCNTL_VFS_POINTER         = 27;
  SQLITE_FCNTL_JOURNAL_POINTER     = 28;

  (* deprecated names *)
  SQLITE_GET_LOCKPROXYFILE    = SQLITE_FCNTL_GET_LOCKPROXYFILE;
  SQLITE_SET_LOCKPROXYFILE    = SQLITE_FCNTL_SET_LOCKPROXYFILE;
  SQLITE_LAST_ERRNO           = SQLITE_FCNTL_LAST_ERRNO;

const
  SQLITE_ACCESS_EXISTS    = 0;
  SQLITE_ACCESS_READWRITE = 1;
  SQLITE_ACCESS_READ      = 2;
  SQLITE_SHM_UNLOCK       = 1;
  SQLITE_SHM_LOCK         = 2;
  SQLITE_SHM_SHARED       = 4;
  SQLITE_SHM_EXCLUSIVE    = 8;
  SQLITE_SHM_NLOCK        = 8;

const
  SQLITE_CONFIG_SINGLETHREAD = 1;
  SQLITE_CONFIG_MULTITHREAD  = 2;
  SQLITE_CONFIG_SERIALIZED   = 3;
  SQLITE_CONFIG_MALLOC       = 4;
  SQLITE_CONFIG_GETMALLOC    = 5;
  SQLITE_CONFIG_SCRATCH      = 6;
  SQLITE_CONFIG_PAGECACHE    = 7;
  SQLITE_CONFIG_HEAP         = 8;
  SQLITE_CONFIG_MEMSTATUS    = 9;
  SQLITE_CONFIG_MUTEX        = 10;
  SQLITE_CONFIG_GETMUTEX     = 11;
  SQLITE_CONFIG_LOOKASIDE    = 13;
  SQLITE_CONFIG_PCACHE       = 14;
  SQLITE_CONFIG_GETPCACHE    = 15;
  SQLITE_CONFIG_LOG          = 16;
  SQLITE_CONFIG_URI          = 17;
  SQLITE_CONFIG_PCACHE2      = 18;  (* sqlite3_pcache_methods2* *)
  SQLITE_CONFIG_GETPCACHE2   = 19;  (* sqlite3_pcache_methods2* *)
  SQLITE_CONFIG_COVERING_INDEX_SCAN = 20;  (* int *)
  SQLITE_CONFIG_SQLLOG              = 21;  (* xSqllog, void* *)
  SQLITE_CONFIG_MMAP_SIZE           = 22;  (* sqlite3_int64, sqlite3_int64 *)
  SQLITE_CONFIG_WIN32_HEAPSIZE      = 23;  (* int nByte *)
  SQLITE_CONFIG_PCACHE_HDRSZ        = 24;  (* int *psz *)
  SQLITE_CONFIG_PMASZ               = 25;  (* unsigned int szPma *)
  SQLITE_CONFIG_STMTJRNL_SPILL      = 26;  (* int nByte *)

  SQLITE_DBCONFIG_LOOKASIDE      = 1001;
  SQLITE_DBCONFIG_ENABLE_FKEY    = 1002;
  SQLITE_DBCONFIG_ENABLE_TRIGGER = 1003;
  SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER = 1004;
  SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION = 1005;

const
  SQLITE_DENY   = 1;   (* Abort the SQL statement with an error *)
  SQLITE_IGNORE = 2;   (* Don't allow access, but don't generate an error *)

const
  SQLITE_CREATE_INDEX         =  1;   (* Index Name      Table Name      *)
  SQLITE_CREATE_TABLE         =  2;   (* Table Name      NULL            *)
  SQLITE_CREATE_TEMP_INDEX    =  3;   (* Index Name      Table Name      *)
  SQLITE_CREATE_TEMP_TABLE    =  4;   (* Table Name      NULL            *)
  SQLITE_CREATE_TEMP_TRIGGER  =  5;   (* Trigger Name    Table Name      *)
  SQLITE_CREATE_TEMP_VIEW     =  6;   (* View Name       NULL            *)
  SQLITE_CREATE_TRIGGER       =  7;   (* Trigger Name    Table Name      *)
  SQLITE_CREATE_VIEW          =  8;   (* View Name       NULL            *)
  SQLITE_DELETE               =  9;   (* Table Name      NULL            *)
  SQLITE_DROP_INDEX           = 10;   (* Index Name      Table Name      *)
  SQLITE_DROP_TABLE           = 11;   (* Table Name      NULL            *)
  SQLITE_DROP_TEMP_INDEX      = 12;   (* Index Name      Table Name      *)
  SQLITE_DROP_TEMP_TABLE      = 13;   (* Table Name      NULL            *)
  SQLITE_DROP_TEMP_TRIGGER    = 14;   (* Trigger Name    Table Name      *)
  SQLITE_DROP_TEMP_VIEW       = 15;   (* View Name       NULL            *)
  SQLITE_DROP_TRIGGER         = 16;   (* Trigger Name    Table Name      *)
  SQLITE_DROP_VIEW            = 17;   (* View Name       NULL            *)
  SQLITE_INSERT               = 18;   (* Table Name      NULL            *)
  SQLITE_PRAGMA               = 19;   (* Pragma Name     1st arg or NULL *)
  SQLITE_READ                 = 20;   (* Table Name      Column Name     *)
  SQLITE_SELECT               = 21;   (* NULL            NULL            *)
  SQLITE_TRANSACTION          = 22;   (* NULL            NULL            *)
  SQLITE_UPDATE               = 23;   (* Table Name      Column Name     *)
  SQLITE_ATTACH               = 24;   (* Filename        NULL            *)
  SQLITE_DETACH               = 25;   (* Database Name   NULL            *)
  SQLITE_ALTER_TABLE          = 26;   (* Database Name   Table Name      *)
  SQLITE_REINDEX              = 27;   (* Index Name      NULL            *)
  SQLITE_ANALYZE              = 28;   (* Table Name      NULL            *)
  SQLITE_CREATE_VTABLE        = 29;   (* Table Name      Module Name     *)
  SQLITE_DROP_VTABLE          = 30;   (* Table Name      Module Name     *)
  SQLITE_FUNCTION             = 31;   (* Function Name   NULL            *)
  SQLITE_SAVEPOINT            = 32;   (* Operation       Savepoint Name  *)
  SQLITE_COPY                 =  0;   (* No longer used *)
  SQLITE_RECURSIVE            = 33;   (* NULL            NULL            *)

const
  SQLITE_LIMIT_LENGTH                    = 0;
  SQLITE_LIMIT_SQL_LENGTH                = 1;
  SQLITE_LIMIT_COLUMN                    = 2;
  SQLITE_LIMIT_EXPR_DEPTH                = 3;
  SQLITE_LIMIT_COMPOUND_SELECT           = 4;
  SQLITE_LIMIT_VDBE_OP                   = 5;
  SQLITE_LIMIT_FUNCTION_ARG              = 6;
  SQLITE_LIMIT_ATTACHED                  = 7;
  SQLITE_LIMIT_LIKE_PATTERN_LENGTH       = 8;
  SQLITE_LIMIT_VARIABLE_NUMBER           = 9;
  SQLITE_LIMIT_TRIGGER_DEPTH             = 10;
  SQLITE_LIMIT_WORKER_THREADS            = 11;

const
  SQLITE_INTEGER  = 1;
  SQLITE_FLOAT    = 2;
  SQLITE_BLOB     = 4;
  SQLITE_NULL     = 5;
  SQLITE_TEXT     = 3;
  SQLITE3_TEXT    = 3;

const
  SQLITE_UTF8           = 1;
  SQLITE_UTF16LE        = 2;
  SQLITE_UTF16BE        = 3;
  SQLITE_UTF16          = 4;    (* Use native byte order *)
  SQLITE_ANY            = 5;    (* Deprecated *)
  SQLITE_UTF16_ALIGNED  = 8;    (* sqlite3_create_collation only *)

  SQLITE_DETERMINISTIC  = $800;

const
  SQLITE_INDEX_SCAN_UNIQUE      = 1;
  SQLITE_INDEX_CONSTRAINT_EQ    = 2;
  SQLITE_INDEX_CONSTRAINT_GT    = 4;
  SQLITE_INDEX_CONSTRAINT_LE    = 8;
  SQLITE_INDEX_CONSTRAINT_LT    = 16;
  SQLITE_INDEX_CONSTRAINT_GE    = 32;
  SQLITE_INDEX_CONSTRAINT_MATCH = 64;
  SQLITE_INDEX_CONSTRAINT_LIKE  = 65;
  SQLITE_INDEX_CONSTRAINT_GLOB  = 66;
  SQLITE_INDEX_CONSTRAINT_REGEXP= 67;


const
  SQLITE_MUTEX_FAST             = 0;
  SQLITE_MUTEX_RECURSIVE        = 1;
  SQLITE_MUTEX_STATIC_MASTER    = 2;
  SQLITE_MUTEX_STATIC_MEM       = 3;  (* sqlite3_malloc() *)
  SQLITE_MUTEX_STATIC_MEM2      = 4;  (* sqlite3_release_memory() *)
  SQLITE_MUTEX_STATIC_PRNG      = 5;  (* sqlite3_random() *)
  SQLITE_MUTEX_STATIC_LRU       = 6;  (* lru page list *)
  SQLITE_MUTEX_STATIC_LRU2      = 7;  (* lru page list *)
  SQLITE_MUTEX_STATIC_APP1      = 8;  (* For use by application *)
  SQLITE_MUTEX_STATIC_APP2      = 9;  (* For use by application *)
  SQLITE_MUTEX_STATIC_APP3      = 10; (* For use by application *)
  SQLITE_MUTEX_STATIC_VFS1      = 11; (* For use by built-in VFS *)
  SQLITE_MUTEX_STATIC_VFS2      = 12; (* For use by extension VFS *)
  SQLITE_MUTEX_STATIC_VFS3      = 13; (* For use by application VFS *)

const
  SQLITE_TESTCTRL_FAULT_CONFIG             = 1;
  SQLITE_TESTCTRL_FAULT_FAILURES           = 2;
  SQLITE_TESTCTRL_FAULT_BENIGN_FAILURES    = 3;
  SQLITE_TESTCTRL_FAULT_PENDING            = 4;
  SQLITE_TESTCTRL_PRNG_SAVE                = 5;
  SQLITE_TESTCTRL_PRNG_RESTORE             = 6;
  SQLITE_TESTCTRL_PRNG_RESET               = 7;
  SQLITE_TESTCTRL_BITVEC_TEST              = 8;
  SQLITE_TESTCTRL_FAULT_INSTALL            = 9;
  SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS      = 10;
  SQLITE_TESTCTRL_PENDING_BYTE             = 11;
  SQLITE_TESTCTRL_ASSERT                   = 12;
  SQLITE_TESTCTRL_ALWAYS                   = 13;
  SQLITE_TESTCTRL_RESERVE                  = 14;
  SQLITE_TESTCTRL_OPTIMIZATIONS            = 15;
  SQLITE_TESTCTRL_ISKEYWORD                = 16;
  SQLITE_TESTCTRL_SCRATCHMALLOC            = 17;
  SQLITE_TESTCTRL_LOCALTIME_FAULT          = 18;
  SQLITE_TESTCTRL_EXPLAIN_STMT             = 19;  (* NOT USED *)
  SQLITE_TESTCTRL_NEVER_CORRUPT            = 20;
  SQLITE_TESTCTRL_VDBE_COVERAGE            = 21;
  SQLITE_TESTCTRL_BYTEORDER                = 22;
  SQLITE_TESTCTRL_ISINIT                   = 23;
  SQLITE_TESTCTRL_SORTER_MMAP              = 24;
  SQLITE_TESTCTRL_IMPOSTER                 = 25;
  SQLITE_TESTCTRL_LAST                     = 25;

  SQLITE_STATUS_MEMORY_USED          = 0;
  SQLITE_STATUS_PAGECACHE_USED       = 1;
  SQLITE_STATUS_PAGECACHE_OVERFLOW   = 2;
  SQLITE_STATUS_SCRATCH_USED         = 3;
  SQLITE_STATUS_SCRATCH_OVERFLOW     = 4;
  SQLITE_STATUS_MALLOC_SIZE          = 5;
  SQLITE_STATUS_PARSER_STACK         = 6;
  SQLITE_STATUS_PAGECACHE_SIZE       = 7;
  SQLITE_STATUS_SCRATCH_SIZE         = 8;
  SQLITE_STATUS_MALLOC_COUNT         = 9;

  SQLITE_DBSTATUS_LOOKASIDE_USED      = 0;
  SQLITE_DBSTATUS_CACHE_USED          = 1;
  SQLITE_DBSTATUS_SCHEMA_USED         = 2;
  SQLITE_DBSTATUS_STMT_USED           = 3;
  SQLITE_DBSTATUS_LOOKASIDE_HIT       = 4;
  SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE = 5;
  SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL = 6;
  SQLITE_DBSTATUS_CACHE_HIT           = 7;
  SQLITE_DBSTATUS_CACHE_MISS          = 8;
  SQLITE_DBSTATUS_CACHE_WRITE         = 9;
  SQLITE_DBSTATUS_DEFERRED_FKS        = 10;
  SQLITE_DBSTATUS_CACHE_USED_SHARED   = 11;
  SQLITE_DBSTATUS_MAX                 = 11;   (* Largest defined DBSTATUS *)

const
  SQLITE_CHECKPOINT_PASSIVE  = 0;  (* Do as much as possible w/o blocking *)
  SQLITE_CHECKPOINT_FULL     = 1;  (* Wait for writers, then checkpoint *)
  SQLITE_CHECKPOINT_RESTART  = 2;  (* Like FULL but wait for for readers *)
  SQLITE_CHECKPOINT_TRUNCATE = 3;  (* Like RESTART but also truncate WAL *)

var
  sqlite3_set_authorizer: function(
    db: psqlite3;
    cb: xAuth;
    pUserData: pointer
 ): cint; cdecl;

  sqlite3_libversion: function: pansichar; cdecl;
  sqlite3_libversion_number: function: cint; cdecl;
  sqlite3_threadsafe: function(): cint; cdecl;

  sqlite3_close: function(ref: psqlite3): cint; cdecl;
  sqlite3_close_v2: function(ref: psqlite3): cint; cdecl;

  sqlite3_exec: function(
    db: psqlite3;                              (* An open database *)
    sql: pansichar;                            (* SQL to be evaluted *)
    cb: sqlite3_callback;                      (* Callback function *)
    user: pointer;                             (* 1st argument to callback *)
    errmsg: ppansichar                         (* Error msg written here *)
 ): cint; cdecl;

  sqlite3_initialize : Function : cint; cdecl;
  sqlite3_shutdown : Function : cint; cdecl;
  sqlite3_os_init : Function : cint; cdecl;
  sqlite3_os_end : Function : cint; cdecl;
  sqlite3_config : function (a : cint): cint; cdecl;
  sqlite3_db_config : function (f : psqlite3; op : cint): cint; cdecl;

  sqlite3_extended_Result_codes: function(db: psqlite3; onoff: cint): cint; cdecl;
  sqlite3_last_insert_rowid: function(db: psqlite3): sqlite3_int64; cdecl;
  sqlite3_changes: function(db: psqlite3): cint; cdecl;
  sqlite3_total_changes: function(db: psqlite3): cint; cdecl;
  sqlite3_interrupt: procedure(db: psqlite3); cdecl;
  sqlite3_complete: function(sql: pansichar): cint; cdecl;
  sqlite3_complete16: function(sql: pansichar): cint; cdecl;

  sqlite3_busy_handler: function(db: psqlite3; cb: busyhandler_callback; user: pointer): cint; cdecl;
  sqlite3_busy_timeout: function(db: psqlite3; ms: cint): cint; cdecl;
  sqlite3_get_table: function(
    db: psqlite3;          (* An open database *)
    sql: pansichar;        (* SQL to be evaluated *)
    pResult: pppansichar;  (* Results of the query *)
    nrow: pcint;           (* Number of Result rows written here *)
    ncolumn: pcint;        (* Number of Result columns written here *)
    errmsg: ppansichar     (* Error msg written here *)
 ): cint; cdecl;

  sqlite3_free_table: procedure(Result: ppansichar); cdecl;
  //char *sqlite3_mprintf(const char*,...);
  //char *sqlite3_vmprintf(const char*, va_list);
  //char *sqlite3_snprintf(int,char*,const char*, ...);

  sqlite3_malloc: function(size: cint): pointer;cdecl;
  sqlite3_malloc64: function(size: sqlite3_uint64): pointer;cdecl;
  sqlite3_realloc: function(ptr: pointer; size: cint): pointer;cdecl;
  sqlite3_realloc64: function(ptr: pointer; size: sqlite3_uint64): pointer;cdecl;
  sqlite3_free: procedure(ptr: pointer);cdecl;
  sqlite3_msize: function(ptr: pointer): sqlite3_uint64;cdecl;

  sqlite3_memory_used: function(): sqlite3_int64; cdecl;
  sqlite3_memory_highwater: function(resetFlag: cint): sqlite3_int64; cdecl;

  sqlite3_randomness: procedure(N: cint; P: pointer); cdecl;

  sqlite3_trace: function(db: psqlite3; cb: xTrace; user: pointer): pointer; cdecl;
  sqlite3_profile: function(db: psqlite3; cb: xProfile; user: pointer): pointer; cdecl;
  sqlite3_progress_handler: procedure(db: psqlite3; i: cint; cb: progress_callback; user: pointer); cdecl;

  sqlite3_open: function(
    filename: pansichar;    (* Database filename (UTF-8) *)
    ppDb: ppsqlite3         (* OUT: SQLite db handle *)
 ): cint; cdecl;

  sqlite3_open16: function(
    filename: pwidechar;    (* Database filename (UTF-16) *)
    ppDb: ppsqlite3         (* OUT: SQLite db handle *)
 ): cint; cdecl;

  sqlite3_open_v2: function(
    filename: pansichar;    (* Database filename (UTF-8) *)
    ppDb: ppsqlite3;        (* OUT: SQLite db handle *)
    flags: cint;            (* Flags *)
    zVfs: pansichar         (* Name of VFS module to use *)
 ): cint; cdecl;

  sqlite3_uri_parameter: function(zFilename : pansichar; zParam: pansichar) : pansichar; cdecl;
  sqlite3_uri_boolean: function(zFile : pansichar; zParam :pansichar; bDefault: cint) : cint; cdecl;
  sqlite3_uri_int64: function(zFile : pansichar; zParam :pansichar; iDefault: sqlite3_int64) : sqlite3_int64; cdecl;

  // uv
  sqlite3_errcode: function(db: psqlite3): cint; cdecl;
  sqlite3_extended_errcode: function(db: psqlite3): cint; cdecl;
  sqlite3_errmsg: function(db: psqlite3): pansichar; cdecl;
  sqlite3_errmsg16: function(db: psqlite3): pwidechar; cdecl;
  sqlite3_errstr: function(errCode: cint): pansichar; cdecl;
  sqlite3_limit: function(db: psqlite3; id: cint; newVal: cint): cint; cdecl;

  sqlite3_prepare: function(
    db: psqlite3;             (* Database handle *)
    zSql: pansichar;          (* SQL statement, UTF-8 encoded *)
    nByte: cint;              (* Maximum length of zSql in bytes. *)
    ppStmt: ppsqlite3_stmt;   (* OUT: Statement handle *)
    pzTail: ppansichar        (* OUT: Pointer to unused portion of zSql *)
 ): cint; cdecl;

  sqlite3_prepare_v2: function(
    db: psqlite3;             (* Database handle *)
    zSql: pansichar;          (* SQL statement, UTF-8 encoded *)
    nByte: cint;              (* Maximum length of zSql in bytes. *)
    ppStmt: ppsqlite3_stmt;   (* OUT: Statement handle *)
    pzTail: ppansichar        (* OUT: Pointer to unused portion of zSql *)
 ): cint; cdecl;

  sqlite3_prepare16: function(
    db: psqlite3;             (* Database handle *)
    zSql: pwidechar;          (* SQL statement, UTF-16 encoded *)
    nByte: cint;              (* Maximum length of zSql in bytes. *)
    ppStmt: ppsqlite3_stmt;   (* OUT: Statement handle *)
    pzTail: ppwidechar        (* OUT: Pointer to unused portion of zSql *)
 ): cint; cdecl;

  sqlite3_prepare16_v2: function(
    db: psqlite3;             (* Database handle *)
    zSql: pwidechar;          (* SQL statement, UTF-16 encoded *)
    nByte: cint;              (* Maximum length of zSql in bytes. *)
    ppStmt: ppsqlite3_stmt;   (* OUT: Statement handle *)
    pzTail: ppwidechar        (* OUT: Pointer to unused portion of zSql *)
 ): cint; cdecl;


  sqlite3_sql: function(pStmt: psqlite3_stmt): pansichar; cdecl;
  sqlite3_expanded_sql: function(pStmt: psqlite3_stmt): pansichar; cdecl;

  sqlite3_bind_blob: function(stmt: psqlite3_stmt; N: cint; V: pointer; L: cint; D: sqlite3_destructor_type): cint; cdecl;
  sqlite3_bind_blob64: function(stmt: psqlite3_stmt; N: cint; V: pointer; L: sqlite3_uint64; D: sqlite3_destructor_type): cint; cdecl;
  sqlite3_bind_double: function(stmt: psqlite3_stmt; N: cint; V: cdouble): cint; cdecl;
  sqlite3_bind_int: function(stmt: psqlite3_stmt; N: cint; V: cint): cint; cdecl;
  sqlite3_bind_int64: function(stmt: psqlite3_stmt; N: cint; V: sqlite3_int64): cint; cdecl;
  sqlite3_bind_null: function(stmt: psqlite3_stmt; N: cint): cint; cdecl;
  sqlite3_bind_text: function(stmt: psqlite3_stmt; N: cint; V: pansichar; L: cint; D: sqlite3_destructor_type): cint; cdecl;
  sqlite3_bind_text16: function(stmt: psqlite3_stmt; N: cint; V: pwidechar; L: cint; D: sqlite3_destructor_type): cint; cdecl;
  sqlite3_bind_text64: function(stmt: psqlite3_stmt; N: cint; V: pansichar; L: sqlite3_uint64; D: sqlite3_destructor_type; encoding: cuchar): cint; cdecl;
  sqlite3_bind_value: function(stmt: psqlite3_stmt; N: cint; V: psqlite3_value): cint; cdecl;
  sqlite3_bind_zeroblob: function(stmt: psqlite3_stmt; N: cint; V: cint): cint; cdecl;
  sqlite3_bind_zeroblob64: function(stmt: psqlite3_stmt; N: cint; V: sqlite3_uint64): cint; cdecl;
  sqlite3_bind_parameter_count: function(stmt: psqlite3_stmt): cint; cdecl;
  sqlite3_bind_parameter_name: function(stmt: psqlite3_stmt; N: cint): pansichar; cdecl;
  sqlite3_bind_parameter_index: function(stmt: psqlite3_stmt; zName: pansichar): cint; cdecl;
  sqlite3_clear_bindings: function(stmt: psqlite3_stmt): cint; cdecl;
  sqlite3_column_count: function(stmt: psqlite3_stmt): cint; cdecl;
  sqlite3_column_name: function(stmt: psqlite3_stmt; N: cint): pansichar; cdecl;
  sqlite3_column_name16: function(stmt: psqlite3_stmt; N: cint): pwidechar; cdecl;
  sqlite3_column_database_name: function(stmt: psqlite3_stmt; N: cint): pansichar; cdecl;
  sqlite3_column_database_name16: function(stmt: psqlite3_stmt; N: cint): pwidechar; cdecl;
  sqlite3_column_table_name: function(stmt: psqlite3_stmt; N: cint): pansichar; cdecl;
  sqlite3_column_table_name16: function(stmt: psqlite3_stmt; N: cint): pwidechar; cdecl;
  sqlite3_column_origin_name: function(stmt: psqlite3_stmt; N: cint): pansichar; cdecl;
  sqlite3_column_origin_name16: function(stmt: psqlite3_stmt; N: cint): pwidechar; cdecl;
  sqlite3_column_decltype: function(stmt: psqlite3_stmt; N: cint): pansichar; cdecl;
  sqlite3_column_decltype16: function(stmt: psqlite3_stmt; N: cint): pwidechar; cdecl;
  sqlite3_step: function(stmt: psqlite3_stmt): cint; cdecl;
  sqlite3_data_count: function(stmt: psqlite3_stmt): cint; cdecl;

  sqlite3_column_blob: function(stmt: psqlite3_stmt; iCol: cint): pointer; cdecl;
  sqlite3_column_bytes: function(stmt: psqlite3_stmt; iCol: cint): cint; cdecl;
  sqlite3_column_bytes16: function(stmt: psqlite3_stmt; iCol: cint): cint; cdecl;
  sqlite3_column_double: function(stmt: psqlite3_stmt; iCol: cint): cdouble; cdecl;
  sqlite3_column_int: function(stmt: psqlite3_stmt; iCol: cint): cint; cdecl;
  sqlite3_column_int64: function(stmt: psqlite3_stmt; iCol: cint): sqlite3_int64; cdecl;
  sqlite3_column_text: function(stmt: psqlite3_stmt; iCol: cint): pansichar; cdecl;
  sqlite3_column_text16: function(stmt: psqlite3_stmt; iCol: cint): pwidechar; cdecl;
  sqlite3_column_type: function(stmt: psqlite3_stmt; iCol: cint): cint; cdecl;
  sqlite3_column_value: function(stmt: psqlite3_stmt; iCol: cint): psqlite3_value; cdecl;
  sqlite3_finalize: function(stmt: psqlite3_stmt): cint; cdecl;
  sqlite3_reset: function(stmt: psqlite3_stmt): cint; cdecl;

  sqlite3_create_function: function(
    db: psqlite3;
    zFunctionName: pansichar;
    nArg: cint;
    eTextRep: cint;
    pApp: pointer;
    funccb: xFunc;
    stepcb: xStep;
    finalcb: xFinal
 ): cint; cdecl;

  sqlite3_create_function16: function(
    db: psqlite3;
    zFunctionName: pwidechar;
    nArg: cint;
    eTextRep: cint;
    pApp: pointer;
    funccb: xFunc;
    stepcb: xStep;
    finalcb: xFinal
 ): cint; cdecl;

  sqlite3_create_function_v2: function(
    db: psqlite3;
    zFunctionName: pansichar;
    nArg: cint;
    eTextRep: cint;
    pApp: pointer;
    funccb: xFunc;
    stepcb: xStep;
    finalcb: xFinal;
    destroycb : xDestroy
 ): cint; cdecl;

{$IFDEF SQLITE_OBSOLETE}
  sqlite3_aggregate_count: function(ctx: psqlite3_context): cint; cdecl;
  sqlite3_expired: function(stmt: psqlite3_stmt): cint; cdecl;
  sqlite3_transfer_bindings: function(stmt: psqlite3_stmt; stmt2: psqlite3_stmt): cint; cdecl;
  sqlite3_global_recover: function(): cint; cdecl;
  sqlite3_thread_cleanup: procedure(); cdecl;
  sqlite3_memory_alarm: function(cb: memory_alarm_cb; user: pointer; i64: sqlite3_int64): cint; cdecl;
{$ENDIF}

  sqlite3_value_blob: function(val: psqlite3_value): pointer; cdecl;
  sqlite3_value_bytes: function(val: psqlite3_value): cint; cdecl;
  sqlite3_value_bytes16: function(val: psqlite3_value): cint; cdecl;
  sqlite3_value_double: function(val: psqlite3_value): cdouble; cdecl;
  sqlite3_value_int: function(val: psqlite3_value): cint; cdecl;
  sqlite3_value_int64: function(val: psqlite3_value): sqlite3_int64; cdecl;
  sqlite3_value_text: function(val: psqlite3_value): pansichar; cdecl;
  sqlite3_value_text16: function(val: psqlite3_value): pwidechar; cdecl;
  sqlite3_value_text16le: function(val: psqlite3_value): pwidechar; cdecl;
  sqlite3_value_text16be: function(val: psqlite3_value): pwidechar; cdecl;
  sqlite3_value_type: function(val: psqlite3_value): cint; cdecl;
  sqlite3_value_numeric_type: function(val: psqlite3_value): cint; cdecl;
  sqlite3_value_subtype: function(val: psqlite3_value): cuint; cdecl;
  sqlite3_value_dup: function(val: psqlite3_value): psqlite3_value; cdecl;
  sqlite3_value_free: procedure(val: psqlite3_value); cdecl;

  sqlite3_aggregate_context: function(ctx: psqlite3_context; nBytes: cint): pointer; cdecl;
  sqlite3_user_data: function(ctx: psqlite3_context): pointer; cdecl;
  sqlite3_context_db_handle: function(ctx: psqlite3_context): psqlite3; cdecl;

  sqlite3_get_auxdata: function(ctx: psqlite3_context; N: cint): pointer; cdecl;
  sqlite3_set_auxdata: procedure(ctx: psqlite3_context; N: cint; P: pointer; cb: set_auxdata_cb); cdecl;
  sqlite3_Result_blob: procedure(ctx: psqlite3_context; V: pointer; N: cint; D: sqlite3_destructor_type); cdecl;
  sqlite3_Result_blob64: procedure(ctx: psqlite3_context; V: pointer; N: sqlite3_uint64; D: sqlite3_destructor_type); cdecl;
  sqlite3_Result_double: procedure(ctx: psqlite3_context; V: cdouble); cdecl;
  sqlite3_Result_error: procedure(ctx: psqlite3_context; V: pansichar; N: cint); cdecl;
  sqlite3_Result_error16: procedure(ctx: psqlite3_context; V: pwidechar; N: cint); cdecl;
  sqlite3_Result_error_toobig: procedure(ctx: psqlite3_context); cdecl;
  sqlite3_Result_error_nomem: procedure(ctx: psqlite3_context); cdecl;
  sqlite3_Result_error_code: procedure(ctx: psqlite3_context; V: cint); cdecl;
  sqlite3_Result_int: procedure(ctx: psqlite3_context; V: cint); cdecl;
  sqlite3_Result_int64: procedure(ctx: psqlite3_context; V: sqlite3_int64); cdecl;
  sqlite3_Result_null: procedure(ctx: psqlite3_context); cdecl;
  sqlite3_Result_text: procedure(ctx: psqlite3_context; V: pansichar; N: cint; D: sqlite3_destructor_type); cdecl;
  sqlite3_Result_text64: procedure(ctx: psqlite3_context; V: pansichar; N: sqlite3_uint64; D: sqlite3_destructor_type; encoding: cuchar); cdecl;
  sqlite3_Result_text16: procedure(ctx: psqlite3_context; V: pwidechar; N: cint; D: sqlite3_destructor_type); cdecl;
  sqlite3_Result_text16le: procedure(ctx: psqlite3_context; V: pwidechar; N: cint; D: sqlite3_destructor_type); cdecl;
  sqlite3_Result_text16be: procedure(ctx: psqlite3_context; V: pwidechar; N: cint; D: sqlite3_destructor_type); cdecl;
  sqlite3_Result_value: procedure(ctx: psqlite3_context; V: psqlite3_value); cdecl;
  sqlite3_Result_zeroblob: procedure(ctx: psqlite3_context; N: cint); cdecl;
  sqlite3_Result_zeroblob64: function(ctx: psqlite3_context; N: sqlite3_uint64): cint; cdecl;

  sqlite3_create_collation: function(
    db: psqlite3;
    zName: pansichar;
    eTextRep: cint;
    user: pointer;
    xcomparecb: xCompare
 ): cint; cdecl;

  sqlite3_create_collation_v2: function(
    db: psqlite3;
    zName: pansichar;
    eTextRep: cint;
    user: pointer;
    xcomparecb: xCompare;
    xdestroycb: xDestroy
 ): cint; cdecl;

  sqlite3_create_collation16: function(
    db: psqlite3;
    zName: pwidechar;
    eTextRep: cint;
    user: pointer;
    xcomparecb: xCompare
 ): cint; cdecl;

  sqlite3_collation_needed: function(
    db: psqlite3;
    user: pointer;
    cb: collation_needed_cb
 ): cint; cdecl;

  sqlite3_collation_needed16: function(
    db: psqlite3;
    user: pointer;
    cb: collation_needed_cb
 ): cint; cdecl;

  sqlite3_key: function(
    db: psqlite3;                   (* Database to be rekeyed *)
    pKey: pointer; nKey: cint       (* The key *)
 ): cint; cdecl;

  sqlite3_key_v2: function(
    db: psqlite3;                   (* Database to be rekeyed *)
    zDbName: pansichar;             (* Name of the database *)
    pKey: pointer; nKey: cint       (* The key *)
 ): cint; cdecl;

  sqlite3_rekey: function(
    db: psqlite3;                   (* Database to be rekeyed *)
    pKey: pointer; nKey: cint       (* The new key *)
 ): cint; cdecl;

  sqlite3_rekey_v2: function(
    db: psqlite3;                   (* Database to be rekeyed *)
    zDbName: pansichar;             (* Name of the database *)
    pKey: pointer; nKey: cint       (* The new key *)
 ): cint; cdecl;

  sqlite3_sleep: function(M: cint): cint; cdecl;

  sqlite3_get_autocommit: function(db: psqlite3): cint; cdecl;
  sqlite3_db_handle: function(stmt: psqlite3_stmt): psqlite3; cdecl;
  sqlite3_db_filename: function(db: psqlite3; zDbName: pansichar): pansichar; cdecl;
  sqlite3_db_readonly: function(db: psqlite3; zDbName: pansichar): cint; cdecl;
  sqlite3_next_stmt: function(db: psqlite3;stmt: psqlite3_stmt):psqlite3_stmt;cdecl;

  sqlite3_commit_hook: function(db: psqlite3; cb: commit_callback; user: pointer): pointer; cdecl;
  sqlite3_rollback_hook: function(db: psqlite3; cb: sqlite3_destructor_type; user: pointer): pointer; cdecl;


  sqlite3_update_hook: function(db: psqlite3; cb: update_callback; user: pointer): pointer; cdecl;
  sqlite3_enable_shared_cache: function(B: cint): cint; cdecl;
  sqlite3_release_memory: function(N: cint): cint; cdecl;
  sqlite3_db_release_memory: function(db: psqlite3): cint; cdecl;
  sqlite3_soft_heap_limit: procedure(N: cint); cdecl;
  sqlite3_soft_heap_limit64: function(N: int64):int64;cdecl;

  sqlite3_table_column_metadata: function(
    db: psqlite3;             (* Connection handle *)
    zDbName: pansichar;           (* Database name or NULL *)
    zTableName: pansichar;        (* Table name *)
    zColumnName: pansichar;       (* Column name *)
    pzDataType: ppansichar;       (* OUTPUT: Declared data type *)
    pzCollSeq: ppansichar;        (* OUTPUT: Collation sequence name *)
    pNotNull: pcint;          (* OUTPUT: True if NOT NULL constracint exists *)
    pPrimaryKey: pcint;       (* OUTPUT: True if column part of PK *)
    pAutoinc: pcint           (* OUTPUT: True if column is auto-increment *)
 ): cint; cdecl;

  sqlite3_load_extension: function(
    db: psqlite3;          (* Load the extension cinto this database connection *)
    zFile: pansichar;          (* Name of the shared library containing extension *)
    zProc: pansichar;          (* Entry point.  Derived from zFile if 0 *)
    pzErrMsg: ppansichar       (* Put error message here if not 0 *)
 ): cint; cdecl;
  sqlite3_enable_load_extension: function(db: psqlite3; onoff: cint): cint; cdecl;
  sqlite3_auto_extension: function(xEntrypoint: pointer): cint; cdecl;
  sqlite3_cancel_auto_extension: function(xEntrypoint: pointer): cint; cdecl;
  sqlite3_reset_auto_extension: procedure(); cdecl;

  sqlite3_create_module: function(
    db: psqlite3;               (* SQLite connection to register module with *)
    zName: pansichar;           (* Name of the module *)
    module: psqlite3_module;    (* Methods for the module *)
    user: pointer               (* Client data for xCreate/xConnect *)
 ): cint; cdecl;

  sqlite3_create_module_v2: function(
    db: psqlite3;               (* SQLite connection to register module with *)
    zName: pansichar;           (* Name of the module *)
    module: psqlite3_module;    (* Methods for the module *)
    user: pointer;              (* Client data for xCreate/xConnect *)
    xdestroycb: xDestroy        (* Module destructor function *)
 ): cint; cdecl;

  sqlite3_declare_vtab: function(db: psqlite3; zCreateTable: pansichar): cint; cdecl;
  sqlite3_overload_function: function(db: psqlite3; zFuncName: pansichar; nArg: cint): cint; cdecl;

  sqlite3_blob_open: function(
    db: psqlite3;
    zDb: pansichar;
    zTable: pansichar;
    zColumn: pansichar;
    iRow: sqlite3_int64;
    flags: cint;
    ppBlob: ppsqlite3_blob
 ): cint; cdecl;

  sqlite3_blob_reopen: function(blob: psqlite3_blob;p:sqlite3_int64): cint; cdecl;
  sqlite3_blob_close: function(blob: psqlite3_blob): cint; cdecl;
  sqlite3_blob_bytes: function(blob: psqlite3_blob): cint; cdecl;
  sqlite3_blob_read: function(blob: psqlite3_blob; Z: pointer; N: cint; iOffset: cint): cint; cdecl;
  sqlite3_blob_write: function(blob: psqlite3_blob; Z: pointer; N: cint; iOffset: cint): cint; cdecl;
  sqlite3_vfs_find: function(zVfsName: pansichar): psqlite3_vfs; cdecl;
  sqlite3_vfs_register: function(vfs: psqlite3_vfs; makeDflt: cint): cint; cdecl;
  sqlite3_vfs_unregister: function(vfs: psqlite3_vfs): cint; cdecl;
  sqlite3_mutex_alloc: function(n: cint): psqlite3_mutex; cdecl;
  sqlite3_mutex_free: procedure(mtx: psqlite3_mutex); cdecl;
  sqlite3_mutex_ente: procedure(mtx: psqlite3_mutex); cdecl;
  sqlite3_mutex_try: function(mtx: psqlite3_mutex): cint; cdecl;
  sqlite3_mutex_leave: procedure(mtx: psqlite3_mutex); cdecl;
  sqlite3_mutex_held: function(mtx: psqlite3_mutex): cint; cdecl;
  sqlite3_mutex_notheld: function(mtx: psqlite3_mutex): cint; cdecl;
  sqlite3_db_mutex: function(db: psqlite3): psqlite3_mutex; cdecl;

  sqlite3_file_control: function(db: psqlite3; zDbName: pansichar; op: cint; p: pointer): cint; cdecl;
  sqlite3_test_control: function(op: cint; args: array of const): cint; cdecl;

  sqlite3_status: function(op: cint; pCurrent: pcint; pHighwater: pcint; resetFlag: cint): cint; cdecl;
  sqlite3_status64: function(op: cint; pCurrent: psqlite3_int64; pHighwater: psqlite3_int64; resetFlag: cint): cint; cdecl;
  sqlite3_db_status: function(db : psqlite3;op: cint; pCurrent:pcint; pHighwater: pcint; resetFlag: cint): cint; cdecl;
  sqlite3_stmt_status: function(stmt: psqlite3_stmt;op: cint; pcurrent:pcint; pHighwater: pcint; resetFlag: cint): cint; cdecl;

  {Backup api}
  sqlite3_backup_init: function(pDest: psqlite3; const zDestName: pansichar; pSource: psqlite3; const zSourceName: pansichar): psqlite3backup; cdecl;
  sqlite3_backup_step: function(p: psqlite3backup; nPage: Integer): Integer; cdecl;
  sqlite3_backup_finish: function(p: psqlite3backup): Integer; cdecl;
  sqlite3_backup_remaining: function(p: psqlite3backup): Integer; cdecl;
  sqlite3_backup_pagecount: function(p: psqlite3backup): Integer; cdecl;

  sqlite3_unlock_notify: function(pBlocked:psqlite3;xNotify: xNotifycb;arg:pointer):cint;cdecl;
  sqlite3_log: procedure(iErrCode:cint;fmt : pansichar); cdecl;

  sqlite3_wal_hook: function(db:psqlite3;cb : wal_hook_cb; p: pointer): pointer;cdecl;
  sqlite3_wal_autocheckpoint: function(db:psqlite3;n : cint): cint;cdecl;
  sqlite3_wal_checkpoint: function(db:psqlite3;zDB: pansichar): cint;cdecl;
  sqlite3_wal_checkpoint_v2: function(db:psqlite3;zDB: pansichar;emode:cint;nLog:pcint;nCkpt:pcint): cint;cdecl;

  {String handling api}
  sqlite3_strglob :function(zGlob, zStr: pansichar): cint; cdecl;
  sqlite3_strlike :function(zGlob, zStr: pansichar; cEsc: cuint): cint; cdecl;

type
  TSQLiteRecord = class
  private
    FItems: array of Variant;
    function GetCount: Integer;
    function GetValue(Idx: Integer): Variant;
    procedure SetCount(const Value: Integer);
    procedure SetValue(Idx: Integer; const Value: Variant);
  public
    property Count: Integer read GetCount write SetCount;
    property Value[Idx: Integer]: Variant read GetValue write SetValue; default;
  end;

  TSQLiteRecords = TObjectList< TSQLiteRecord >;

  TSQLiteResult = class
  private
    FRecords: TSQLiteRecords;
    FColumns: TStrings;
    FCurrentRow: Integer;
    function GetValue(Row: Integer; Column: string): Variant;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Empty: Boolean;
    function Clone: TSQLiteResult;

    property Columns: TStrings read FColumns;
    property Records: TSQLiteRecords read FRecords;
    property Values[ Row: Integer; Column: string ]: Variant read GetValue; default;
    property CurrentRow: Integer read FCurrentRow write FCurrentRow;
  end;

  TSQLite = class
  private
    FDBFileName: string;
    FDB: psqlite3;
    FSQLiteResult: TSQLiteResult;
  public
    constructor Create(DBFileName: string);
    destructor Destroy; override;

    function Open: Boolean;
    procedure Close;
    function DBQuery(Sql: string; Params: array of const): Boolean;

    property SQLiteResult: TSQLiteResult read FSQLiteResult;
  end;

function sqlite3_version(): pansichar;

//function InitializeSqliteANSI(const LibraryName: AnsiString = ''): Integer; //needed as TLibraryLoadFunction
function InitializeSqlite(const LibraryName: string = ''): Integer;
function TryInitializeSqlite(const ALibraryName: string = ''): Integer;
procedure ReleaseSqlite; //needed as TLibraryUnLoadFunction

var
  SQLiteLibraryHandle: TLibHandle;
  SQLiteLoadedLibrary: string;

implementation

uses
  Variants, StrUtils;

function sqlite3_version(): pansichar;
begin
  Result := sqlite3_libversion();
end;

resourcestring
  SErrLoadFailed     = 'Can not load SQLite client library "%s". Check your installation.';
  SErrAlreadyLoaded  = 'SQLite interface already initialized from library %s.';

procedure LoadAddresses(LibHandle: TLibHandle);
begin
  sqlite3_libversion := GetProcAddress(LibHandle,'sqlite3_libversion');
  sqlite3_libversion_number := GetProcAddress(LibHandle,'sqlite3_libversion_number');
  sqlite3_threadsafe := GetProcAddress(LibHandle,'sqlite3_threadsafe');
  sqlite3_close := GetProcAddress(LibHandle,'sqlite3_close');
  sqlite3_close_v2 := GetProcAddress(LibHandle,'sqlite3_close_v2');
  sqlite3_exec := GetProcAddress(LibHandle,'sqlite3_exec');
  sqlite3_extended_Result_codes := GetProcAddress(LibHandle,'sqlite3_extended_Result_codes');
  sqlite3_last_insert_rowid := GetProcAddress(LibHandle,'sqlite3_last_insert_rowid');
  sqlite3_changes := GetProcAddress(LibHandle,'sqlite3_changes');
  sqlite3_total_changes := GetProcAddress(LibHandle,'sqlite3_total_changes');
  sqlite3_complete := GetProcAddress(LibHandle,'sqlite3_complete');
  sqlite3_complete16 := GetProcAddress(LibHandle,'sqlite3_complete16');
  sqlite3_busy_handler := GetProcAddress(LibHandle,'sqlite3_busy_handler');
  sqlite3_busy_timeout := GetProcAddress(LibHandle,'sqlite3_busy_timeout');
  sqlite3_get_table := GetProcAddress(LibHandle,'sqlite3_get_table');
  sqlite3_malloc := GetProcAddress(LibHandle,'sqlite3_malloc');
  sqlite3_realloc := GetProcAddress(LibHandle,'sqlite3_realloc');
  sqlite3_memory_used := GetProcAddress(LibHandle,'sqlite3_memory_used');
  sqlite3_memory_highwater := GetProcAddress(LibHandle,'sqlite3_memory_highwater');
  sqlite3_set_authorizer := GetProcAddress(LibHandle,'sqlite3_set_authorizer');
  sqlite3_trace := GetProcAddress(LibHandle,'sqlite3_trace');
  sqlite3_profile := GetProcAddress(LibHandle,'sqlite3_profile');
  sqlite3_open := GetProcAddress(LibHandle,'sqlite3_open');
  sqlite3_open16 := GetProcAddress(LibHandle,'sqlite3_open16');
  sqlite3_open_v2 := GetProcAddress(LibHandle,'sqlite3_open_v2');
  sqlite3_errcode := GetProcAddress(LibHandle,'sqlite3_errcode');
  sqlite3_extended_errcode := GetProcAddress(LibHandle,'sqlite3_extended_errcode');
  sqlite3_errmsg := GetProcAddress(LibHandle,'sqlite3_errmsg');
  sqlite3_errmsg16 := GetProcAddress(LibHandle,'sqlite3_errmsg16');
  sqlite3_errstr := GetProcAddress(LibHandle,'sqlite3_errstr');
  sqlite3_limit := GetProcAddress(LibHandle,'sqlite3_limit');
  sqlite3_prepare := GetProcAddress(LibHandle,'sqlite3_prepare');
  sqlite3_prepare_v2 := GetProcAddress(LibHandle,'sqlite3_prepare_v2');
  sqlite3_prepare16 := GetProcAddress(LibHandle,'sqlite3_prepare16');
  sqlite3_prepare16_v2 := GetProcAddress(LibHandle,'sqlite3_prepare16_v2');
  sqlite3_sql := GetProcAddress(LibHandle,'sqlite3_sql');
  sqlite3_expanded_sql := GetProcAddress(LibHandle,'sqlite3_expanded_sql');
  sqlite3_bind_blob := GetProcAddress(LibHandle,'sqlite3_bind_blob');
  sqlite3_bind_blob64 := GetProcAddress(LibHandle,'sqlite3_bind_blob64');
  sqlite3_bind_double := GetProcAddress(LibHandle,'sqlite3_bind_double');
  sqlite3_bind_int := GetProcAddress(LibHandle,'sqlite3_bind_int');
  sqlite3_bind_int64 := GetProcAddress(LibHandle,'sqlite3_bind_int64');
  sqlite3_bind_null := GetProcAddress(LibHandle,'sqlite3_bind_null');
  sqlite3_bind_text := GetProcAddress(LibHandle,'sqlite3_bind_text');
  sqlite3_bind_text64 := GetProcAddress(LibHandle,'sqlite3_bind_text64');
  sqlite3_bind_text16 := GetProcAddress(LibHandle,'sqlite3_bind_text16');
  sqlite3_bind_value := GetProcAddress(LibHandle,'sqlite3_bind_value');
  sqlite3_bind_zeroblob := GetProcAddress(LibHandle,'sqlite3_bind_zeroblob');
  sqlite3_bind_zeroblob64 := GetProcAddress(LibHandle,'sqlite3_bind_zeroblob64');
  sqlite3_bind_parameter_count := GetProcAddress(LibHandle,'sqlite3_bind_parameter_count');
  sqlite3_bind_parameter_name := GetProcAddress(LibHandle,'sqlite3_bind_parameter_name');
  sqlite3_bind_parameter_index := GetProcAddress(LibHandle,'sqlite3_bind_parameter_index');
  sqlite3_clear_bindings := GetProcAddress(LibHandle,'sqlite3_clear_bindings');
  sqlite3_column_count := GetProcAddress(LibHandle,'sqlite3_column_count');
  sqlite3_column_name := GetProcAddress(LibHandle,'sqlite3_column_name');
  sqlite3_column_name16 := GetProcAddress(LibHandle,'sqlite3_column_name16');
  sqlite3_column_database_name := GetProcAddress(LibHandle,'sqlite3_column_database_name');
  sqlite3_column_database_name16 := GetProcAddress(LibHandle,'sqlite3_column_database_name16');
  sqlite3_column_table_name := GetProcAddress(LibHandle,'sqlite3_column_table_name');
  sqlite3_column_table_name16 := GetProcAddress(LibHandle,'sqlite3_column_table_name16');
  sqlite3_column_origin_name := GetProcAddress(LibHandle,'sqlite3_column_origin_name');
  sqlite3_column_origin_name16 := GetProcAddress(LibHandle,'sqlite3_column_origin_name16');
  sqlite3_column_decltype := GetProcAddress(LibHandle,'sqlite3_column_decltype');
  sqlite3_column_decltype16 := GetProcAddress(LibHandle,'sqlite3_column_decltype16');
  sqlite3_step := GetProcAddress(LibHandle,'sqlite3_step');
  sqlite3_data_count := GetProcAddress(LibHandle,'sqlite3_data_count');
  sqlite3_column_blob := GetProcAddress(LibHandle,'sqlite3_column_blob');
  sqlite3_column_bytes := GetProcAddress(LibHandle,'sqlite3_column_bytes');
  sqlite3_column_bytes16 := GetProcAddress(LibHandle,'sqlite3_column_bytes16');
  sqlite3_column_double := GetProcAddress(LibHandle,'sqlite3_column_double');
  sqlite3_column_int := GetProcAddress(LibHandle,'sqlite3_column_int');
  sqlite3_column_int64 := GetProcAddress(LibHandle,'sqlite3_column_int64');
  sqlite3_column_text := GetProcAddress(LibHandle,'sqlite3_column_text');
  sqlite3_column_text16 := GetProcAddress(LibHandle,'sqlite3_column_text16');
  sqlite3_column_type := GetProcAddress(LibHandle,'sqlite3_column_type');
  sqlite3_column_value := GetProcAddress(LibHandle,'sqlite3_column_value');
  sqlite3_finalize := GetProcAddress(LibHandle,'sqlite3_finalize');
  sqlite3_reset := GetProcAddress(LibHandle,'sqlite3_reset');
  sqlite3_create_function := GetProcAddress(LibHandle,'sqlite3_create_function');
  sqlite3_create_function16 := GetProcAddress(LibHandle,'sqlite3_create_function16');
  sqlite3_create_function_v2 := GetProcAddress(LibHandle,'sqlite3_create_function_v2');
  sqlite3_value_blob := GetProcAddress(LibHandle,'sqlite3_value_blob');
  sqlite3_value_bytes := GetProcAddress(LibHandle,'sqlite3_value_bytes');
  sqlite3_value_bytes16 := GetProcAddress(LibHandle,'sqlite3_value_bytes16');
  sqlite3_value_double := GetProcAddress(LibHandle,'sqlite3_value_double');
  sqlite3_value_int := GetProcAddress(LibHandle,'sqlite3_value_int');
  sqlite3_value_int64 := GetProcAddress(LibHandle,'sqlite3_value_int64');
  sqlite3_value_text := GetProcAddress(LibHandle,'sqlite3_value_text');
  sqlite3_value_text16 := GetProcAddress(LibHandle,'sqlite3_value_text16');
  sqlite3_value_text16le := GetProcAddress(LibHandle,'sqlite3_value_text16le');
  sqlite3_value_text16be := GetProcAddress(LibHandle,'sqlite3_value_text16be');
  sqlite3_value_type := GetProcAddress(LibHandle,'sqlite3_value_type');
  sqlite3_value_numeric_type := GetProcAddress(LibHandle,'sqlite3_value_numeric_type');
  sqlite3_value_subtype := GetProcAddress(LibHandle,'sqlite3_value_subtype');
  sqlite3_aggregate_context := GetProcAddress(LibHandle,'sqlite3_aggregate_context');
  sqlite3_user_data := GetProcAddress(LibHandle,'sqlite3_user_data');
  sqlite3_context_db_handle := GetProcAddress(LibHandle,'sqlite3_context_db_handle');
  sqlite3_get_auxdata := GetProcAddress(LibHandle,'sqlite3_get_auxdata');
  sqlite3_create_collation := GetProcAddress(LibHandle,'sqlite3_create_collation');
  sqlite3_create_collation_v2 := GetProcAddress(LibHandle,'sqlite3_create_collation_v2');
  sqlite3_create_collation16 := GetProcAddress(LibHandle,'sqlite3_create_collation16');
  sqlite3_collation_needed := GetProcAddress(LibHandle,'sqlite3_collation_needed');
  sqlite3_collation_needed16 := GetProcAddress(LibHandle,'sqlite3_collation_needed16');
  sqlite3_key := GetProcAddress(LibHandle,'sqlite3_key');
  sqlite3_key_v2 := GetProcAddress(LibHandle,'sqlite3_key_v2');
  sqlite3_rekey := GetProcAddress(LibHandle,'sqlite3_rekey');
  sqlite3_rekey_v2 := GetProcAddress(LibHandle,'sqlite3_rekey_v2');
  sqlite3_sleep := GetProcAddress(LibHandle,'sqlite3_sleep');
  sqlite3_get_autocommit := GetProcAddress(LibHandle,'sqlite3_get_autocommit');
  sqlite3_db_handle := GetProcAddress(LibHandle,'sqlite3_db_handle');
  sqlite3_commit_hook := GetProcAddress(LibHandle,'sqlite3_commit_hook');
  sqlite3_rollback_hook := GetProcAddress(LibHandle,'sqlite3_rollback_hook');
  sqlite3_update_hook := GetProcAddress(LibHandle,'sqlite3_update_hook');
  sqlite3_enable_shared_cache := GetProcAddress(LibHandle,'sqlite3_enable_shared_cache');
  sqlite3_release_memory := GetProcAddress(LibHandle,'sqlite3_release_memory');
  sqlite3_table_column_metadata := GetProcAddress(LibHandle,'sqlite3_table_column_metadata');
  sqlite3_load_extension := GetProcAddress(LibHandle,'sqlite3_load_extension');
  sqlite3_enable_load_extension := GetProcAddress(LibHandle,'sqlite3_enable_load_extension');
  sqlite3_auto_extension := GetProcAddress(LibHandle,'sqlite3_auto_extension');
  sqlite3_create_module := GetProcAddress(LibHandle,'sqlite3_create_module');
  sqlite3_create_module_v2 := GetProcAddress(LibHandle,'sqlite3_create_module_v2');
  sqlite3_declare_vtab := GetProcAddress(LibHandle,'sqlite3_declare_vtab');
  sqlite3_overload_function := GetProcAddress(LibHandle,'sqlite3_overload_function');
  sqlite3_blob_open := GetProcAddress(LibHandle,'sqlite3_blob_open');
  sqlite3_blob_reopen := GetProcAddress(LibHandle,'sqlite3_blob_reopen');
  sqlite3_blob_close := GetProcAddress(LibHandle,'sqlite3_blob_close');
  sqlite3_blob_bytes := GetProcAddress(LibHandle,'sqlite3_blob_bytes');
  sqlite3_blob_read := GetProcAddress(LibHandle,'sqlite3_blob_read');
  sqlite3_blob_write := GetProcAddress(LibHandle,'sqlite3_blob_write');
  sqlite3_vfs_find := GetProcAddress(LibHandle,'sqlite3_vfs_find');
  sqlite3_vfs_register := GetProcAddress(LibHandle,'sqlite3_vfs_register');
  sqlite3_vfs_unregister := GetProcAddress(LibHandle,'sqlite3_vfs_unregister');
  sqlite3_mutex_alloc := GetProcAddress(LibHandle,'sqlite3_mutex_alloc');
  sqlite3_mutex_try := GetProcAddress(LibHandle,'sqlite3_mutex_try');
  sqlite3_mutex_held := GetProcAddress(LibHandle,'sqlite3_mutex_held');
  sqlite3_mutex_notheld := GetProcAddress(LibHandle,'sqlite3_mutex_notheld');
  sqlite3_db_mutex := GetProcAddress(LibHandle,'sqlite3_db_mutex');
  sqlite3_file_control := GetProcAddress(LibHandle,'sqlite3_file_control');
  sqlite3_test_control := GetProcAddress(LibHandle,'sqlite3_test_control');
  sqlite3_status := GetProcAddress(LibHandle,'sqlite3_status');
  sqlite3_status64 := GetProcAddress(LibHandle,'sqlite3_status64');
  sqlite3_db_status := GetProcAddress(LibHandle,'sqlite3_db_status');
  sqlite3_stmt_status := GetProcAddress(LibHandle,'sqlite3_stmt_status');
  sqlite3_interrupt := GetProcAddress(LibHandle,'sqlite3_interrupt');
  sqlite3_free_table := GetProcAddress(LibHandle,'sqlite3_free_table');
  sqlite3_free := GetProcAddress(LibHandle,'sqlite3_free');
  sqlite3_randomness := GetProcAddress(LibHandle,'sqlite3_randomness');
  sqlite3_progress_handler := GetProcAddress(LibHandle,'sqlite3_progress_handler');
  sqlite3_set_auxdata := GetProcAddress(LibHandle,'sqlite3_set_auxdata');
  sqlite3_Result_blob := GetProcAddress(LibHandle,'sqlite3_Result_blob');
  sqlite3_Result_blob64 := GetProcAddress(LibHandle,'sqlite3_Result_blob64');
  sqlite3_Result_double := GetProcAddress(LibHandle,'sqlite3_Result_double');
  sqlite3_Result_error := GetProcAddress(LibHandle,'sqlite3_Result_error');
  sqlite3_Result_error16 := GetProcAddress(LibHandle,'sqlite3_Result_error16');
  sqlite3_Result_error_toobig := GetProcAddress(LibHandle,'sqlite3_Result_error_toobig');
  sqlite3_Result_error_nomem := GetProcAddress(LibHandle,'sqlite3_Result_error_nomem');
  sqlite3_Result_error_code := GetProcAddress(LibHandle,'sqlite3_Result_error_code');
  sqlite3_Result_int := GetProcAddress(LibHandle,'sqlite3_Result_int');
  sqlite3_Result_int64 := GetProcAddress(LibHandle,'sqlite3_Result_int64');
  sqlite3_Result_null := GetProcAddress(LibHandle,'sqlite3_Result_null');
  sqlite3_Result_text := GetProcAddress(LibHandle,'sqlite3_Result_text');
  sqlite3_Result_text64 := GetProcAddress(LibHandle,'sqlite3_Result_text64');
  sqlite3_Result_text16 := GetProcAddress(LibHandle,'sqlite3_Result_text16');
  sqlite3_Result_text16le := GetProcAddress(LibHandle,'sqlite3_Result_text16le');
  sqlite3_Result_text16be := GetProcAddress(LibHandle,'sqlite3_Result_text16be');
  sqlite3_Result_value := GetProcAddress(LibHandle,'sqlite3_Result_value');
  sqlite3_Result_zeroblob := GetProcAddress(LibHandle,'sqlite3_Result_zeroblob');
  sqlite3_Result_zeroblob64 := GetProcAddress(LibHandle,'sqlite3_Result_zeroblob64');
  sqlite3_soft_heap_limit := GetProcAddress(LibHandle,'sqlite3_soft_heap_limit');
  sqlite3_soft_heap_limit64 := GetProcAddress(LibHandle,'sqlite3_soft_heap_limit64');
  sqlite3_reset_auto_extension := GetProcAddress(LibHandle,'sqlite3_reset_auto_extension');
  sqlite3_mutex_free := GetProcAddress(LibHandle,'sqlite3_mutex_free');
  sqlite3_mutex_ente := GetProcAddress(LibHandle,'sqlite3_mutex_ente');
  sqlite3_mutex_leave := GetProcAddress(LibHandle,'sqlite3_mutex_leave');
  sqlite3_backup_init := GetProcAddress(LibHandle,'sqlite3_backup_init');
  sqlite3_backup_step := GetProcAddress(LibHandle,'sqlite3_backup_step');
  sqlite3_backup_finish := GetProcAddress(LibHandle,'sqlite3_backup_finish');
  sqlite3_backup_remaining := GetProcAddress(LibHandle,'sqlite3_backup_remaining');
  sqlite3_backup_pagecount := GetProcAddress(LibHandle,'sqlite3_backup_pagecount');
  sqlite3_unlock_notify := GetProcAddress(LibHandle,'sqlite3_unlock_notify');
  sqlite3_log := GetProcAddress(LibHandle,'sqlite3_log');
  sqlite3_wal_hook := GetProcAddress(LibHandle,'sqlite3_wal_hook');
  sqlite3_wal_autocheckpoint := GetProcAddress(LibHandle,'sqlite3_wal_autocheckpoint');
  sqlite3_wal_checkpoint := GetProcAddress(LibHandle,'sqlite3_wal_checkpoint');
  sqlite3_wal_checkpoint_v2 := GetProcAddress(LibHandle,'sqlite3_wal_checkpoint_v2');
  sqlite3_strlike := GetProcAddress(LibHandle,'sqlite3_strlike');

  sqlite3_initialize := GetProcAddress(LibHandle,'sqlite3_initialize');
  sqlite3_shutdown := GetProcAddress(LibHandle,'sqlite3_shutdown');
  sqlite3_os_init := GetProcAddress(LibHandle,'sqlite3_os_init');
  sqlite3_os_end := GetProcAddress(LibHandle,'sqlite3_os_end');
  sqlite3_config := GetProcAddress(LibHandle,'sqlite3_config');
  sqlite3_db_config := GetProcAddress(LibHandle,'sqlite3_db_config');
  sqlite3_uri_parameter := GetProcAddress(LibHandle,'sqlite3_uri_parameter');

{$IFDEF SQLITE_OBSOLETE}
  sqlite3_aggregate_count := GetProcAddress(LibHandle,'sqlite3_aggregate_count');
  sqlite3_expired := GetProcAddress(LibHandle,'sqlite3_expired');
  sqlite3_transfer_bindings := GetProcAddress(LibHandle,'sqlite3_transfer_bindings');
  sqlite3_global_recover := GetProcAddress(LibHandle,'sqlite3_global_recover');
  sqlite3_memory_alarm := GetProcAddress(LibHandle,'sqlite3_memory_alarm');
  sqlite3_thread_cleanup := GetProcAddress(LibHandle,'sqlite3_thread_cleanup');
{$ENDIF}
end;

var
  RefCount: Integer;

function TryInitializeSqlite(const ALibraryName: string): Integer;
var
  vLibraryName: string;
begin
  vLibraryName := IfThen(ALibraryName = '', cSqlite3Lib, ALibraryName);

  Result := InterlockedIncrement(RefCount);
  if Result = 1 then
  begin
    SQLiteLibraryHandle := LoadLibrary(PWideChar(vLibraryName));
    if (SQLiteLibraryHandle = 0) then
    begin
      RefCount := 0;
      Exit(-1);
    end;
    SQLiteLoadedLibrary := vLibraryName;
    LoadAddresses(SQLiteLibraryHandle);
  end;
end;

{function  InitializeSQLiteANSI(const LibraryName: AnsiString):integer;
begin
  Result:=InitializeSQLite(LibraryName);
end;}

function  InitializeSQLite(const LibraryName: string) :integer;
begin
  if (LibraryName<>'') and (SQLiteLoadedLibrary <> '') and (SQLiteLoadedLibrary <> LibraryName) then
    raise EInoutError.CreateFmt(SErrAlreadyLoaded,[SQLiteLoadedLibrary]);

  Result := TryInitializeSQLite(LibraryName);
  if Result=-1 then
    if LibraryName='' then
      raise EInOutError.CreateFmt(SErrLoadFailed, [cSqlite3Lib])
    else
      raise EInOutError.CreateFmt(SErrLoadFailed, [LibraryName]);
end;

procedure ReleaseSQLite;
begin
  if InterlockedDecrement(RefCount) <= 0 then
  begin
    if SQLiteLibraryHandle <> 0 then
      FreeLibrary(SQLiteLibraryHandle);
    SQLiteLibraryHandle := 0;
    SQLiteLoadedLibrary := '';
    RefCount := 0;
  end;
end;

procedure EmptyDestructor(user: pointer); cdecl;
begin
end;

// SQLite select Result
{function Callback(user: pointer; cols: cint; values, name: ppansichar): cint; cdecl;
var
  I: Integer;
  n, v: PAnsiChar;
  SQLiteResult: TSQLiteResult;
  Rec: TSQLiteRecord;
begin
  Result := 0; // OK
  if user = nil then Exit;
  SQLiteResult := TSQLite(user).FSQLiteResult;

  if SQLiteResult.Empty then
  begin
    // заполняем имена колонок
    for I := 0 to cols - 1 do
    begin
      n := name^;
      SQLiteResult.Columns.Add(AnsiString(n));
      inc(name);
    end;
  end;

  // значения
  Rec := TSQLiteRecord.Create;
  Rec.Count := cols;
  for I := 0 to cols - 1 do
  begin
    v := values^;
    Rec[I] := AnsiString(v);
    inc(values);
  end;
  SQLiteResult.Records.Add(Rec);
end;   }

{ TSQLiteResult }

procedure TSQLiteResult.Clear;
begin
  FColumns.Clear;
  FRecords.Clear;
  FCurrentRow := 0;
end;

function TSQLiteResult.Clone: TSQLiteResult;
var
  Rec, NewRec: TSQLiteRecord;
  I, J: Integer;
begin
  Result := TSQLiteResult.Create;
  Result.Columns.Assign(Self.Columns);
  for I := 0 to Self.Records.Count - 1 do
  begin
    Rec := Self.Records[I];
    NewRec := TSQLiteRecord.Create;
    NewRec.Count := Rec.Count;
    for J := 0 to Rec.Count - 1 do
      NewRec[J] := Rec[J];
    Result.Records.Add(NewRec);
  end;
end;

constructor TSQLiteResult.Create;
begin
  FColumns := TStringList.Create;
  FRecords := TSQLiteRecords.Create;
end;

destructor TSQLiteResult.Destroy;
begin
  FreeAndNil(FColumns);
  FreeAndNil(FRecords);
  inherited;
end;

function TSQLiteResult.Empty: Boolean;
begin
  Result := FColumns.Count = 0;
end;

function TSQLiteResult.GetValue(Row: Integer; Column: string): Variant;
var
  ColIdx: Integer;
  Rec: TSQLiteRecord;
begin
  Result := Null;
  if Empty then Exit;
  if (Row < 0) or (Row >= FRecords.Count) then Exit;
  if Column = '' then Exit;
  ColIdx := FColumns.IndexOf(Column);
  if ColIdx < 0 then Exit;
  Rec := FRecords[ Row ];
  Result := Rec[ ColIdx ];
end;

{ TSQLite }

procedure TSQLite.Close;
begin
  sqlite3_close(FDB);
end;

constructor TSQLite.Create(DBFileName: string);
var
  s: AnsiString;
begin
  FSQLiteResult := TSQLiteResult.Create;
  s := AnsiString(DBFileName);
  sqlite3_open(PAnsiChar(s), @FDB);
end;

function TSQLite.DBQuery(Sql: string; Params: array of const): Boolean;
var
  res, I, BindIdx, IntVal, Col, ColType, ColCount: Integer;
  Int64Val: Int64;
  FloatVal: Double;
  stm: psqlite3_stmt;
  tail, ColName: PAnsiChar;
  asql: AnsiString;
  s: WideString;
  v: Variant;
  vt: TVarType;
  Rec: TSQLiteRecord;
  ArgType, ArrLen: Integer;
  ResData, ArrData: PByte;
begin
  Result := False;
  FSQLiteResult.Clear;

  if FDB = nil then Exit;
  if Sql = '' then Exit;

  asql := AnsiString(sql);
  res := sqlite3_prepare(FDB, PAnsiChar(asql), Length(asql), @stm, @tail);
  if res <> SQLITE_OK then
  begin
    if tail <> nil then
      sqlite3_free(tail);
    Exit;
  end;
  try
    // связываем значения параметров
    for I := 0 to High(Params) do
    begin
      BindIdx := I + 1;
      s := '';
      with TVarRec(Params[I]) do
      begin
        ArgType := VType;
        case ArgType of
          vtBoolean,
          vtInteger:        sqlite3_bind_int(stm, BindIdx, VInteger);
          vtExtended:       sqlite3_bind_double(stm, BindIdx, VExtended^);
          vtInt64:          sqlite3_bind_int64(stm, BindIdx, vtInt64);

          vtString:         s := WideString(VString);
          vtPChar:          s := WideString(VPChar);
          vtPWideChar:      s := WideString(VPWideChar);
          vtAnsiString:     s := WideString(PAnsiChar(VAnsiString));
          vtWideString:     s := WideString(PWideChar(VWideString));
          vtUnicodeString:  s := WideString(PWideChar(VUnicodeString));

          vtVariant:
            begin
              v := VVariant^;
              vt := VarType(v);
              case vt of
                varSingle,
                varDouble,
                varDate:
                  begin
                    FloatVal := v;
                    sqlite3_bind_double(stm, BindIdx, FloatVal);
                  end;

                varSmallInt,
                varInteger,
                varBoolean,
                varShortInt,
                varByte,
                varWord,
                varUInt32:
                  begin
                    IntVal := v;
                    sqlite3_bind_int(stm, BindIdx, IntVal);
                  end;

                varInt64:
                  begin
                    Int64Val := v;
                    sqlite3_bind_int64(stm, BindIdx, Int64Val);
                  end;

                varOleStr,
                varUString,
                varString:   s := VarToStr(v);
              else
              end;
            end;
        else
          // vtChar, vtPointer, vtObject, vtClass, vtCurrency, vtInterface, vtWideChar
        end;
      end;
      // строковые данные
      if s <> '' then
      begin
        sqlite3_bind_text16(stm, BindIdx, PWideChar(s), ByteLength(s), EmptyDestructor);
      end;
    end;

    // выполняем
    res := sqlite3_step(stm);

    // insert, update, delete or empty select Result
    if res = SQLITE_DONE then
    begin
      Result := True;
      Exit;
    end;

    // select Result

    // columns
    if res = SQLITE_ROW then
    begin
      ColCount := sqlite3_column_count(stm);
      for Col := 0 to ColCount - 1 do
      begin
        ColName := sqlite3_column_name(stm, Col);
        FSQLiteResult.Columns.Add(string(AnsiString(ColName)));
      end;

      // data
      while res = SQLITE_ROW do
      begin
        Rec := TSQLiteRecord.Create;
        Rec.Count := ColCount;
        for Col := 0 to ColCount - 1 do
        begin
          ColType := sqlite3_column_type(stm, Col);
          case ColType of
            SQLITE_INTEGER: Rec[ Col ] := sqlite3_column_int(stm, col);
            SQLITE_FLOAT:   Rec[ Col ] := sqlite3_column_double(stm, col);
            SQLITE_TEXT:    Rec[ Col ] := WideString(sqlite3_column_text16(stm, col));
            SQLITE_NULL:    Rec[ Col ] := Null;
            SQLITE_BLOB:    // Rec[ Col ] := Null;
              begin
                ArrLen := sqlite3_column_bytes(stm, Col);
                v := VarArrayCreate([0, ArrLen - 1], varByte);
                ArrData := VarArrayLock(v);
                ResData := sqlite3_column_blob(stm, col);
                Move(ResData^, ArrData^, ArrLen);
                VarArrayUnlock(v);
                Rec[ Col ] := v;
              end;
          else
          end;
        end;
        FSQLiteResult.Records.Add(Rec);
        res := sqlite3_step(stm);
      end;
    end;
  finally
    sqlite3_finalize(stm);
  end;
  Result := True;
end;

destructor TSQLite.Destroy;
begin
  FSQLiteResult.Free;
  inherited;
end;

function TSQLite.Open: Boolean;
var
  s: AnsiString;
begin
  Result := False;
  if not FileExists(FDBFileName) then Exit;
  s := AnsiString(FDBFileName);
  Result := sqlite3_open(PAnsiChar(s), @FDB) = SQLITE_OK;
end;

{ TSQLiteRecord }

function TSQLiteRecord.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TSQLiteRecord.GetValue(Idx: Integer): Variant;
begin
  Result := Null;
  if (Idx < 0) or (Idx >= Count) then Exit;
  Result := FItems[ Idx ];
end;

procedure TSQLiteRecord.SetCount(const Value: Integer);
begin
  if Value < 0 then Exit;
  SetLength(FItems, Value);
end;

procedure TSQLiteRecord.SetValue(Idx: Integer; const Value: Variant);
begin
  if (Idx < 0) or (Idx >= Count) then Exit;
  FItems[ Idx ] := Value;
end;

initialization

InitializeSQLite;

finalization

ReleaseSqlite;

end.
