; AI-M.Capital

#define MyAppTitle "Конфигуратор"
#define MyAppName "Configurator"
#define MyAppVersion "0.1"
#define MyAppPublisher "Sensoft"
#define MyAppURL "https://sensoft.pro"
#define MyLowerCaseAppName LowerCase(MyAppName)
#define MyAppExeName "Configurator.exe"
#define MyBinDir "..\..\..\bin"
#define MyBuildDir MyBinDir + "\Win64"
#define MySolutionDir MyBinDir + "\solutions\" + MyLowerCaseAppName
#define MyOutputDir MyBinDir

#sub ChangeFileVersion
  #define DotCount = 0
  #define LocalAppVersion
  #for {LocalAppVersion = MyAppVersion; Pos('.', LocalAppVersion) > 0; Delete(LocalAppVersion, 1, Pos('.', LocalAppVersion))} DotCount = DotCount + 1

  #define StampFileVersion
  #for {StampFileVersion = MyAppVersion; DotCount < 3; DotCount++} StampFileVersion = StampFileVersion + ".0"

  #define StampVersionPath "C:\SetupTools\StampVer.exe"
  #define StampVersionParams "-f""" + StampFileVersion + """ -p""" + StampFileVersion + """ """ + SourcePath + MyBuildDir + "\" + MyAppExeName + """"

  #expr Exec(StampVersionPath, StampVersionParams)
#endsub

; Changing of File version and Product version
#expr ChangeFileVersion

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Create GUID inside the IDE.)
AppId={{461CEC61-A2BE-4539-B980-EEC2E2A24203}}
AppName={#MyAppTitle}
AppVersion={#MyAppVersion}  
AppVerName={#MyAppTitle} {#MyAppVersion}                     
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
ArchitecturesInstallIn64BitMode=x64
ArchitecturesAllowed=x64
DefaultDirName={autopf}\{#MyAppPublisher}\{#MyAppName}
DefaultGroupName={#MyAppTitle}
AllowNoIcons=yes
OutputDir={#MyOutputDir}
OutputBaseFilename=setup_{#MyLowerCaseAppName}_x64_{#MyAppVersion}
;SetupIconFile={#MyAppName}.ico 
Compression=lzma
SolidCompression=yes
LanguageDetectionMethod=locale
VersionInfoVersion={#MyAppVersion}
PrivilegesRequired=admin
SignTool=SensoftSign

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"

[Dirs]
Name: "{app}\Win64"; Permissions: users-modify; Flags: uninsalwaysuninstall
Name: "{app}\solutions\{#MyLowerCaseAppName}"; Permissions: users-modify; Flags: uninsalwaysuninstall
Name: "{app}\res"; Permissions: users-modify; Flags: uninsalwaysuninstall

[Files]
Source: "{#MyBuildDir}\{#MyAppExeName}"; DestDir: "{app}\Win64"; DestName: {#MyAppExeName}; Flags: ignoreversion sign
Source: "{#MyBuildDir}\ssleay32.dll"; DestDir: "{app}\Win64"; Flags: ignoreversion
Source: "{#MyBuildDir}\libeay32.dll"; DestDir: "{app}\Win64"; Flags: ignoreversion
Source: "{#MyBuildDir}\sqlite3.dll"; DestDir: "{app}\Win64"; Flags: ignoreversion
Source: "{#MyBinDir}\res\styles\default\icons\*.png"; DestDir: "{app}\res\styles\default\icons"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#MyBinDir}\res\translations\*.lng"; DestDir: "{app}\res\translations"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#MyBinDir}\res\images\*.*"; DestDir: "{app}\res\images"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#MyBinDir}\res\layouts\*.dfm"; DestDir: "{app}\res\layouts"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#MySolutionDir}\icons\*.png"; DestDir: "{app}\solutions\{#MyLowerCaseAppName}\icons"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#MySolutionDir}\images\*.png"; DestDir: "{app}\solutions\{#MyLowerCaseAppName}\images"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#MySolutionDir}\layouts\*.dfm"; DestDir: "{app}\solutions\{#MyLowerCaseAppName}\layouts"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#MySolutionDir}\translations\*.lng"; DestDir: "{app}\solutions\{#MyLowerCaseAppName}\translations"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#MySolutionDir}\templates\*.*"; DestDir: "{app}\solutions\{#MyLowerCaseAppName}\templates"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#MySolutionDir}\templates\layouts\*.*"; DestDir: "{app}\solutions\{#MyLowerCaseAppName}\templates\layouts"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#MySolutionDir}\favicon.ico"; DestDir: "{app}\solutions\{#MyLowerCaseAppName}"; Flags: ignoreversion skipifsourcedoesntexist

[Icons]
Name: "{group}\{#MyAppTitle} (64-бит)"; Filename: "{app}\Win64\{#MyAppExeName}"; IconFilename: "{app}\solutions\{#MyLowerCaseAppName}\favicon.ico"
Name: "{group}\{cm:UninstallProgram,{#MyAppTitle}}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#MyAppTitle} (64-бит)"; Filename: "{app}\Win64\{#MyAppExeName}"; Tasks: desktopicon; IconFilename: "{app}\solutions\{#MyLowerCaseAppName}\favicon.ico"

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "ru"; MessagesFile: "compiler:Languages\Russian.isl"

[Run]
Filename: "{app}\Win64\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppTitle, '&', '&&')}}"; Flags: postinstall

[UninstallDelete]
Type: files; Name: "{app}\solutions\{#MyLowerCaseAppName}\settings.ini"
Type: files; Name: "{app}\solutions\{#MyLowerCaseAppName}\local.db"

[INI]
Filename: "{app}\settings.ini"; Section: "Core"; Key: "RunMode"; String: "singleton"; Flags: uninsdeletesectionifempty uninsdeleteentry
Filename: "{app}\solutions\{#MyLowerCaseAppName}\settings.ini"; Section: "Core"; Key: "Deployment"; String: "prod"; Flags: createkeyifdoesntexist uninsdeletesectionifempty uninsdeleteentry
;Filename: "{app}\solutions\{#MyLowerCaseAppName}\settings.ini"; Section: "Core"; Key: "AutoLogin"; String: "1"; Flags: uninsdeletesectionifempty uninsdeleteentry
;Filename: "{app}\solutions\{#MyLowerCaseAppName}\settings.ini"; Section: "Core"; Key: "Layout"; String: "mdi"; Flags: uninsdeletesectionifempty uninsdeleteentry
Filename: "{app}\solutions\{#MyLowerCaseAppName}\settings.ini"; Section: "Core"; Key: "EnvironmentID"; String: ""; Flags: uninsdeletesectionifempty uninsdeleteentry
Filename: "{app}\solutions\{#MyLowerCaseAppName}\settings.ini"; Section: "Modules"; Key: "DataStorage"; String: "SQLite"; Flags: uninsdeletesectionifempty uninsdeleteentry
Filename: "{app}\solutions\{#MyLowerCaseAppName}\settings.ini"; Section: "Modules"; Key: "ReportEngine"; String: "FastReport"; Flags: uninsdeletesectionifempty uninsdeleteentry
Filename: "{app}\solutions\{#MyLowerCaseAppName}\settings.ini"; Section: "Modules"; Key: "ChartPainter"; String: "VCL"; Flags: uninsdeletesectionifempty uninsdeleteentry
Filename: "{app}\solutions\{#MyLowerCaseAppName}\settings.ini"; Section: "Modules"; Key: "UI"; String: "Windows.DevExpress"; Flags: uninsdeletesectionifempty uninsdeleteentry
Filename: "{app}\solutions\{#MyLowerCaseAppName}\settings.ini"; Section: "Modules"; Key: "TaskEngine"; String: "SimpleEngine"; Flags: uninsdeletesectionifempty uninsdeleteentry
Filename: "{app}\solutions\{#MyLowerCaseAppName}\settings.ini"; Section: "SQLite"; Key: "Database"; String: "local.db"; Flags: uninsdeletesectionifempty uninsdeleteentry
