; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "Fate Explorer"
#define MyAppVersion "1.0 alpha"
#define MyAppPublisher "Praise Beauty Productions"
#define MyAppExeName "LaunchFateExplorer.vbs"
#define MainDirName "FateExplorer"

[Setup]
; NOTE: The value of AppId uniquely identifies this application. Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={BDD412E5-7C09-4CF1-9207-2FB40F1DFE7D}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
DefaultDirName={autopf}\{#MainDirName}
DisableProgramGroupPage=yes
LicenseFile=D:\Programmierung\R\DSADiceApp.git\LICENSE
; Uncomment the following line to run in non administrative install mode (install for current user only.)
;PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
OutputDir=D:\Programmierung\R\DSADiceApp.git\deploy\InnoSetup
OutputBaseFilename=InstallFateExplorer
Compression=lzma
SolidCompression=yes
WizardStyle=modern
UninstallDisplayIcon={app}\fe.ico


[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "D:\Programmierung\R\DSADiceApp.git\deploy\LaunchFateExplorer.vbs"; DestDir: "{app}"; Flags: ignoreversion
Source: "D:\Programmierung\R\DSADiceApp.git\deploy\FateExplorer.R"; DestDir: "{app}"; Flags: ignoreversion
Source: "D:\Programmierung\R\DSADiceApp.git\deploy\fe.ico"; DestDir: "{app}"; Flags: ignoreversion
Source: "D:\Programmierung\R\DSADiceApp.git\deploy\R-Portable\*"; DestDir: "{app}\R-Portable"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "D:\Programmierung\R\DSADiceApp.git\src\*"; DestDir: "{app}\src"; Flags: ignoreversion recursesubdirs createallsubdirs
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{autoprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\fe.ico"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\fe.ico"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: shellexec postinstall skipifsilent

