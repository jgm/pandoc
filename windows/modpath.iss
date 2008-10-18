// ----------------------------------------------------------------------------
//
// Inno Setup Ver:  5.2.1
// Script Version:  1.3.1
// Author:          Jared Breland <jbreland@legroom.net>
// Homepage:		http://www.legroom.net/software
//
// Modified 10/18/2008 by John MacFarlane to set path in HKCU instead of HKLM if
// user does not have admin privileges.  Also correctly handle the case where
// oldpath is empty.
//
// Script Function:
//	Enable modification of system path directly from Inno Setup installers
//
// Instructions:
//	Copy modpath.iss to the same directory as your setup script
//
//	Add this statement to your [Setup] section
//		ChangesEnvironment=yes
//
//	Add this statement to your [Tasks] section
//	You can change the Description or Flags, but the Name must be modifypath
//		Name: modifypath; Description: &Add application directory to your system path; Flags: unchecked
//
//	Add the following to the end of your [Code] section
//	setArrayLength must specify the total number of dirs to be added
//	Dir[0] contains first directory, Dir[1] contains second, etc.
//		function ModPathDir(): TArrayOfString;
//		var
//			Dir:	TArrayOfString;
//		begin
//			setArrayLength(Dir, 1)
//			Dir[0] := ExpandConstant('{app}');
//			Result := Dir;
//		end;
//		#include "modpath.iss"
// ----------------------------------------------------------------------------

procedure ModPath();
var
	oldpath:	String;
	newpath:	String;
	pathArr:	TArrayOfString;
	aExecFile:	String;
	aExecArr:	TArrayOfString;
	i, d:		Integer;
	pathdir:	TArrayOfString;
	reg: Integer;
  regkey: String;
begin

	// Get array of new directories and act on each individually
	pathdir := ModPathDir();
	for d := 0 to GetArrayLength(pathdir)-1 do begin

		// Modify WinNT path
		if UsingWinNT() = true then begin

      // Select HKLM or HKCU depending on whether user has admin privileges
      if (IsAdminLoggedOn or IsPowerUserLoggedOn) then begin
          reg := HKEY_LOCAL_MACHINE;
          regkey := 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment';
      end else begin
          reg := HKEY_CURRENT_USER;
          regkey := 'Environment';
      end;

			// Get current path, split into an array
			RegQueryStringValue(reg, regkey, 'Path', oldpath);
			oldpath := oldpath + ';';
			i := 0;
			while (Pos(';', oldpath) > 0) do begin
				SetArrayLength(pathArr, i+1);
				pathArr[i] := Copy(oldpath, 0, Pos(';', oldpath)-1);
				oldpath := Copy(oldpath, Pos(';', oldpath)+1, Length(oldpath));
				i := i + 1;

				// Check if current directory matches app dir
				if pathdir[d] = pathArr[i-1] then begin
					// if uninstalling, remove dir from path
					if IsUninstaller() = true then begin
						continue;
					// if installing, abort because dir was already in path
					end else begin
						abort;
					end;
				end;

				// Add current directory to new path
				if i = 1 then begin
					newpath := pathArr[i-1];
				end else begin
					newpath := newpath + ';' + pathArr[i-1];
				end;
			end;

			// Append app dir to path if not already included
			if IsUninstaller() = false then begin
				if newpath = '' then begin
					newpath := pathdir[d];
				end else begin
					newpath := newpath + ';' + pathdir[d];
				end;
			end;

			// Write new path
			RegWriteStringValue(reg, regkey, 'Path', newpath);

		// Modify Win9x path
		end else begin

			// Convert to shortened dirname
			pathdir[d] := GetShortName(pathdir[d]);

			// If autoexec.bat exists, check if app dir already exists in path
			aExecFile := 'C:\AUTOEXEC.BAT';
			if FileExists(aExecFile) then begin
				LoadStringsFromFile(aExecFile, aExecArr);
				for i := 0 to GetArrayLength(aExecArr)-1 do begin
					if IsUninstaller() = false then begin
						// If app dir already exists while installing, abort add
						if (Pos(pathdir[d], aExecArr[i]) > 0) then
							abort;
					end else begin
						// If app dir exists and = what we originally set, then delete at uninstall
						if aExecArr[i] = 'SET PATH=%PATH%;' + pathdir[d] then
							aExecArr[i] := '';
					end;
				end;
			end;

			// If app dir not found, or autoexec.bat didn't exist, then (create and) append to current path
			if IsUninstaller() = false then begin
				SaveStringToFile(aExecFile, #13#10 + 'SET PATH=%PATH%;' + pathdir[d], True);

			// If uninstalling, write the full autoexec out
			end else begin
				SaveStringsToFile(aExecFile, aExecArr, False);
			end;
		end;

		// Write file to flag modifypath was selected
		//   Workaround since IsTaskSelected() cannot be called at uninstall and AppName and AppId cannot be "read" in Code section
		if IsUninstaller() = false then
			SaveStringToFile(ExpandConstant('{app}') + '\uninsTasks.txt', WizardSelectedTasks(False), False);
	end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
	if CurStep = ssPostInstall then
		if IsTaskSelected('modifypath') then
			ModPath();
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
	appdir:			String;
	selectedTasks:	String;
begin
	appdir := ExpandConstant('{app}')
	if CurUninstallStep = usUninstall then begin
		if LoadStringFromFile(appdir + '\uninsTasks.txt', selectedTasks) then
			if Pos('modifypath', selectedTasks) > 0 then
				ModPath();
		DeleteFile(appdir + '\uninsTasks.txt')
	end;
end;

function NeedRestart(): Boolean;
begin
	if IsTaskSelected('modifypath') and not UsingWinNT() then begin
		Result := True;
	end else begin
		Result := False;
	end;
end;

