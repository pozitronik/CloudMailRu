@call clean.cmd

@set _compiler=24w32

@if exist "MailRuCloudResource.res" goto L_MAIN
@echo\
@echo\ make MailRuCloudResource.rc
@echo\
@call make_prj.cmd %_compiler% ?
@%DELPHI_ROOTDIR%\bin\brcc32.exe MailRuCloudResource.rc
@ren MailRuCloudResource.RES MailRuCloudResource.res

:L_MAIN
@echo\
@echo\ make MailRuCloud.dpr
@echo\
@call make_prj.cmd %_compiler% MailRuCloud.dpr
@call clean.cmd
