@echo off

dotnet fsi --optimize+ --tailcalls+ Day%1.fsx %2
