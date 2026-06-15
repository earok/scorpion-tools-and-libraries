@echo off
setlocal

set AS=D:\Toolbox\DEV\SDCC\bin\sdasz80.exe
set LD=D:\Toolbox\DEV\SDCC\bin\sdldz80.exe
set SRC=%~dp0
set OUT=%SRC%..\audio_plugins

cd /d "%SRC%"

echo Assembling nullsound Z80 driver...

%AS% -a -g -l -p -s -u -I. -o memory.rel          memory.s        || goto fail
%AS% -a -g -l -p -s -u -I. -o bios-commands.rel   bios-commands.s || goto fail
%AS% -a -g -l -p -s -u -I. -o adpcm.rel           adpcm.s        || goto fail
%AS% -a -g -l -p -s -u -I. -o ym2610.rel          ym2610.s        || goto fail
%AS% -a -g -l -p -s -u -I. -o stream.rel          stream.s        || goto fail
%AS% -a -g -l -p -s -u -I. -o timer.rel           timer.s         || goto fail
%AS% -a -g -l -p -s -u -I. -o utils.rel           utils.s         || goto fail
%AS% -a -g -l -p -s -u -I. -o nss-fm.rel          nss-fm.s        || goto fail
%AS% -a -g -l -p -s -u -I. -o nss-ssg.rel         nss-ssg.s       || goto fail
%AS% -a -g -l -p -s -u -I. -o nss-adpcm-a.rel     nss-adpcm-a.s   || goto fail
%AS% -a -g -l -p -s -u -I. -o nss-adpcm-b.rel     nss-adpcm-b.s   || goto fail
%AS% -a -g -l -p -s -u -I. -o fx-vibrato.rel      fx-vibrato.s    || goto fail
%AS% -a -g -l -p -s -u -I. -o fx-slide.rel        fx-slide.s      || goto fail
%AS% -a -g -l -p -s -u -I. -o fx-trigger.rel      fx-trigger.s    || goto fail
%AS% -a -g -l -p -s -u -I. -o fx-arpeggio.rel     fx-arpeggio.s   || goto fail
%AS% -a -g -l -p -s -u -I. -o fx-legato.rel       fx-legato.s     || goto fail
%AS% -a -g -l -p -s -u -I. -o volume.rel          volume.s        || goto fail

echo Assembling entrypoint (MVS variant)...
:: entrypoint needs -Imvs for the hardware-specific FM/SSG frequency tables
:: included transitively through buffers.s
%AS% -a -g -l -p -s -u -I. -Imvs -o entrypoint-mvs.rel entrypoint.s || goto fail

echo Concatenating into nullsound.lib...
copy /b entrypoint-mvs.rel+memory.rel+bios-commands.rel+adpcm.rel+ym2610.rel+stream.rel+timer.rel+utils.rel+nss-fm.rel+nss-ssg.rel+nss-adpcm-a.rel+nss-adpcm-b.rel+fx-vibrato.rel+fx-slide.rel+fx-trigger.rel+fx-arpeggio.rel+fx-legato.rel+volume.rel nullsound.lib || goto fail

echo Assembling user_commands...
%AS% -a -g -l -p -s -u -I. -o user_commands.rel user_commands.s || goto fail

echo Linking...
%LD% -n -b DATA=0xf800 nullsound_driver nullsound.lib user_commands.rel -i -w -m || goto fail

echo Converting IHX to binary...
python ihx2bin.py nullsound_driver.ihx nullsound_driver.bin || goto fail

echo Copying to audio_plugins...
copy /y nullsound_driver.bin "%OUT%\nullsound_driver.bin" || goto fail
copy /y nullsound_driver.map "%OUT%\nullsound_driver.map" || goto fail

echo.
echo Build complete. Extracting symbol offsets...
python extract_offsets.py nullsound_driver.map nullsound_offsets.txt || goto fail

copy /y nullsound_offsets.txt "%OUT%\nullsound_offsets.txt"
echo.
echo Done. See audio_plugins\nullsound_offsets.txt for C# compiler reference.
goto end

:fail
echo.
echo BUILD FAILED
exit /b 1

:end
