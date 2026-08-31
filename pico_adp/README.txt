pico_adp - uPD7759 / Sega Pico ADPCM encoder DLL
================================================

What this is
------------
nec_codec.c / nec_codec.h are "Encode and decode algorithms for NEC uPD775x
ADPCM, 2022 by superctr" (see the header comment in nec_codec.c). Everything
else here - dllmain.cpp, framework.h, pch.*, and the .sln/.vcxproj - is a thin
MSVC wrapper that exports the codec as a DLL so it can be called from managed
code. No upstream commit was recorded when this was first vendored.

Exports
-------
  nec_encode(buffer, outbuffer, len, div, threshold, master)
  nec_get_divider(buffer, len)
  nec_get_length(buffer, len, div)

nec_codec.h documents all three. div is the clock divider; the replay rate is
(chip clock / 4 / div), and it is carried in the stream's own block headers, so
a decoder does not need to be told the rate separately. master mode (RLE
compression, for a uPD7759 reading from its own ROM) is not implemented - the
encoder falls back to plain output and says so on stdout.

Building
--------
Open pico_adp.sln in Visual Studio and build Release | x64. Build the
architecture that matches the host process - a Win32 build will not load into a
64 bit host. There is no command line build script; this is a rebuild-by-hand
component, so it is worth recording in the commit message which revision of this
project a shipped DLL was built from.

The length trap
---------------
There are two different "lengths" here and they are easy to swap by mistake:

  nec_encode()      returns a POINTER TO THE END of the encoded stream, and
                    writes the 0x00 terminator byte itself. Subtract the start
                    pointer to get the encoded size in bytes. This is the number
                    you want if you are going to stream the data to the chip.

  nec_get_length()  returns the size of the buffer needed to DECODE the sample,
                    i.e. a PCM sample count, roughly twice the encoded byte
                    count. It is a decoder helper, not a stream length.

Using nec_get_length as a stream length makes a player read about twice the
sample and run off the end of it.

Also note nec_encode returns NULL if its internal malloc fails, and the codec
prints progress to stdout - harmless in most hosts, but it is not a silent
library.
