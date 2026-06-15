"""Extract key NullSound symbol offsets from the linker map file."""
import re, sys

map_file = sys.argv[1]
out_file = sys.argv[2]

syms = [
    'snd_adpcm_a_play',
    'snd_adpcm_a_play_exclusive',
    'snd_adpcm_b_play',
    'snd_adpcm_b_play_loop',
    'snd_adpcm_b_stop',
    'snd_stream_play',
    'snd_stream_stop',
]

found = {}
with open(map_file) as f:
    for line in f:
        for s in syms:
            m = re.search(r'([0-9A-Fa-f]{4})\s+' + re.escape(s) + r'\b', line)
            if m:
                found[s] = int(m.group(1), 16)

with open(out_file, 'w') as out:
    out.write('# NullSound Z80 driver symbol offsets\n')
    out.write('# These are byte offsets into nullsound_driver.bin\n')
    out.write('# (Z80 address == binary offset because code loads at 0x0000)\n')
    out.write('# cmd_jmptable is always at (fileLength - 384) and needs no entry here\n\n')
    for s in syms:
        if s in found:
            line = f'{s:<30} 0x{found[s]:04X}'
        else:
            line = f'{s:<30} NOT FOUND - check nullsound_driver.map for actual name'
        out.write(line + '\n')
        print(line)
