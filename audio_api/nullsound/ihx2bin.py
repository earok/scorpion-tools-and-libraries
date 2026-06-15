"""Convert Intel HEX to flat binary, filling gaps with 0xFF."""
import sys

with open(sys.argv[1]) as f:
    lines = f.readlines()

data = {}
for line in lines:
    line = line.strip()
    if not line.startswith(':'):
        continue
    count = int(line[1:3], 16)
    addr  = int(line[3:7], 16)
    rtype = int(line[7:9], 16)
    if rtype == 0x00:
        for i in range(count):
            data[addr + i] = int(line[9 + i*2 : 11 + i*2], 16)
    elif rtype == 0x01:
        break

if not data:
    sys.exit("No data records found in IHX file")

max_addr = max(data.keys()) + 1
out = bytearray(b'\xff' * max_addr)
for addr, byte in data.items():
    out[addr] = byte

with open(sys.argv[2], 'wb') as f:
    f.write(bytes(out))

print(f"Written {max_addr} bytes to {sys.argv[2]}")
