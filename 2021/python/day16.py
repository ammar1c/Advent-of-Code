from attr import dataclass
from enum import Enum
from functools import reduce
from operator import mul, add, gt, lt, eq

@dataclass
class Packet:
    version: int
    pid: int

@dataclass
class LiteralPacket(Packet):
    literal: int

@dataclass
class OpPacket(Packet):
    packets: list[Packet]

def parse_packet(bin_str):
    version = int(bin_str[:3], 2)
    pid = int(bin_str[3:6], 2)
    packet_len_id = bin_str[6]
    subpackets = []
    if pid == 4:
        i, last = 6, False
        literal = 0
        while not last:
            if bin_str[i] == '0':
                last = True
            i += 1
            literal = literal << 4 | int(bin_str[i:i + 4], 2)
            i += 4
        return (i, LiteralPacket(version, pid, literal))
    if packet_len_id == '0':
        total_len = int(bin_str[7:22], 2)
        i = 22
        limit = i + total_len
        while i < limit:
            sub_len, sub = parse_packet(bin_str[i:])
            subpackets.append(sub)
            i += sub_len
        return (i, OpPacket(version, pid, subpackets))
    else:
        subpackets_len = int(bin_str[7:18], 2)
        i = 18
        for _ in range(subpackets_len):
            sub_len, sub = parse_packet(bin_str[i:])
            subpackets.append(sub)
            i += sub_len
        return (i, OpPacket(version, pid, subpackets))



def calc_value(packet):

    if packet.pid == 0  : return reduce(add, map(calc_value, packet.packets))
    elif packet.pid == 1: return reduce(mul, map(calc_value, packet.packets))
    elif packet.pid == 2: return reduce(min, map(calc_value, packet.packets))
    elif packet.pid == 3: return reduce(max, map(calc_value, packet.packets))
    elif packet.pid == 4: return packet.literal
    elif packet.pid == 5: return int(reduce(gt, map(calc_value, packet.packets)))
    elif packet.pid == 6: return int(reduce(lt, map(calc_value, packet.packets)))
    else                : return int(reduce(eq, map(calc_value, packet.packets)))


def sum_version_numbers(packet):
    if issubclass(LiteralPacket, type(packet)): return packet.version
    else: return reduce(lambda x,y: x+y, map(sum_version_numbers, packet.packets), packet.version)

def to_bin(hex_string):
    return "".join(f"{int(h, 16):04b}" for h in  hex_string)

def main(hex_str):
    part1 = to_bin(hex_str)
    packet = parse_packet(part1)
    print(sum_version_numbers(packet[1]))
    print(calc_value(packet[1]))

if __name__ == '__main__':
    with open('./input.txt') as f:
        all = f.read()
        main(all)