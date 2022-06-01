import std;
import std.experimental.logger;

enum NUM_SONGS = 0xBF;
enum NUM_PACKS = 0xA9;
enum BGM_PACK_TABLE = 0x4F70A;
enum PACK_POINTER_TABLE = 0x4F947;
enum SONG_POINTER_TABLE = 0x26298C;

align(1) struct PackPointer {
	align(1):
	ubyte bank;
	ushort addr;
	uint full() {
		return addr + ((cast(uint)bank) << 16);
	}
}

struct NSPCFileHeader {
	align(1):
	uint variant;
	ushort songBase;
	ushort instrumentBase;
	ushort sampleBase;
	ubyte[22] reserved;
}

void main(string[] args) {
	auto rom = cast(ubyte[])std.file.read(args[1]);
	auto packTable = cast(PackPointer[])(rom[PACK_POINTER_TABLE .. PACK_POINTER_TABLE + NUM_PACKS * PackPointer.sizeof]);
	auto bgmPacks = cast(ubyte[3][])rom[BGM_PACK_TABLE .. BGM_PACK_TABLE + (ubyte[3]).sizeof * NUM_SONGS];
	auto songPointers = cast(ushort[])rom[SONG_POINTER_TABLE .. SONG_POINTER_TABLE + ushort.sizeof * NUM_SONGS];
	foreach (idx, songPacks; bgmPacks) {
		infof("Song ID: 0x%03X", idx);
		auto file = File(format!"songs/%03X.nspc"(idx), "w");
		void writePack(ubyte pack) {
			size_t offset = packTable[pack].full - 0xC00000;
			while(true) {
				auto size = (cast(ushort[])(rom[offset .. offset + 2]))[0];
				if (size == 0) {
					break;
				}
				auto spcOffset = (cast(ushort[])(rom[offset + 2 .. offset + 4]))[0];
				infof("Song subpack: $%04X, %04X bytes", spcOffset, size);
				file.rawWrite(rom[offset .. offset + size + 4]);
				offset += size + 4;
			}
		}
		NSPCFileHeader header;
		header.songBase = songPointers[idx];
		header.instrumentBase = 0x6E00;
		header.sampleBase = 0x6C00;
		infof("Song Base: $%04X", header.songBase);
		file.rawWrite([header]);
		if (songPacks[2] == 0xFF) {
			writePack(1);
		}
		foreach (pack; songPacks) {
			if (pack == 0xFF) {
				continue;
			}
			infof("Song pack: $%02X ($%06X)", pack, packTable[pack].full);
			writePack(pack);
		}
	}
}