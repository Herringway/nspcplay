import std;
import std.experimental.logger;

import nspc;

align(1) struct PackPointer {
	align(1):
	ubyte bank;
	ushort addr;
	uint full() {
		return addr + ((cast(uint)bank) << 16);
	}
}

int main(string[] args) {
	auto rom = cast(ubyte[])std.file.read(args[1]);
	if (rom[0x7FC0 .. 0x7FD5] == "KIRBY SUPER DELUXE   ") {
		extractKSS(rom, args[2]);
	} else if (rom[0xFFC0 .. 0xFFD5] == "EARTH BOUND          ") {
		extractEarthbound(rom, args[2]);
	} else {
		writeln("I don't know what that is.");
		return 1;
	}
	return 0;
}

const(ubyte[]) readPacks(const ubyte[] input) {
	const(ubyte)[] result;
	size_t offset = 0;
	while (true) {
		auto size = (cast(ushort[])(input[offset .. offset + 2]))[0];
		if (size == 0) {
			break;
		}
		auto spcOffset = (cast(ushort[])(input[offset + 2 .. offset + 4]))[0];
		infof("Song subpack: $%04X, %04X bytes", spcOffset, size);
		result ~= input[offset .. offset + size + 4];
		offset += size + 4;
	}
	return result;
}

void extractEarthbound(const scope ubyte[] data, string outDir) {
	align(1) static struct PackPointer {
		align(1):
		ubyte bank;
		ushort addr;
		uint full() {
			return addr + ((cast(uint)bank) << 16);
		}
	}
	enum NUM_SONGS = 0xBF;
	enum NUM_PACKS = 0xA9;
	enum BGM_PACK_TABLE = 0x4F70A;
	enum PACK_POINTER_TABLE = 0x4F947;
	auto packTable = cast(PackPointer[])(data[PACK_POINTER_TABLE .. PACK_POINTER_TABLE + NUM_PACKS * PackPointer.sizeof]);
	//const SONG_POINTER_TABLE = packTable[1].full - 0xC00000 + 4 + 0x2E4A;
	enum SONG_POINTER_TABLE = 0x26298C;
	auto bgmPacks = cast(ubyte[3][])data[BGM_PACK_TABLE .. BGM_PACK_TABLE + (ubyte[3]).sizeof * NUM_SONGS];
	auto songPointers = cast(ushort[])data[SONG_POINTER_TABLE .. SONG_POINTER_TABLE + ushort.sizeof * NUM_SONGS];
	mkdirRecurse("songs/eb");
	foreach (idx, songPacks; bgmPacks) {
		infof("Song ID: 0x%03X", idx);
		auto file = File(buildPath(outDir, format!"%03X.nspc"(idx)), "w");
		void writePack(ubyte pack) {
			size_t offset = packTable[pack].full - 0xC00000;
			file.rawWrite(readPacks(data[offset .. $]));
		}
		NSPCFileHeader header;
		header.songBase = songPointers[idx];
		header.instrumentBase = 0x6E00;
		header.sampleBase = 0x6C00;
		header.volumeTable = VolumeTable.hal1;
		header.releaseTable = ReleaseTable.hal1;
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

void extractKSS(const scope ubyte[] data, string outDir) {
	align(1) static struct PackPointer {
		align(1):
		ushort addr;
		ubyte bank;
		uint full() {
			return addr + ((cast(uint)bank) << 16);
		}
	}
	enum PACK_POINTER_TABLE = 0x5703;
	enum InstrumentPackTable = 0x57E7;
	enum numSongs = 65;
	auto packTable = cast(PackPointer[])(data[PACK_POINTER_TABLE .. PACK_POINTER_TABLE + (numSongs + 10) * PackPointer.sizeof]);
	auto sfxPacks = data[InstrumentPackTable .. InstrumentPackTable + numSongs];
	const progOffset = packTable[0].full - 0xC00000;
	const progPack = readPacks(data[progOffset .. $]);
	//const tPack = readPacks(data[packTable[1].full - 0xC00000 .. $]);
	foreach (song; 0 .. numSongs) {
		auto seqPackOffset = packTable[song + 10].full - 0xC00000;
		infof("Song data found at $%06X", seqPackOffset);
		const seqPackData = readPacks(data[seqPackOffset .. $]);
		auto instrPackOffset = packTable[sfxPacks[song] + 1].full - 0xC00000;
		infof("Instrument data found at $%06X", instrPackOffset);
		const instrPackData = readPacks(data[instrPackOffset .. $]);
		//const songPointers = cast(const(ushort)[])(tPack[112 * 2 .. 112 * 2 + numSongs * 2]);
		infof("Song ID: 0x%03X", song);
		auto file = File(buildPath(outDir, format!"%03X.nspc"(song)), "w");
		NSPCFileHeader header;
		header.songBase = (cast(const(ushort)[])(seqPackData[2 .. 4]))[0];
		header.instrumentBase = 0x500;
		header.sampleBase = 0x300;
		header.volumeTable = VolumeTable.hal2;
		header.releaseTable = ReleaseTable.hal2;
		infof("Song Base: $%04X", header.songBase);
		file.rawWrite([header]);
		file.rawWrite(progPack);
		//if (instrPackData != tPack) {
		file.rawWrite(instrPackData);
		//}
		file.rawWrite(seqPackData);
	}
}
