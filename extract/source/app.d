import std;
import std.experimental.logger;

import nspc;

immutable ushort[] packTerminator = [0];

align(1) struct PackPointer {
	align(1):
	ubyte bank;
	ushort addr;
	uint full() {
		return addr + ((cast(uint)bank) << 16);
	}
}

int main(string[] args) {
	const rom = readROM(args[1]);
	mkdirRecurse(args[2]);
	if (rom.title == "KIRBY SUPER DELUXE   ") {
		extractKSS(rom.data, args[2]);
	} else if (rom.title == "EARTH BOUND          ") {
		extractEarthbound(rom.data, args[2]);
	} else if (rom.title == "SUPER MARIOWORLD     ") {
		extractSMW(rom.data, args[2]);
	} else {
		writefln!"I don't know what '%s' is."(rom.title);
		return 1;
	}
	return 0;
}

struct ROMFile {
	string title;
	const ubyte[] data;
}

ROMFile readROM(string path) {
	const rom = cast(ubyte[])std.file.read(path);
	immutable headerOffsets = [
		0x7FB0: false, //lorom
		0xFFB0: false, //hirom
		0x81B0: true, //lorom + copier header
		0x101B0: true, //hirom + copier header
	];
	foreach (offset, stripHeader; headerOffsets) {
		const ushort checksum = (cast(const(ushort)[])rom[offset + 44 .. offset + 46])[0];
		const ushort checksumComplement = (cast(const(ushort)[])rom[offset + 46 .. offset + 48])[0];
		if ((checksum ^ checksumComplement) == 0xFFFF) {
			return ROMFile((cast(char[])rom[offset + 16 .. offset + 37]).idup, rom[stripHeader ? 0x200 : 0 .. $]);
		}
	}
	return ROMFile.init;
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
		file.rawWrite(packTerminator);
	}
}

void extractSMW(const scope ubyte[] data, string outDir) {
	const progOffset = 0x70000;
	const sfxOffset = 0x78000;
	const seq1Offset = 0x718B1;
	const seq2Offset = 0x72ED6;
	const seq3Offset = 0x1E400;
	const progPack = readPacks(data[progOffset .. $]);
	const sfxPack = readPacks(data[sfxOffset .. $]);
	const seq1Pack = readPacks(data[seq1Offset .. $]);
	const seq2Pack = readPacks(data[seq2Offset .. $]);
	const seq3Pack = readPacks(data[seq3Offset .. $]);
	const parsedProg = parsePacks(progPack);
	const parsedSeq1 = parsePacks(seq1Pack);
	const parsedSeq2 = parsePacks(seq2Pack);
	const parsedSeq3 = parsePacks(seq3Pack);
	enum tableBase = 0x135E;
	const(ubyte)[] writablePack(const Pack pack) {
		const(ubyte)[] result;
		const ushort[] packHeader = [pack.size, pack.address];
		result ~= cast(const(ubyte)[])packHeader ~ pack.data;
		return result;
	}
	immutable ubyte[8][] firCoefficients = [[0xFF, 0x08, 0x17, 0x24, 0x24, 0x17, 0x08, 0xFF], [0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]];
	foreach (bank, pack; chain(parsedProg, parsedSeq1, parsedSeq2, parsedSeq3).enumerate) {
		if (pack.address == 0x1360) {
			ushort currentOffset = tableBase;
			ushort lowest = ushort.max;
			foreach (idx, songAddr; cast(const(ushort)[])pack.data[0 .. 255 * 2]) {
				currentOffset += 2;
				if (lowest <= currentOffset) {
					break;
				}
				const filename = format!"%s - %02X.nspc"(bank, idx);
				auto file = File(buildPath(outDir, filename), "w");
				infof("Writing %s", filename);
				NSPCFileHeader header;
				header.songBase = songAddr;
				header.sampleBase = 0x8000;
				header.instrumentBase = 0x5F46;
				header.variant = nspc.Variant.prototype;
				header.extra.percussionBase = 0x5FA5;
				header.volumeTable = VolumeTable.nintendo;
				header.releaseTable = ReleaseTable.nintendo;
				header.firCoefficientTableCount = 2;
				file.rawWrite([header]);
				file.rawWrite(writablePack(parsedProg[1]));
				file.rawWrite(sfxPack);
				file.rawWrite(writablePack(pack));
				file.rawWrite(packTerminator);
				// FIR coefficients
				file.rawWrite(firCoefficients);
				lowest = min(songAddr, lowest);
			}
		}
	}
}

struct Pack {
	ushort size;
	ushort address;
	const(ubyte)[] data;
}

const(Pack)[] parsePacks(const(ubyte)[] input) {
	const(Pack)[] result;
	size_t offset = 0;
	while (offset < input.length) {
		Pack pack;
		auto size = (cast(ushort[])(input[offset .. offset + 2]))[0];
		if (size == 0) {
			break;
		}
		auto spcOffset = (cast(ushort[])(input[offset + 2 .. offset + 4]))[0];
		pack.size = size;
		pack.address = spcOffset;
		pack.data = input[offset + 4 .. offset + size + 4];
		result ~= pack;
		offset += size + 4;
	}
	return result;
}
