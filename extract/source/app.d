import std;
import std.experimental.logger;

import nspcplay;

align(1) struct PackPointer {
	align(1):
	ubyte bank;
	ushort addr;
	uint full() const {
		return addr + ((cast(uint)bank) << 16);
	}
}
align(1) static struct PackPointerLH {
	align(1):
	ushort addr;
	ubyte bank;
	uint full() const {
		return addr + ((cast(uint)bank) << 16);
	}
}

int main(string[] args) {
	const rom = readROM(args[1]);
	mkdirRecurse(args[2]);
	if (rom.title == "KIRBY SUPER DELUXE   ") {
		extractKSS(rom.data, args[2]);
	} else if (rom.title == "Kirby's Dream Course ") {
		extractKDC(rom.data, args[2]);
	} else if (rom.title == "EARTH BOUND          ") {
		extractEarthbound(rom.data, false, 0x4F947, 0x4F70A, args[2]);
	} else if (rom.title == "MOTHER-2             ") {
		extractEarthbound(rom.data, true, 0x4CCE2, 0x4CAA5, args[2]);
	} else if (rom.title == "01 95.03.27          ") {
		extractEarthbound(rom.data, false, 0x4FBD4, 0x4F997, args[2]);
	} else if (rom.title == "SUPER MARIOWORLD     ") {
		extractSMW(rom.data, args[2]);
	} else if (rom.title == "THE LEGEND OF ZELDA  ") {
		extractZ3(rom.data, args[2]);
	} else if (rom.title == "Super Metroid        ") {
		extractSMET(rom.data, args[2]);
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

void extractEarthbound(const scope ubyte[] data, bool m2packPointers, size_t packPointerTable, size_t packTableOffset, string outDir) {
	static immutable string[] titles = import("eb.txt").split("\n");
	enum NUM_SONGS = 0xBF;
	enum NUM_PACKS = 0xA9;
	auto packs = (cast(PackPointer[])(data[packPointerTable .. packPointerTable + NUM_PACKS * PackPointer.sizeof]))
		.map!(x => parsePacks(data[x.full + (m2packPointers ? 0x220000 : -0xC00000) .. $]));
	enum SONG_POINTER_TABLE = 0x294E;
	auto bgmPacks = cast(ubyte[3][])data[packTableOffset .. packTableOffset + (ubyte[3]).sizeof * NUM_SONGS];
	auto songPointers = cast(ushort[])packs[1][2].data[SONG_POINTER_TABLE .. SONG_POINTER_TABLE + ushort.sizeof * NUM_SONGS];
	foreach (idx, songPacks; bgmPacks) {
		auto file = File(buildPath(outDir, format!"%03X.nspc"(idx)), "w").lockingBinaryWriter;
		NSPCWriter writer;
		writer.header.songBase = songPointers[idx];
		writer.header.instrumentBase = 0x6E00;
		writer.header.sampleBase = 0x6C00;
		writer.header.volumeTable = VolumeTable.hal1;
		writer.header.releaseTable = ReleaseTable.hal1;
		if (songPacks[2] == 0xFF) {
			writer.packs ~= packs[1];
		}
		foreach (pack; songPacks) {
			if (pack == 0xFF) {
				continue;
			}
			writer.packs ~= packs[pack];
		}
		writer.tags = [
			TagPair("album", "Earthbound"),
			TagPair("title", titles[idx]),
		];
		writer.toBytes(file);
	}
}

void extractKDC(const scope ubyte[] data, string outDir) {
	const sequencePackPointerTable = cast(uint[])data[0x3745 .. 0x3745 + 33 * uint.sizeof];
	const samplePackPointerTable = cast(uint[])data[0x372D .. 0x372D + 3 * uint.sizeof];
	const instrumentPackPointerTable = cast(uint[])data[0x373B .. 0x373B + 2 * uint.sizeof];
	const(Pack)[] globalPacks;
	foreach (pack; samplePackPointerTable) {
		globalPacks ~= parsePacks(data[loromToPC(pack) .. $]);
	}
	foreach (pack; instrumentPackPointerTable) {
		globalPacks ~= parsePacks(data[loromToPC(pack) .. $]);
	}
	foreach (id, sequencePackPointer; sequencePackPointerTable) {
		auto file = File(buildPath(outDir, format!"%03X.nspc"(id)), "w").lockingBinaryWriter;
		const seqPack = parsePacks(data[loromToPC(sequencePackPointer) .. $]);
		NSPCWriter writer;
		writer.header.songBase = 0x6502;
		writer.header.sampleBase = 0x400;
		writer.header.instrumentBase = 0x600;
		writer.header.volumeTable = VolumeTable.hal3;
		writer.header.releaseTable = ReleaseTable.hal3;
		foreach (instrumentPack; globalPacks) {
			writer.packs ~= instrumentPack;
		}
		writer.packs ~= seqPack;
		writer.tags = [
			TagPair("album", "Kirby's Dream Course"),
		];
		writer.toBytes(file);
	}
}

uint loromToPC(uint addr) @safe pure {
	return (((addr & 0x7FFFFF) >> 1) & 0xFF8000) + (addr & 0x7FFF);
}

@safe unittest {
	assert(loromToPC(0x97EDA8) == 0xBEDA8);
}

void extractKSS(const scope ubyte[] data, string outDir) {
	enum PACK_POINTER_TABLE = 0x5703;
	enum InstrumentPackTable = 0x57E7;
	enum numSongs = 65;
	auto packTable = cast(PackPointerLH[])(data[PACK_POINTER_TABLE .. PACK_POINTER_TABLE + (numSongs + 10) * PackPointerLH.sizeof]);
	auto sfxPacks = data[InstrumentPackTable .. InstrumentPackTable + numSongs];
	const progOffset = packTable[0].full - 0xC00000;
	const progPack = parsePacks(data[progOffset .. $]);
	foreach (song; 0 .. numSongs) {
		auto seqPackOffset = packTable[song + 10].full - 0xC00000;
		infof("Song data found at $%06X", seqPackOffset);
		const seqPackData = parsePacks(data[seqPackOffset .. $]);
		auto instrPackOffset = packTable[sfxPacks[song] + 1].full - 0xC00000;
		infof("Instrument data found at $%06X", instrPackOffset);
		const instrPackData = parsePacks(data[instrPackOffset .. $]);
		infof("Song ID: 0x%03X", song);
		auto file = File(buildPath(outDir, format!"%03X.nspc"(song)), "w").lockingBinaryWriter;
		NSPCWriter writer;
		writer.header.songBase = seqPackData[0].address;
		writer.header.instrumentBase = 0x500;
		writer.header.sampleBase = 0x300;
		writer.header.volumeTable = VolumeTable.hal2;
		writer.header.releaseTable = ReleaseTable.hal2;
		writer.packs ~= progPack;
		writer.packs ~= instrPackData;
		writer.packs ~= seqPackData;
		writer.tags = [
			TagPair("album", "Kirby Super Star"),
		];
		writer.toBytes(file);
	}
}

void extractSMW(const scope ubyte[] data, string outDir) {
	enum firCoefficientsTable = 0x70DB1;
	const progOffset = 0x70000;
	const sfxOffset = 0x78000;
	const seq1Offset = 0x718B1;
	const seq2Offset = 0x72ED6;
	const seq3Offset = 0x1E400;
	const parsedProg = parsePacks(data[progOffset .. $]);
	const parsedSeq1 = parsePacks(data[seq1Offset .. $]);
	const parsedSeq2 = parsePacks(data[seq2Offset .. $]);
	const parsedSeq3 = parsePacks(data[seq3Offset .. $]);
	const parsedSfx = parsePacks(data[sfxOffset .. $]);
	enum tableBase = 0x135E;
	const ubyte[8][] firCoefficients = cast(const(ubyte[8])[])(data[firCoefficientsTable .. firCoefficientsTable + 2 * 8]);
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
				auto file = File(buildPath(outDir, filename), "w").lockingBinaryWriter;
				NSPCWriter writer;
				infof("Writing %s", filename);
				writer.header.songBase = songAddr;
				writer.header.sampleBase = 0x8000;
				writer.header.instrumentBase = 0x5F46;
				writer.header.variant = nspcplay.Variant.prototype;
				writer.header.extra.percussionBase = 0x5FA5;
				writer.header.volumeTable = VolumeTable.nintendo;
				writer.header.releaseTable = ReleaseTable.nintendo;
				writer.header.firCoefficientTableCount = 2;
				writer.packs ~= parsedProg[1];
				writer.packs ~= parsedSfx;
				writer.packs ~= pack;
				writer.firCoefficients = firCoefficients;
				writer.tags = [
					TagPair("album", "Super Mario World"),
				];
				writer.toBytes(file);
				lowest = min(songAddr, lowest);
			}
		}
	}
}

void extractZ3(const scope ubyte[] data, string outDir) {
	enum progOffset = 0xC8000;
	enum songOffset2 = 0xD8000;
	enum songOffset3 = 0xD5380;
	const parsed = [parsePacks(data[progOffset .. $]), parsePacks(data[songOffset2 .. $]), parsePacks(data[songOffset3 .. $])];
	const songTable = cast(const(ushort)[])parsed[0][7].data[0 .. 27 * ushort.sizeof]; //bank 0's song table is a little shorter than the others...?
	const songTable2 = cast(const(ushort)[])parsed[1][0].data[0 .. 35 * ushort.sizeof];
	const songTable3 = cast(const(ushort)[])parsed[2][0].data[0 .. 35 * ushort.sizeof];
	uint songID;
	foreach (p1, p2, p3; zip(songTable.chain(0.repeat(8)), songTable2, songTable3)) {
		foreach (idx, address; only(p1, p2, p3).enumerate) {
			if (address == 0) {
				continue;
			}
			const filename = format!"%s - %02X.nspc"(idx, songID);
			auto file = File(buildPath(outDir, filename), "w").lockingBinaryWriter;
			NSPCWriter writer;
			writer.header.songBase = cast(ushort)address;
			writer.header.sampleBase = 0x3C00;
			writer.header.instrumentBase = 0x3D00;
			writer.header.variant = nspcplay.Variant.standard;
			writer.header.volumeTable = VolumeTable.nintendo;
			writer.header.releaseTable = ReleaseTable.nintendo;
			writer.packs = parsed[0];
			if (idx > 0) {
				writer.packs ~= parsed[idx];
			}
			writer.tags = [
				TagPair("album", "The Legend of Zelda: A Link to the Past"),
			];
			writer.toBytes(file);
		}
		songID++;
	}
}

void extractSMET(const scope ubyte[] data, string outDir) {
	static immutable string[] titles = import("smet.txt").split("\n");
	enum songs = 25;
	enum packTableOffset = 0x7E7E1;
	const table = cast(const(PackPointerLH)[])(data[packTableOffset .. packTableOffset + songs * PackPointerLH.sizeof]);
	const first = parsePacks(data[loromToPC(table[0].full) .. $]);
	const firCoefficients = cast(const(ubyte[8])[])(first[2].data[0x1E32 - 0x1500 .. 0x1E32 - 0x1500 + 8 * 11]); //there only seem to be 4, but songs are relying on an invalid 11th preset?
	uint songID = 0;
	foreach (idx, pack; table) {
		const packs = parsePacks(data[loromToPC(pack.full) .. $]);
		const(ushort)[] subSongs;
		uint subSongIndex;
		foreach (sp; packs) {
			if (sp.address == 0x5820) { // initial pack contains 4 'always loaded' songs and one dynamic
				subSongs = cast(const(ushort)[])(sp.data[0 .. 14]);
				subSongIndex = 0;
				break;
			}
			if (sp.address == 0x5828) { // all other packs have a single dynamically loaded song
				const firstSong = (cast(const(ushort)[])(sp.data[0 .. 2]))[0];
				subSongs = cast(const(ushort)[])(sp.data[0 .. firstSong - 0x5828]);
				subSongIndex = 4;
				break;
			}
		}
		foreach (subSong; subSongs) {
			const filename = format!"%02X - %s.nspc"(idx, subSongIndex++);
			NSPCWriter writer;
			writer.header.songBase = subSong;
			writer.header.sampleBase = 0x6D00;
			writer.header.instrumentBase = 0x6C00;
			writer.header.variant = nspcplay.Variant.standard;
			writer.header.volumeTable = VolumeTable.nintendo;
			writer.header.releaseTable = ReleaseTable.nintendo;
			if (idx > 0) {
				writer.packs = first;
			}
			writer.packs ~= packs;
			writer.tags = [
				TagPair("album", "Super Metroid"),
				TagPair("title", titles[songID++]),
			];
			writer.header.firCoefficientTableCount = cast(ubyte)firCoefficients.length;
			writer.firCoefficients = firCoefficients;
			auto file = File(buildPath(outDir, filename), "w").lockingBinaryWriter;
			writer.toBytes(file);
		}
	}
}
