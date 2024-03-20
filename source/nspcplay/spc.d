module nspcplay.spc;

import nspcplay.common : ReleaseTable, Variant, VolumeTable;
import nspcplay.song : NSPCFileHeader, releaseTables, Song, volumeTables;
import nspcplay.tags : TagPair;

import std.algorithm;
import std.exception;
import std.logger;
import std.range;
import std.string;

union ID666Tags {
	TextID666Tags text;
	BinaryID666Tags binary;
}

struct TextID666Tags {
	align(1):
	char[32] songTitle;
	char[32] gameTitle;
	char[16] dumperName;
	char[32] comments;
	char[11] dumpDate;
	char[3] fadeStart;
	char[5] fadeLength;
	char[32] songArtist;
	ubyte defaultChannelEnables;
	ubyte emulator;
	ubyte[45] reserved;
}
struct BinaryID666Tags {
	align(1):
	char[32] songTitle;
	char[32] gameTitle;
	char[16] dumperName;
	char[32] comments;
	ubyte[4] dumpDate;
	ubyte[7] reserved;
	char[3] fadeStart;
	char[4] fadeLength;
	char[32] songArtist;
	ubyte defaultChannelEnables;
	ubyte emulator;
	ubyte[46] reserved2;
}

Song loadSPCFile(scope const ubyte[] file) @safe {
	Song song;
	const spc = file[0x100 .. 0x10100];
	const dsp = file[0x10100 .. 0x10180];

	song.tags = processID666(file[0x2E .. 0x100]);
	foreach (extendedTag; processExtendedID666(file[0x10200 .. $])) {
		auto existingTag = song.tags.countUntil!(x => x.key == extendedTag.key);
		if (existingTag >= 0) {
			song.tags[existingTag] = extendedTag;
		} else {
			song.tags ~= extendedTag;
		}
	}
	auto header = detectParameters(spc[], dsp[]).header;
	infof("Detected variant: %s", header.variant);
	infof("Detected instruments: %04X", header.instrumentBase);
	infof("Detected samples: %04X", header.sampleBase);
	infof("Detected song: %04X", header.songBase);
	infof("Detected release table: %s, volume table: %s", header.releaseTable, header.volumeTable);
	song.loadNSPC(header, spc[]);
	return song;
}
private int integer(scope const ubyte[] data) @safe pure {
	enforce(data.length >= 4);
	return (cast(const(int)[])data[0 .. 4])[0];
}
TagPair[] processExtendedID666(scope const ubyte[] data) @safe {
	import std.conv : text;
	import std.datetime.date : Date;
	TagPair[] result;
	if (!data.startsWith("xid6")) {
		return [];
	}
	const dataSize = (cast(const(uint)[])(data[4 .. 8]))[0];
	auto dataLeft = data[8 .. 8 + dataSize];
	while (dataLeft.length > 0) {
		const id = dataLeft[0];
		if (id == 0) {
			break;
		}
		const type = dataLeft[1];
		const length = (cast(const(ushort)[])(dataLeft[2 .. 4]))[0];
		const tagData = type == 0 ? [] : dataLeft[4 .. 4 + length];
		dataLeft = dataLeft[4 + tagData.length .. $];
		switch (cast(ExID666IDs)id) {
			case ExID666IDs.songName:
				result ~= TagPair("title", (cast(const(char)[])tagData).idup);
				break;
			case ExID666IDs.gameName:
				result ~= TagPair("album", (cast(const(char)[])tagData).idup);
				break;
			case ExID666IDs.artistName:
				result ~= TagPair("artist", (cast(const(char)[])tagData).idup);
				break;
			case ExID666IDs.dumperName:
				result ~= TagPair("dumper", (cast(const(char)[])tagData).idup);
				break;
			case ExID666IDs.dateDumped:
				const intTimestamp = tagData.integer;
				const dumpDate = new Date(intTimestamp / 10000, (intTimestamp / 100) % 12, intTimestamp % 100);
				result ~= TagPair("date dumped", dumpDate.toISOExtString);
				break;
			case ExID666IDs.emulator:
				enum emulators = [1: "ZSNES", 2: "SNES9x"];
				result ~= TagPair("emulator", emulators.get(tagData.integer, "Unknown"));
				break;
			case ExID666IDs.comments:
				result ~= TagPair("comments", (cast(const(char)[])tagData).idup);
				break;
			case ExID666IDs.ostTitle:
				result ~= TagPair("official soundtrack title", (cast(const(char)[])tagData).idup);
				break;
			case ExID666IDs.ostDisc:
				result ~= TagPair("disc", (length & 0xFF).text);
				break;
			case ExID666IDs.ostTrack:
				result ~= TagPair("track", ((length >> 8) & 0xFF).text);
				break;
			case ExID666IDs.publisher:
				result ~= TagPair("publisher", (cast(const(char)[])tagData).idup);
				break;
			case ExID666IDs.copyrightYear:
				result ~= TagPair("copyright year", (length & 0xFFFF).text);
				break;
			case ExID666IDs.introLength:
				result ~= TagPair("_intro length", tagData.integer.text);
				break;
			case ExID666IDs.loopLength:
				result ~= TagPair("_loop length", tagData.integer.text);
				break;
			case ExID666IDs.endLength:
				result ~= TagPair("_end length", tagData.integer.text);
				break;
			case ExID666IDs.fadeLength:
				result ~= TagPair("_fade length", tagData.integer.text);
				break;
			case ExID666IDs.mutedChannels:
				result ~= TagPair("_muted channels", (length & 0xFF).text);
				break;
			case ExID666IDs.loopCount:
				result ~= TagPair("_loop count", (length & 0xFF).text);
				break;
			case ExID666IDs.amplification:
				result ~= TagPair("_amplification", tagData.integer.text);
				break;
			default:
				tracef("Skipping unknown tag id %s", id);
		}
	}
	return result;
}
TagPair[] processID666(scope const ubyte[] data) @safe {
	TagPair[] result;
	const id666 = (cast(const(ID666Tags)[])data)[0];
	void addTag(string tag, const char[] value) {
		if (value != "") {
			result ~= TagPair(tag, value.idup);
		}
	}
	addTag("title", id666.text.songTitle.fromStringz);
	addTag("album", id666.text.gameTitle.fromStringz);
	addTag("artist", id666.text.songArtist.fromStringz);
	addTag("comment", id666.text.comments.fromStringz);
	enum emulators = [1: "ZSNES", 2: "SNES9x"];
	addTag("emulator", emulators.get(id666.text.emulator, ""));
	return result;
}
struct DetectionResult {
	NSPCFileHeader header;
	ushort songTableAddress;
}

DetectionResult detectParameters(scope const(ubyte)[] data, scope const(ubyte)[] dsp) @safe pure {
	import std.algorithm.searching : find;
	DetectionResult result;
	result.header.sampleBase = dsp[0x5D] << 8;
	if (data[0x400 .. 0x410].among(earlyAMK, laterAMK)) {
		result.header.variant = nspcplay.Variant.addmusick;
		auto foundPreTable = data;
		if (!findSkip(foundPreTable, amkPreTable)) {
			enforce(findSkip(foundPreTable, amkPreTable109), "AMK detected, but song table location failed!");
		}
		const tableAddr = (cast(const(ushort)[])(foundPreTable[0 .. 2]))[0] + 2 + 18;
		result.header.songBase = (cast(const(ushort)[])data[tableAddr .. tableAddr + 2])[0];
		result.header.instrumentBase = 0x1844;
		const songStartData = data[result.header.songBase .. $];
		ushort customInstrumentBase = 0;
		while(songStartData[customInstrumentBase .. customInstrumentBase + 2] != [0xFF, 0x00]) {
			customInstrumentBase += 2;
		}
		result.header.extra.customInstruments = cast(ushort)((cast(const(ushort)[])songStartData[customInstrumentBase + 2 .. customInstrumentBase  + 4])[0] + 6);
	} else {
		const(ushort)[] songTable;
		ubyte trackPointerAddress;
		for (ushort i = 0; i < 0xFF00; i++) {
			// prototype percussion
			if ((data[i .. i + 6] == [0x80, 0xA8, 0xD0, 0x8D, 0x06, 0x8F]) && (data[i + 7 .. i + 9] == [0x14, 0x8F]) && (data[i + 10 .. i + 12] == [0x15, 0x3F])) {
				result.header.extra.percussionBase = data[i + 6] + (data[i + 9] << 8);
			}
			// prototype instruments
			if ((data[i .. i + 4] == [0x9C, 0x8D, 0x05, 0x8F]) && (data[i + 5 .. i + 7] == [0x14, 0x8F]) && (data[i + 8 .. i + 12] == [0x15, 0xCF, 0x7A, 0x14])) {
				result.header.variant = Variant.prototype;
				result.header.instrumentBase = data[i + 4] + (data[i + 7] << 8);
			}
			// instruments
			if ((data[i .. i + 2] == [0xCF, 0xDA]) && (data[i + 3 .. i + 5] == [0x60, 0x98]) && (data[i + 7] == 0x98) && (data[i+2] == data[i + 6])) {
				result.header.instrumentBase = data[i + 5] + (data[i + 8] << 8);
			}
			// common song table
			if ((data[i .. i + 3] == [0x1C, 0x5D, 0xF5]) && (data[i + 5 .. i + 7] == [0xFD, 0xF5]) && (data[i + 9] == 0xDA)) {
				trackPointerAddress = data[i + 10];
				result.songTableAddress = data[i + 7] + (data[i + 8] << 8);
			}
			// star fox song table
			if ((data[i .. i + 3] == [0x1C, 0x5D, 0xF5]) && (data[i + 5] == 0xFD) && (data[i + 11] == 0xF5) && (data[i + 14] == 0xDA)) {
				trackPointerAddress = data[i + 15];
				result.songTableAddress = data[i + 12] + (data[i + 13] << 8);
			}
			// prototype song table
			if ((data[i .. i + 3] == [0x1C, 0xFD, 0xF6]) && (data[i + 5] == 0xC4) && (data[i + 7] == 0xF6) && (data[i + 10] == 0xC4) && (data[i + 12] == 0xCD)) {
				trackPointerAddress = data[i + 6];
				result.songTableAddress = data[i + 3] + (data[i + 4] << 8);
			}
			// fir coefficients
			if ((data[i .. i + 7] == [0x8D, 0x08, 0xCF, 0x5D, 0x8D, 0x0F, 0xF5])) {
				debug infof("Found FIR coefficients at %04X", (cast(const(ushort)[])(data[i + 7 .. i + 9]))[0]);
			}
			// release table
			if ((data[i .. i + 6] == [0x2D, 0x9F, 0x28, 0x07, 0xFD, 0xF6])) {
				const offset = (cast(const(ushort)[])(data[i + 6 .. i + 8]))[0];
				debug infof("Found release table at %04X", offset);
				bool found;
				foreach (idx, table; releaseTables) {
					if (data[offset .. offset + table.length] == table) {
						result.header.releaseTable = cast(ReleaseTable)idx;
						found = true;
						break;
					}
				}
				if (!found) {
					debug infof("No release table match: [%(0x%02X, %)]", data[offset .. offset + releaseTables[0].length]);
				}
			}
			// volume table
			if ((data[i .. i + 5] == [0xAE, 0x28, 0x0F, 0xFD, 0xF6])) {
				const offset = (cast(const(ushort)[])(data[i + 5 .. i + 7]))[0];
				debug infof("Found volume table at %04X", offset);
				bool found;
				foreach (idx, table; volumeTables) {
					if (data[offset .. offset + table.length] == table) {
						result.header.volumeTable = cast(VolumeTable)idx;
						found = true;
						break;
					}
				}
				if (!found) {
					debug infof("No volume table match: [%(0x%02X, %)]", data[offset .. offset + volumeTables[0].length]);
				}
			}
		}
		if (data[0x800 .. 0x810] == fzeroStart) {
			result.header.variant = Variant.fzero;
		}
		if (result.songTableAddress != 0) {
			songTable = (cast(const(ushort)[])data[result.songTableAddress .. min($, result.songTableAddress + 512)]);
			debug infof("Found song table at %04X", result.songTableAddress);
			ubyte songID = 0;
			foreach (songIDCandidate; only(data[0x04], data[0x00], data[0x06], data[0x08], data[0x0D], data[0xF3], data[0xF4])) {
				if (songIDCandidate == 0) {
					continue;
				}
				songID = songIDCandidate;
				break;
			}
			if(songID != 0) {
				debug infof("Found song id: %s (%04X)", songID, songTable[songID]);
				result.header.songBase = songTable[songID];
			} else { // scan the song table for something close to the loaded pointer
				debug infof("Trying track pointer $%02X", trackPointerAddress);
				const currentSongPointer = (cast(const(ushort)[])(data[trackPointerAddress .. trackPointerAddress + 2]))[0];
				ushort closest;
				foreach (songTableEntry; songTable) {
					if (songTableEntry > currentSongPointer) {
						continue;
					}
					if (currentSongPointer - songTableEntry < currentSongPointer - closest) {
						closest = songTableEntry;
					}
				}
				debug tracef("Closest to %04X? %04X?", currentSongPointer, closest);
				if (currentSongPointer - closest < 32) {
					result.header.songBase = closest;
				}
			}
		}
		enforce(result.header.instrumentBase != 0, "Instrument table not found");
		if (result.songTableAddress == 0) {
			enforce(result.header.songBase != 0, "Song data and song table not found");
		} else {
			enforce(result.header.songBase != 0, "Song data not found");
		}
	}
	return result;
}
enum fzeroStart = [0x20, 0xCD, 0xCF, 0xBD, 0xE8, 0x00, 0x5D, 0xAF, 0xC8, 0xE8, 0xD0, 0xFB, 0xBC, 0x3F, 0x3D, 0x0E]; // start of f-zero's program
// MOV $0C, #$02
// POP A
// ASL A
// MOV Y, A
// MOV A, <addr - 2> + Y
enum amkPreTable = [0x8F, 0x02, 0x0C, 0xAE, 0x1C, 0xFD, 0xF6];
// MOV $0C, #$02
// ASL A
// MOV Y, A
// MOV A, <addr - 2> + Y
enum amkPreTable109 = [0x8F, 0x02, 0x0C, 0x1C, 0xFD, 0xF6];
enum earlyAMK = [0x20, 0xCD, 0xCF, 0xBD, 0xE8, 0x00, 0x8D, 0x00, 0xD6, 0x00, 0x01, 0xFE, 0xFB, 0xD6, 0x00, 0x02]; //pre-1.0.9
enum laterAMK = [0x20, 0xCD, 0xCF, 0xBD, 0xE8, 0x00, 0xFD, 0xD6, 0x00, 0x01, 0xD6, 0x00, 0x02, 0xD6, 0x00, 0x03]; //1.0.9
enum ExID666IDs {
	songName = 0x01,
	gameName = 0x02,
	artistName = 0x03,
	dumperName = 0x04,
	dateDumped = 0x05,
	emulator = 0x06,
	comments = 0x07,
	ostTitle = 0x10,
	ostDisc = 0x11,
	ostTrack = 0x12,
	publisher = 0x13,
	copyrightYear = 0x14,
	introLength = 0x30,
	loopLength = 0x31,
	endLength = 0x32,
	fadeLength = 0x33,
	mutedChannels = 0x34,
	loopCount = 0x35,
	amplification = 0x36,
}
