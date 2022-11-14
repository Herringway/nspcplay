module nspcplay.song;

import std.algorithm.comparison : max, min;
import std.algorithm.sorting : sort;
import std.algorithm.searching : canFind;
import std.exception : enforce;
import std.experimental.logger;
import std.format : format;

import nspcplay.common;
import nspcplay.samples;
import nspcplay.sequence;

private enum maxInstruments = 64;
private enum maxSampleCount = 128;

enum PhraseType {
	pattern,
	jumpLimited,
	jump,
	end,
	fastForwardOn,
	fastForwardOff,
}
///
struct NSPCFileHeader {
	align(1):
	static union Extra {
		struct { //Variant.prototype
			ushort percussionBase;
		}
		ubyte[19] reserved;
	}
	/// Which version of NSPC to use
	Variant variant;
	/// Base SPC address of the song's sequence data
	ushort songBase;
	/// Base SPC address of the instruments
	ushort instrumentBase;
	/// Base SPC address of the samples
	ushort sampleBase;
	/// Release table to use
	ReleaseTable releaseTable;
	/// Volume table to use
	VolumeTable volumeTable;
	/// Extra information for variants
	Extra extra;
	/// Number of FIR coefficient tables
	ubyte firCoefficientTableCount;
}

struct Phrase {
	PhraseType type;
	ushort id;
	ushort jumpTimes;
	void toString(S)(ref S sink) const {
		import std.format : formattedWrite;
		final switch (type) {
			case PhraseType.pattern:
				sink.formattedWrite!"Play pattern %04X"(id);
				break;
			case PhraseType.jumpLimited:
				sink.formattedWrite!"Jump to phrase %s %s times"(id, jumpTimes);
				break;
			case PhraseType.jump:
				sink.formattedWrite!"Jump to phrase %s"(id);
				break;
			case PhraseType.end:
				sink.formattedWrite!"End of song"();
				break;
			case PhraseType.fastForwardOn:
				sink.formattedWrite!"Fast forward on"();
				break;
			case PhraseType.fastForwardOff:
				sink.formattedWrite!"Fast forward off"();
				break;
		}
	}
}

struct Song {
	ushort address;
	ubyte changed;
	const(Phrase)[] order;
	ushort[8][ushort] trackLists;
	const(ubyte)[][ushort] tracks;
	const(ubyte[8])[] firCoefficients;
	ubyte[8] releaseTable;
	ubyte[16] volumeTable;
	Variant variant;
	const(Instrument)[] instruments;
	Sample[128] samples;
	ubyte[256] percussionNotes;
	size_t percussionBase;
	/// Load a single sequence pack at a given address
	void loadSequencePack(const(ubyte)[] data, ushort base) @safe {
		loadSequence(data, base);
	}
	/// Load a single sequence pack, automatically detecting the address from the pack header
	void loadSequencePack(const(ubyte)[] data) @safe {
		ushort base = read!ushort(data, 2);
		loadSequence(data, base);
	}
	private void loadSequence(const(ubyte)[] data, ushort base) @safe {
		ubyte[65536] buffer;
		loadAllSubpacks(buffer, data);
		decompileSong(buffer[], this, base, cast(int)(base + data.length));
	}
	/// Load instruments from provided packs at the given addresses
	void loadInstruments(const(ubyte)[][] packs, ushort instrumentBase, ushort sampleBase) @safe {
		ubyte[65536] buffer;
		foreach (pack; packs) {
			loadInstrumentPack(buffer, pack);
		}
		initializeInstruments(buffer, instrumentBase, sampleBase);
	}
	/// Load instruments from provided packs at the given addresses
	void loadInstrumentPack(scope ref ubyte[65536] buffer, const(ubyte)[] pack) @safe {
		loadAllSubpacks(buffer[], pack);
	}
	void initializeInstruments(scope const ref ubyte[65536] buffer, ushort instrumentBase, ushort sampleBase) @safe {
		NSPCFileHeader fakeHeader;
		fakeHeader.instrumentBase = instrumentBase;
		fakeHeader.sampleBase = sampleBase;
		processInstruments(this, buffer, fakeHeader);
	}
	private void validatePhrases() @safe {
		import std.algorithm.comparison : among;
		bool endFound;
		foreach (phrase; order) {
			enforce!NSPCException(!endFound, "Phrases found after end of song");
			final switch (phrase.type) {
				case PhraseType.end:
					endFound = true;
					break;
				case PhraseType.fastForwardOn:
				case PhraseType.fastForwardOff:
					throw new NSPCException("Fast forward not yet supported");
				case PhraseType.jump:
				case PhraseType.jumpLimited:
					enforce!NSPCException(phrase.id < order.length, "Cannot jump past end of song");
					break;
				case PhraseType.pattern:
					break;
			}
		}
		enforce!NSPCException(order.length > 0, "No phrases loaded");
		enforce!NSPCException(order[$ - 1].type.among(PhraseType.end, PhraseType.jump), "Phrase list must have an end phrase");
	}
	private void validateInstrument(size_t id) @safe {
		enforce!NSPCException(id < instruments.length, format!"Invalid instrument %s - Index out of bounds"(id));
		const idata = instruments[id];
		enforce!NSPCException(samples[idata.sampleID].isValid, format!"Invalid instrument %s - Invalid sample %s"(id, idata.sampleID));
		if (idata.tuning == 0) {
			tracef("Suspicious instrument %s - no tuning (will be silent)", id);
		}
	}
	private void validateTrack(scope const ubyte[] track, bool isSub, ref ubyte tmpPercussionBase) @safe {
		const(ubyte)[] data = track;
		bool endReached;
		while (data.length > 0) {
			size_t length;
			const command = readCommand(variant, data, length);
			data = data[length .. $];
			enforce!NSPCException(!endReached, "Track must end with terminator");
			final switch (command.type) {
				case VCMDClass.terminator:
					endReached = true;
					break;
				case VCMDClass.noteDuration:
					enforce!NSPCException(data.length > 0, "Track can not end with note-length code");
					break;
				case VCMDClass.percussion:
					validateInstrument(absoluteInstrumentID(command.instrument, tmpPercussionBase, true));
					break;
				case VCMDClass.special:
					enforce!NSPCException(command.special != VCMD.invalid, format!"Invalid code command %02X"(command.relative));

					if (command.special == VCMD.instrument) {
						validateInstrument(absoluteInstrumentID(command.parameters[0], tmpPercussionBase, false));
					}
					if (command.special == VCMD.subRoutine) {
						enforce!NSPCException(!isSub, "Can't call sub from within a sub");
						ushort sub = read!ushort(command.parameters);
						enforce!NSPCException(sub in tracks, format!"Subroutine %d not present"(sub));
						enforce!NSPCException(command.parameters[2] != 0, "Subroutine loop count can not be 0");
					}
					if (command.special == VCMD.percussionBaseInstrumentRedefine) {
						tmpPercussionBase = command.parameters[0];
					}
					break;
				case VCMDClass.note: // nothing to validate here
				case VCMDClass.tie:
				case VCMDClass.rest:
					break;
			}
		}
	}
	package size_t absoluteInstrumentID(size_t id, size_t base, bool percussion) const @safe pure nothrow {
		if (id >= percussionID(variant)) {
			percussion = true;
			id -= percussionID(variant);
		}
		if (percussion) {
			id += base + percussionBase;
		}
		return id;
	}
	package ubyte defaultTempo() const @safe pure nothrow {
		if (variant == Variant.prototype) {
			return 0x36;
		}
		return 0x20;
	}
	void toString(S)(ref S sink) const {
		import std.format : formattedWrite;
		import std.range : put;
		void printSequence(const(ubyte)[] data) {
			bool done;
			while((data.length > 0) && (!done)) {
				size_t nextOffset;
				const command = readCommand(variant, data, nextOffset);
				sink.formattedWrite!"%x\n"(command);
				if (command.type == VCMDClass.terminator) {
					done = true;
				}
				data = data[nextOffset .. $];
			}
		}
		sink.formattedWrite!"Phrases: %s\n"(order);
		foreach (id, trackList; trackLists) {
			sink.formattedWrite!"Pattern %04X\n"(id);
			foreach (trackID, track; trackList) {
				put(sink, "----------\n");
				sink.formattedWrite!"Track %s ($%04X)\n"(trackID, track);
				printSequence(tracks.get(track, []));
			}
		}
		foreach (subroutineID, subroutine; tracks) {
			bool isTrack;
			foreach (trackList; trackLists) {
				if (trackList[].canFind(subroutineID)) {
					isTrack = true;
					break;
				}
			}
			if (isTrack) {
				continue;
			}
			put(sink, "----------\n");
			sink.formattedWrite!"Subroutine %04X\n"(subroutineID);
			printSequence(subroutine);
		}
	}
	private void _tmp() const {
		import std.range : nullSink;
		auto n = nullSink;
		toString(n);
	}
}
private struct PrototypeInstrument {
	align(1):
	ubyte sampleID;
	ADSRGain adsrGain;
	ubyte tuning;
	Instrument opCast(T: Instrument)() const {
		return Instrument(sampleID, adsrGain, tuning, 0);
	}
}
private struct PrototypePercussion {
	align(1):
	ubyte sampleID;
	ADSRGain adsrGain;
	ubyte tuning;
	ubyte note;
	Instrument opCast(T: Instrument)() const {
		return Instrument(sampleID, adsrGain, tuning, 0);
	}
}

// note style tables, from 6F80
private immutable ubyte[8][] releaseTables = [
	[0x32, 0x65, 0x7F, 0x98, 0xB2, 0xCB, 0xE5, 0xFC],
	[0x33, 0x66, 0x7F, 0x99, 0xB2, 0xCC, 0xE5, 0xFC], // HAL (Earthbound)
	[0x65, 0x7F, 0x98, 0xB2, 0xCB, 0xE5, 0xF2, 0xFC], // HAL (Kirby Super Star)
	[0x66, 0x7F, 0x99, 0xB2, 0xCC, 0xE5, 0xF2, 0xFC], // HAL (Kirby's Dream Course)
];
private immutable ubyte[16][] volumeTables = [
	[0x19, 0x32, 0x4C, 0x65, 0x72, 0x7F, 0x8C, 0x98, 0xA5, 0xB2, 0xBF, 0xCB, 0xD8, 0xE5, 0xF2, 0xFC],
	[0x19, 0x33, 0x4C, 0x66, 0x72, 0x7F, 0x8C, 0x99, 0xA5, 0xB2, 0xBF, 0xCC, 0xD8, 0xE5, 0xF2, 0xFC], // HAL (Earthbound)
	[0x4C, 0x59, 0x6D, 0x7F, 0x87, 0x8E, 0x98, 0xA0, 0xA8, 0xB2, 0xBF, 0xCB, 0xD8, 0xE5, 0xF2, 0xFC], // HAL (Kirby Super Star)
	[0x59, 0x66, 0x72, 0x7F, 0x87, 0x8E, 0x99, 0xA0, 0xA8, 0xB2, 0xBF, 0xCC, 0xD8, 0xE5, 0xF2, 0xFC], // HAL (Kirby's Dream Course)
];
private immutable ubyte[8][] defaultFIRCoefficients = [
	[0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
	[0x58, 0xBF, 0xDB, 0xF0, 0xFE, 0x07, 0x0C, 0x0C],
	[0x0C, 0x21, 0x2B, 0x2B, 0x13, 0xFE, 0xF3, 0xF9],
	[0x34, 0x33, 0x00, 0xD9, 0xE5, 0x01, 0xFC, 0xEB],
];

private const(ubyte)[] loadAllSubpacks(scope ubyte[] buffer, const(ubyte)[] pack) @safe {
	ushort size, base;
	while (true) {
		if (pack.length == 0) {
			break;
		}
		size = read!ushort(pack);
		if (size == 0) {
			break;
		}
		base = read!ushort(pack, 2);
		debug(nspclogging) tracef("Loading subpack to %X (%s bytes)", base, size);
		enforce!NSPCException(base + size <= ushort.max, "Invalid pack - base + size exceeds 64KB memory limit");
		buffer[base .. base + size] = pack[4 .. size + 4];
		pack = pack[size + 4 .. $];
	}
	return pack[2 .. $];
}
/// Load an NSPC file
Song loadNSPCFile(const(ubyte)[] data) @safe {
	Song song;
	ubyte[65536] buffer;
	auto header = read!NSPCFileHeader(data);
	debug(nspclogging) tracef("Loading NSPC - so: %X, i: %X, sa: %X, variant: %s", header.songBase, header.instrumentBase, header.sampleBase, header.variant);
	song.variant = header.variant;
	assert(header.volumeTable < volumeTables.length, "Invalid volume table");
	assert(header.releaseTable < releaseTables.length, "Invalid release table");
	song.volumeTable = volumeTables[header.volumeTable];
	song.releaseTable = releaseTables[header.releaseTable];
	debug(nspclogging) tracef("Release table: %s, volume table: %s", header.releaseTable, header.volumeTable);
	const remaining = loadAllSubpacks(buffer[], data[NSPCFileHeader.sizeof .. $]);
	processInstruments(song, buffer, header);
	decompileSong(buffer[], song, header.songBase, buffer.length - 1);
	if (header.firCoefficientTableCount == 0) {
		song.firCoefficients = defaultFIRCoefficients;
	} else {
		song.firCoefficients = cast(const(ubyte[8])[])remaining[0 .. 8 * header.firCoefficientTableCount];
	}
	debug(nspclogging) tracef("FIR coefficients: %s", song.firCoefficients);
	return song;
}
private void decompileSong(scope ubyte[] data, ref Song song, int startAddress, int endAddress) @safe {
	immutable copyData = data.idup;
	song.address = cast(ushort) startAddress;
	song.changed = false;

	// Get order length and repeat info (at this point, we don't know how
	// many patterns there are, so the pattern pointers aren't validated yet)
	const ushort[] wpO = cast(ushort[]) data[startAddress .. $ -  ($ - startAddress) % 2];
	uint index;
	uint phraseCount;
	bool nextEnds;
	while (wpO[index] != 0) {
		if (wpO[index] < 0x100) {
			if (wpO[index] >= 0x80) {
				nextEnds = true;
			}
			index++;
		}
		if (nextEnds) {
			break;
		}
		phraseCount++;
		index++;
	}
	ushort phraseID(ushort address) {
		ushort id;
		const offset = (address - startAddress) >> 1;
		uint o;
		bool skip;
		while ((o < offset) && (wpO[o] != 0)) {
			if (wpO[o] < 0x80) {
				skip = true;
			} else if ((wpO[o] > 0x81) && (wpO[o] < 0x100)) {
				skip = true;
			}
			if (!skip) {
				id++;
			} else {
				skip = false;
			}
			o++;
		}
		return id;
	}
	enforce!NSPCException(phraseCount > 0, "No phrases in song");
	auto newPhrases = new Phrase[](phraseCount + 1);
	index++;

	const fpIndex = index;
	const firstPattern = startAddress + index * 2;

	const phrases = wpO[0 .. index];
	// Now the number of patterns is known, so go back and get the order
	size_t idx;
	foreach (ref order; newPhrases) {
		if (phrases[idx] == 0) {
			order.type = PhraseType.end;
		} else if (phrases[idx] == 0x80) {
			order.type = PhraseType.fastForwardOn;
		} else if (phrases[idx] == 0x81) {
			order.type = PhraseType.fastForwardOff;
		} else if (phrases[idx] < 0x80) {
			order.type = PhraseType.jumpLimited;
			order.jumpTimes = phrases[idx];
			idx++;
			order.id = phraseID(phrases[idx]);
		} else if ((phrases[idx] > 0x81) && (phrases[idx] < 0x100)) {
			order.type = PhraseType.jump;
			idx++;
			order.id = phraseID(phrases[idx]);
		} else {
			order.type = PhraseType.pattern;
			order.id = phrases[idx];
		}
		idx++;
	}
	song.order = newPhrases;

	debug(nspclogging) tracef("Phrases: %(%s, %)", song.order);
	song.validatePhrases();

	song.trackLists = null;
	song.tracks = null;

	index = fpIndex;
	ushort[] subTable;
	ubyte tmpPercussionBase;
	foreach (phrase; song.order) {
		if (phrase.type != PhraseType.pattern) {
			continue;
		}
		song.trackLists.require(phrase.id, (cast(const(ushort[8])[])(data[phrase.id .. phrase.id + 8 * ushort.sizeof]))[0]);
	}
	foreach (trackList; song.trackLists) {
		foreach (idx, trackAddress; trackList) {
			if (trackAddress == 0) {
				continue;
			}
			debug(nspclogging) tracef("Decompiling track at %04X", trackAddress);
			song.tracks[trackAddress] = decompileTrack(copyData, trackAddress, 0x10000, song, subTable, tmpPercussionBase);
		}
	}
}
private const(ubyte)[] decompileTrack(immutable(ubyte)[] data, ushort start, uint next, ref Song song, ref ushort[] subTable, ref ubyte tmpPercussionBase) @safe {
	// Determine the end of the track.
	const(ubyte)[] trackEnd = data[start .. $];
	size_t length;
	size_t totalLength;
	while (true) {
		const command = readCommand(song.variant, trackEnd, length);
		trackEnd = trackEnd[length .. $];
		totalLength += length;
		if (command.type == VCMDClass.terminator) {
			break;
		}
		if (command.special == VCMD.subRoutine) { //decompile subroutines too
			ushort subPtr = read!ushort(command.parameters);

			song.tracks.require(subPtr, () {
				const(ubyte)[] st;
				const(ubyte)[] subData = data[subPtr .. $];
				size_t subLength;
				size_t subTotalLength;
				while (readCommand(song.variant, subData, subLength).type != VCMDClass.terminator) {
					subData = subData[subLength .. $];
					subTotalLength += subLength;
				}
				st = data[subPtr .. subPtr + subTotalLength + 1];
				song.validateTrack(st, true, tmpPercussionBase);
				return st;
			}());
		}
	}
	const track = data[start .. start + totalLength];

	song.validateTrack(track, false, tmpPercussionBase);
	return track;
}
private void processInstruments(ref Song song, scope const ubyte[] buffer, const NSPCFileHeader header) @safe {
	decodeSamples(song, buffer, cast(const(ushort[2])[])(buffer[header.sampleBase .. header.sampleBase + maxSampleCount * 4]));
	song.instruments = [];
	song.instruments.reserve(maxInstruments);
	if (song.variant == Variant.prototype) {
		size_t instrumentCount = maxInstruments;
		if (header.instrumentBase < header.extra.percussionBase) {
			instrumentCount = min(instrumentCount, (header.extra.percussionBase - header.instrumentBase) / PrototypeInstrument.sizeof);
		}
		foreach (idx, instrument; cast(const(PrototypeInstrument)[])(buffer[header.instrumentBase .. header.instrumentBase + instrumentCount * PrototypeInstrument.sizeof])) {
			song.instruments ~= cast(Instrument)instrument;
		}
		song.percussionBase = instrumentCount;
		foreach (idx, percussion; cast(const(PrototypePercussion)[])(buffer[header.extra.percussionBase .. header.extra.percussionBase + maxInstruments * PrototypePercussion.sizeof])) {
			song.instruments ~= cast(Instrument)percussion;
			song.percussionNotes[idx] = cast(ubyte)(percussion.note - 0x80);
		}
	} else {
		foreach (idx, instrument; cast(const(Instrument)[])(buffer[header.instrumentBase .. header.instrumentBase + maxInstruments * Instrument.sizeof])) {
			song.instruments ~= instrument;
		}
		song.percussionNotes = 0x24;
	}
	debug(nspclogging) foreach (idx, instrument; song.instruments) {
		 if ((instrument.sampleID < song.samples.length) && song.samples[instrument.sampleID].isValid && (instrument.tuning != 0)) {
			tracef("%s (%s) - %s", idx, ((song.percussionBase > 0) && (idx > song.percussionBase)) ? "Percussion" : "Standard", instrument);
		}
	}
}
private void decodeSamples(ref Song song, scope const ubyte[] buffer, const scope ushort[2][] ptrtable) nothrow @safe {
	foreach (idx, ref sample; song.samples) {
		const start = ptrtable[idx][0];
		const loop = ptrtable[idx][1];
		if (start == 0 || start == 0xffff) {
			continue;
		}
		try {
			sample = decodeSample(buffer, start, loop);
		} catch (Exception e) {
			debug tracef("Couldn't load sample %d: %s", idx, e.msg);
		}
	}
}
