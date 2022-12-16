import std;
import std.experimental.logger;
import nspcplay;

enum Mode {
	undefined,
	spc
}

int main(string[] args) {
	(cast()sharedLog).logLevel = LogLevel.trace;
	enum validArgs = ["spc", "packs"];
	if (args.length == 1) {
		stderr.writefln!"Missing argument - valid options are %-('%s', %)"(validArgs);
		return 1;
	}
	NSPCWriter finished;
	string filename;
	try {
		switch (args[1].toLower) {
			case "spc":
				finished = buildNSPCFromSPC(args[0] ~ args[2 .. $], filename);
				break;
			case "packs":
				finished = buildNSPCFromPackfiles(args[0] ~ args[2 .. $], filename);
				break;
			default:
				stderr.writefln!"Invalid argument - valid options are %-('%s', %)"(validArgs);
				return 1;
		}
	} catch (HelpWantedException e) {
		defaultGetoptPrinter(e.helpMessage, e.options);
		return 1;
	}
	Appender!(ubyte[]) buffer;
	finished.toBytes(buffer);
	const loadedSong = loadNSPCFile(buffer.data);
	File(filename, "w").rawWrite(buffer.data);
	return 0;
}

NSPCWriter buildNSPCFromSPC(string[] args, out string filename) {
	NSPCWriter writer;
	bool autodetect;
	void handleIntegers(string opt, string value) {
		ushort val;
		if (value.startsWith("0x")) {
			val = value[2 .. $].to!ushort(16);
		} else {
			val = value.to!ushort(10);
		}
		switch (opt) {
			case "s|songaddress":
				writer.header.songBase = val;
				break;
			case "a|sampleaddress":
				writer.header.sampleBase = val;
				break;
			case "i|instrumentaddress":
				writer.header.instrumentBase = val;
				break;
			case "addmusick-custominstruments":
				writer.header.extra.customInstruments = val;
				break;
			case "prototype-percussionbase":
			case "addmusick-percussionbase":
				writer.header.extra.percussionBase = val;
				break;
			default:
				throw new Exception("Unknown option "~opt);
		}
	}
	auto helpInfo = getopt(args,
		"d|autodetect", "Try autodetecting some addresses", &autodetect,
		"o|output", "Filename to write to (defaults to filename.nspc)", &filename,
		"s|songaddress", "Address of song data", &handleIntegers,
		"a|sampleaddress", "Address of sample data", &handleIntegers,
		"i|instrumentaddress", "Address of instrument data", &handleIntegers,
		"prototype-percussionbase|addmusick-percussionbase", "Percussion base id (prototype, addmusick variants)", &handleIntegers,
		"addmusick-custominstruments", "Custom instrument address (addmusick variant)", &handleIntegers,
		"v|variant", "NSPC variant to use", &writer.header.variant,
	);
	if (helpInfo.helpWanted || (args.length == 1)) {
		throw new HelpWantedException(format!"NSPC creation tool (SPC conversion)\nUsage: %s spc <filename.spc>"(args[0]), helpInfo.options);
	}
	if (!filename) {
		filename = args[1].baseName.withExtension(".nspc").text;
	}
	auto spcFile = cast(ubyte[])read(args[1]);
	if (autodetect) {
		const sampleDirectory = spcFile[0x1015D];
		infof("Using auto-detected sample directory: %04X", sampleDirectory << 8);
		writer.header.sampleBase = sampleDirectory << 8;
	}
	if (autodetect) {
		if (spcFile.isAMK) {
			infof("Detected AddMusicK song");
			writer.header.variant = nspcplay.Variant.addmusick;
			const songTableAddress = spcFile.find(amkPreTable)[amkPreTable.length .. $];
			const tableAddr = (cast(const(ushort)[])(songTableAddress[0 .. 2]))[0] + 2 + 0x100 + 18;
			infof("Detected song table: %04X", tableAddr - 0x100);
			writer.header.songBase = (cast(const(ushort)[])spcFile[tableAddr .. tableAddr + 2])[0];
			writer.header.instrumentBase = 0x1844;
			infof("Detected song address: %04X", writer.header.songBase);
			const songStartData = spcFile[writer.header.songBase + 0x100 .. $];
			ushort customInstrumentBase = 0;
			while(songStartData[customInstrumentBase .. customInstrumentBase + 2] != [0xFF, 0x00]) {
				customInstrumentBase += 2;
			}
			writer.header.extra.customInstruments = cast(ushort)((cast(const(ushort)[])songStartData[customInstrumentBase + 2 .. customInstrumentBase  + 4])[0] + 6);
			infof("Detected custom instruments: %04X", writer.header.extra.customInstruments);
		}
	}
	writer.packs ~= Pack(0, spcFile[0x100 .. 0x100FF]);
	return writer;
}

NSPCWriter buildNSPCFromPackfiles(string[] args, out string filename) {
	NSPCWriter writer;
	void handleIntegers(string opt, string value) {
		ushort val;
		if (value.startsWith("0x")) {
			val = value[2 .. $].to!ushort(16);
		} else {
			val = value.to!ushort(10);
		}
		switch (opt) {
			case "s|songaddress":
				writer.header.songBase = val;
				break;
			case "a|sampleaddress":
				writer.header.sampleBase = val;
				break;
			case "i|instrumentaddress":
				writer.header.instrumentBase = val;
				break;
			default:
				throw new Exception("Unknown option "~opt);
		}
	}
	auto helpInfo = getopt(args,
		"o|output", "Filename to write to (defaults to filename.nspc)", &filename,
		"s|songaddress", "Address of song data", &handleIntegers,
		"a|sampleaddress", "Address of sample data", &handleIntegers,
		"i|instrumentaddress", "Address of instrument data", &handleIntegers,
		"v|variant", "NSPC variant to use", &writer.header.variant,
	);
	if (helpInfo.helpWanted || (args.length == 1)) {
		throw new HelpWantedException(format!"NSPC creation tool (Packfile conversion)\nUsage: %s spc <filename.bin> [filename.bin ...]"(args[0]), helpInfo.options);
	}
	if (!filename) {
		filename = args[1].baseName.withExtension(".nspc").text;
	}
	foreach(file; args[1 .. $]) {
		const packs = parsePacks(cast(ubyte[])read(file));
		foreach (pack; packs) {
			infof("Adding pack %04X (%s bytes)", pack.address, pack.size);
		}
		writer.packs ~= packs;
	}
	return writer;
}

bool inRange(T)(T val, T lower, T upper) {
	return (val >= lower) && (val < upper);
}

enum amkPreTable = [0x8F, 0x02, 0x0C, 0xAE, 0x1C, 0xFD, 0xF6];

bool isAMK(const ubyte[] data) @safe pure {
	return data[0x500 .. 0x510].among(
		[0x20, 0xCD, 0xCF, 0xBD, 0xE8, 0x00, 0x8D, 0x00, 0xD6, 0x00, 0x01, 0xFE, 0xFB, 0xD6, 0x00, 0x02], //pre-1.0.9
		[0x20, 0xCD, 0xCF, 0xBD, 0xE8, 0x00, 0xFD, 0xD6, 0x00, 0x01, 0xD6, 0x00, 0x02, 0xD6, 0x00, 0x03] //1.0.9
	);
}

class HelpWantedException : Exception {
	Option[] options;
	string helpMessage;
	this(string msg, Option[] opts, string file = __FILE__, ulong line = __LINE__) @safe pure {
		this.helpMessage = msg;
		this.options = opts;
		super("Help wanted!", file, line);
	}
}
