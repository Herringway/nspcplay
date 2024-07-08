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
		stderr.writefln!"Missing argument - valid options are %-('%s'%|, %)"(validArgs);
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
		writer.header = detectParameters(spcFile[0x100 .. 0x10100], spcFile[0x10100 .. 0x10180]).header;
		infof("Detected parameters: Variant: %s, Song: %04X, Instruments: %04X, Samples: %04X", writer.header.variant, writer.header.songBase, writer.header.instrumentBase, writer.header.sampleBase);
	}
	const id666 = (cast(ID666Tags[])spcFile[0x2E .. 0x100])[0];
	void addTag(string tag, const char[] value) {
		if (value != "") {
			writer.tags ~= TagPair(tag, value.idup);
		}
	}
	addTag("title", id666.text.songTitle.fromStringz);
	addTag("album", id666.text.gameTitle.fromStringz);
	addTag("artist", id666.text.songArtist.fromStringz);
	addTag("comment", id666.text.comments.fromStringz);
	string[ubyte] emulators = [1: "ZSNES", 2: "SNES9x"];
	addTag("emulator", emulators.get(id666.text.emulator, ""));
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
	return !!data[0x500 .. 0x510].among(
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
