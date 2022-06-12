import std;
import std.experimental.logger;
import nspc;

enum Mode {
	undefined,
	spc
}

void main(string[] args) {
	enum validArgs = ["spc", "packs"];
	if (args.length == 1) {
		stderr.writefln!"Missing argument - valid options are %-('%s', %)"(validArgs);
		return;
	}
	switch (args[1].toLower) {
		case "spc":
			buildNSPCFromSPC(args[0] ~ args[2 .. $]);
			break;
		case "packs":
			buildNSPCFromPackfiles(args[0] ~ args[2 .. $]);
			break;
		default:
			stderr.writefln!"Invalid argument - valid options are %-('%s', %)"(validArgs);
			break;
	}
}

void buildNSPCFromSPC(string[] args) {
	NSPCFileHeader header;
	void handleIntegers(string opt, string value) {
		ushort val;
		if (value.startsWith("0x")) {
			val = value[2 .. $].to!ushort(16);
		} else {
			val = value.to!ushort(10);
		}
		switch (opt) {
			case "s|songaddress":
				header.songBase = val;
				break;
			case "a|sampleaddress":
				header.sampleBase = val;
				break;
			case "i|instrumentaddress":
				header.instrumentBase = val;
				break;
			default:
				throw new Exception("Unknown option "~opt);
		}
	}
	string filename;
	auto helpInfo = getopt(args,
		"o|output", "Filename to write to (defaults to filename.nspc)", &filename,
		"s|songaddress", "Address of song data", &handleIntegers,
		"a|sampleaddress", "Address of sample data", &handleIntegers,
		"i|instrumentaddress", "Address of instrument data", &handleIntegers,
	);
	if (helpInfo.helpWanted || (args.length == 1)) {
		defaultGetoptPrinter(format!"NSPC creation tool (SPC conversion)\nUsage: %s spc <filename.spc>"(args[0]), helpInfo.options);
		return;
	}
	if (!filename) {
		filename = args[1].baseName.withExtension(".nspc").text;
	}
	auto spcFile = cast(ubyte[])read(args[1]);
	auto f = File(filename, "w");
	f.rawWrite([header]);
	f.rawWrite(cast(ushort[])[65535, 0]);
	f.rawWrite(spcFile[0x100 .. 0x100FF]);
	f.rawWrite(cast(ushort[])[0]);
}


void buildNSPCFromPackfiles(string[] args) {
	NSPCFileHeader header;
	void handleIntegers(string opt, string value) {
		ushort val;
		if (value.startsWith("0x")) {
			val = value[2 .. $].to!ushort(16);
		} else {
			val = value.to!ushort(10);
		}
		switch (opt) {
			case "s|songaddress":
				header.songBase = val;
				break;
			case "a|sampleaddress":
				header.sampleBase = val;
				break;
			case "i|instrumentaddress":
				header.instrumentBase = val;
				break;
			default:
				throw new Exception("Unknown option "~opt);
		}
	}
	string filename;
	auto helpInfo = getopt(args,
		"o|output", "Filename to write to (defaults to filename.nspc)", &filename,
		"s|songaddress", "Address of song data", &handleIntegers,
		"a|sampleaddress", "Address of sample data", &handleIntegers,
		"i|instrumentaddress", "Address of instrument data", &handleIntegers,
	);
	if (helpInfo.helpWanted || (args.length == 1)) {
		defaultGetoptPrinter(format!"NSPC creation tool (Packfile conversion)\nUsage: %s spc <filename.bin> [filename.bin ...]"(args[0]), helpInfo.options);
		return;
	}
	if (!filename) {
		filename = args[1].baseName.withExtension(".nspc").text;
	}
	auto f = File(filename, "w");
	f.rawWrite([header]);
	foreach(file; args[1 .. $]) {
		auto packFile = cast(ubyte[])read(file);
		size_t offset;
		while(true) {
			auto size = (cast(ushort[])(packFile[offset .. offset + 2]))[0];
			if (size == 0) {
				break;
			}
			auto spcOffset = (cast(ushort[])(packFile[offset + 2 .. offset + 4]))[0];
			infof("Song subpack: $%04X, %04X bytes", spcOffset, size);
			f.rawWrite(packFile[offset .. offset + size + 4]);
			offset += size + 4;
		}
	}
	f.rawWrite(cast(ushort[])[0]);
}
