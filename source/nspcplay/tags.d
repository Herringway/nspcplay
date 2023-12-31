module nspcplay.tags;

import std.experimental.logger;

struct APETagHeaderFooter {
	align(1):
	char[8] magic = "APETAGEX";
	uint version_ = 2000;
	uint tagSize;
	uint tagItems;
	Flags globalFlags;
	ulong reserved;
}

enum Encoding {
	utf8 = 0,
	binary = 1,
	external = 2,
	reserved = 3
}

align(1) struct Flags {
	align(1):
	uint flags;
	bool header() const @safe pure {
		return !!(flags & (1 << 29));
	}
	void header(bool newValue) @safe pure {
		flags |= (newValue << 29);
	}
	bool tagHeader() const @safe pure {
		return !!(flags & (1 << 31));
	}
	bool tagFooter() const @safe pure {
		return !!(flags & (1 << 30));
	}
	void tagHeader(bool newValue) @safe pure {
		flags |= (newValue << 31);
	}
	void tagFooter(bool newValue) @safe pure {
		flags |= (newValue << 30);
	}
	Encoding encoding() const @safe pure {
		return cast(Encoding)((flags >> 1) & 3);
	}
}

struct TagHeader {
	align(1):
	uint size;
	Flags flags;
	ubyte[0] key;
}

struct TagPair {
	const(char)[] key;
	private bool _binary;
	private const(ubyte)[] _value;
	this(string key, string value) @safe pure {
		this.key = key;
		this._binary = false;
		this._value = cast(const(ubyte)[])value;
	}
	this(string key, const(ubyte)[] value) @safe pure {
		this.key = key;
		this._binary = true;
		this._value = value;
	}
	const(char)[] str() const @safe pure {
		if (_binary) {
			return "";
		}
		return cast(const(char)[])_value;
	}
	const(ubyte)[] binary() const @safe pure {
		if (!_binary) {
			return [];
		}
		return _value;
	}
}

TagPair[] readTags(const scope ubyte[] data) @safe pure {
	import std.algorithm.searching : countUntil;
	static TagPair[] readTagData(const scope ubyte[] data) {
		TagPair[] result;
		const(ubyte)[] buffer = data;
		while (buffer.length > 0) {
			TagPair pair;
			const header = (cast(const(TagHeader)[])buffer[0 .. TagHeader.sizeof])[0];
			const tagData = buffer[TagHeader.key.offsetof .. $];
			const keyEnd = tagData.countUntil(0) + 1;
			pair.key = cast(const(char)[])tagData[0 .. keyEnd - 1].dup;
			pair._binary = header.flags.encoding == Encoding.binary;
			pair._value = tagData[keyEnd .. keyEnd + header.size].dup;
			buffer = buffer[TagHeader.sizeof + keyEnd + header.size .. $];
			result ~= pair;
		}
		return result;
	}
	if (data.length < APETagHeaderFooter.sizeof) {
		return [];
	}
	const footer = (cast(const(APETagHeaderFooter)[])data[$ - APETagHeaderFooter.sizeof .. $])[0];
	if ((footer.magic == "APETAGEX") && !footer.globalFlags.header) {
		debug(nspclogging) tracef("Found APE tag footer");
		return readTagData(data[$ - footer.tagSize .. $ - APETagHeaderFooter.sizeof]);
	} else {
		const header = (cast(const(APETagHeaderFooter)[])data[0 .. APETagHeaderFooter.sizeof])[0];
		if ((header.magic == "APETAGEX") && header.globalFlags.header) {
			debug(nspclogging) tracef("Found APE tag header");
			return readTagData(data[APETagHeaderFooter.sizeof .. APETagHeaderFooter.sizeof + footer.tagSize]);
		}
	}
	return [];
}

const(ubyte)[] tagsToBytes(scope const(TagPair)[] tags) @safe pure {
	APETagHeaderFooter[1] footer;
	const(ubyte)[] result;
	static const(ubyte)[] tagToBytes(TagPair tag) {
		const(ubyte)[] result;
		TagHeader[1] header;
		assert(tag._value.length < uint.max);
		header[0].size = cast(uint)tag._value.length;
		result ~= cast(const(ubyte)[])header[];
		result ~= tag.key;
		result ~= 0;
		result ~= tag._value;
		return result;
	}
	footer[0].tagSize = APETagHeaderFooter.sizeof;
	footer[0].globalFlags.header = false;
	foreach (tag; tags) {
		const data = tagToBytes(tag);
		result ~= data;
		footer[0].tagSize += data.length;
		footer[0].tagItems++;
	}
	result ~= cast(const(ubyte)[])footer[];
	return result;
}

@safe pure unittest {
	static immutable ubyte[] sample = [0x0B, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x61, 0x6C, 0x62, 0x75, 0x6D, 0x00, 0x4A, 0x75, 0x73, 0x74, 0x20, 0x61, 0x20, 0x74, 0x65, 0x73, 0x74, 0x02, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x74, 0x69, 0x74, 0x6C, 0x65, 0x00, 0x01, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x61, 0x72, 0x74, 0x69, 0x73, 0x74, 0x00, 0x41, 0x50, 0x45, 0x54, 0x41, 0x47, 0x45, 0x58, 0xD0, 0x07, 0x00, 0x00, 0x58, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
	const tags = readTags(sample);
	assert(tags.length == 3);
	with(tags[0]) {
		assert(key == "album");
		assert(str == "Just a test");
	}
	with(tags[1]) {
		assert(key == "title");
		assert(binary == [0x01, 0x02]);
	}
	with(tags[2]) {
		assert(key == "artist");
		assert(str == "");
	}
}
