module nspcplay.util.wav;

align(1) struct RIFFHeader {
	align(1):
	char[4] magic = "RIFF";
	uint filesize;
	char[4] type;
}

align(1) struct RIFFChunkHeader {
	align(1):
	char[4] fourCC;
	uint size;
	ubyte[0] data;
}

struct RIFFChunk {
	char[4] fourCC;
	const(ubyte)[] data;
}

private union RawBytes(T) {
	T value;
	ubyte[T.sizeof] raw;
}

struct RIFFFile {
	RIFFHeader header;
	RIFFChunk[] chunks;
	void toBytes(W)(ref W writer) const {
		import std.range : put;
		ulong totalLength;
		foreach (chunk; chunks) {
			totalLength += RIFFChunkHeader.sizeof + chunk.data.length;
		}
		assert(totalLength <= uint.max, "Too much data for RIFF file");
		RIFFHeader headerCopy = header;
		headerCopy.filesize = cast(uint)(totalLength + header.type.sizeof);
		put(writer, RawBytes!RIFFHeader(headerCopy).raw);
		foreach (chunk; chunks) {
			assert(chunk.data.length <= uint.max, "Chunk data too large for RIFF file");
			put(writer, RIFFChunkHeader(chunk.fourCC, cast(uint)chunk.data.length));
			put(writer, chunk.data);
		}
	}
	this(const(ubyte)[] data) @safe pure {
		header = (cast(const(RIFFHeader)[])(data[0 .. RIFFHeader.sizeof]))[0];
		data = data[RIFFHeader.sizeof .. $];
		while (data.length > 0) {
			RIFFChunk chunk;
			auto chunkHeader = (cast(const(RIFFChunkHeader)[])data[0 .. RIFFChunkHeader.sizeof])[0];
			chunk.fourCC = chunkHeader.fourCC;
			chunk.data = data[RIFFChunkHeader.data.offsetof .. RIFFChunkHeader.data.offsetof + chunkHeader.size];
			chunks ~= chunk;
			data = data[RIFFChunkHeader.data.offsetof + chunkHeader.size .. $];
		}
	}
}

public align(1) struct WaveFormatHeader {
	align(1):
	ushort format = 1;
	ushort channels;
	uint sampleRate;
	uint secondSize;
	ushort sampleSize;
	ushort bitsPerSample;
}

void dumpWav(T)(T[] samples, uint sampleRate, ushort channels, string filename, SampleChunk sampleChunk) {
	import std.algorithm.iteration : joiner, map;
	import std.array : array;
	import std.range : chain;
	import std.stdio : File;
	auto file = File(filename, "w").lockingBinaryWriter;
	WaveFormatHeader header;
	header.sampleRate = sampleRate;
	header.channels = channels;
	header.bitsPerSample = 16;
	header.sampleSize = cast(ushort)(header.channels * header.bitsPerSample / 8);
	header.secondSize = sampleRate * header.sampleSize;

	sampleChunk.header.sampleLoopCount = cast(uint)sampleChunk.loops.length;
	const(ubyte)[] smplChunk = RawBytes!SampleChunkHeader(sampleChunk.header).raw[].idup.chain(sampleChunk.loops.map!(x => RawBytes!SampleLoop(x).raw[].idup).joiner).array;
	RIFFFile riffFile;
	riffFile.header.type = "WAVE";
	riffFile.chunks ~= RIFFChunk("fmt ", RawBytes!WaveFormatHeader(header).raw[].dup);
	riffFile.chunks ~= RIFFChunk("smpl", smplChunk);
	riffFile.chunks ~= RIFFChunk("data", cast(const(ubyte)[])samples);
	riffFile.toBytes(file);
}

WaveFormatHeader readWaveHeader(const ubyte[] data) @safe pure {
	return (cast(const(WaveFormatHeader)[])(data[0 .. WaveFormatHeader.sizeof]))[0];
}

align(1) struct SampleLoop {
	align(1):
	uint id;
	uint type;
	uint start;
	uint end;
	uint fraction;
	uint count;
}

align(1) struct SampleChunkHeader {
	align(1):
	uint manufacturer;
	uint product;
	uint samplePeriod;
	uint midiUnityNote;
	uint midiPitchFraction;
	uint smpteFormat;
	uint smpteOffset;
	uint sampleLoopCount;
	uint extraDataLength;
	SampleLoop[0] sampleLoops;
}

struct SampleChunk {
	SampleChunkHeader header;
	const(SampleLoop)[] loops;
}

SampleChunk readSampleChunk(const ubyte[] data) @safe pure {
	SampleChunk result;
	const header = (cast(const(SampleChunkHeader)[])(data[0 .. SampleChunkHeader.sizeof]))[0];
	result.header = header;
	result.loops = cast(const(SampleLoop)[])(data[SampleChunkHeader.sampleLoops.offsetof .. SampleChunkHeader.sampleLoops.offsetof + SampleLoop.sizeof * header.sampleLoopCount]);
	return result;
}
