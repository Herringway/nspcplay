module nspcplay.samples;

import nspcplay.common;
import nspcplay.interpolationtables;
import std.bitmanip;
import std.exception;
import std.range;

enum BRRFilter {
	filter0,
	filter1,
	filter2,
	filter3,
}
struct BRRBlock {
	align(1):
	mixin(bitfields!(
		bool, "end", 1,
		bool, "loop", 1,
		BRRFilter, "filter", 2,
		ubyte, "range", 4,
	));
	ubyte[8] samples;
	this(ubyte[9] raw) @safe pure {
		this.tupleof[0] = raw[0];
		samples[] = raw[1 .. $];
	}
}

Sample decodeSample(scope const ubyte[] buffer, ushort start, ushort loop) @safe {
	Sample sample;
	sample.start = start;
	const blocks = getBRRBlocks(buffer, start);

	sample.loopLength = blocks[$ - 1].loop ? ((start - loop) / cast(int)BRRBlock.sizeof + cast(int)blocks.length) * 16 : 0;
	enforce(sample.loopLength >= 0, "Invalid loop");
	enforce(sample.loopLength < blocks.length * 16, "Invalid loop");

	sample.data = new short[](sample.loopLength + blocks.length * 16);

	short[2] pExtra;
	foreach (idx, block; blocks.chain(blocks[$ - sample.loopLength / 16 .. $]).enumerate) {
		auto sampleChunk = sample.data[idx * 16 .. (idx + 1) * 16];
		decodeBRRBlock(sampleChunk, pExtra, block);
		pExtra[] = sampleChunk[$ - 2 .. $];
	}
	uint loopsAdded = 1;
	outer: while (sample.loopLength) {
		short[16] tmp;
		decodeBRRBlock(tmp, pExtra, blocks[$ - sample.loopLength / 16]);
		enforce(loopsAdded < 10_000, "BRR sample does not stabilize");
		foreach_reverse(i; 0 .. loopsAdded) {
			if (tmp[] == sample.data[blocks.length * 16 + sample.loopLength * i .. blocks.length * 16 + sample.loopLength * i + 16]) {
				break outer;
			}
		}
		sample.data.length += sample.loopLength;
		auto newLoop = sample.data[$ - sample.loopLength .. $];
		foreach (idx, block; blocks[$ - sample.loopLength / 16 .. $]) {
			auto sampleChunk = newLoop[idx * 16 .. (idx + 1) * 16];
			decodeBRRBlock(sampleChunk, pExtra, block);
			pExtra[] = sampleChunk[$ - 2 .. $];
		}
		loopsAdded++;
	}
	sample.loopLength *= loopsAdded;
	return sample;
}

unittest {
	with(decodeSample(cast(immutable(ubyte)[])import("wilhelm.brr"), 0, 0)) {
		assert(start == 0);
		assert(data.length == 12032);
		assert(data == cast(immutable(short)[])import("wilhelm.pcms16"));
	}
	with(decodeSample(cast(immutable(ubyte)[])import("distortguitar.brr"), 0, 0x2E2)) {
		assert(start == 0);
		assert(loopLength == 6960);
		assert(data == cast(immutable(short)[])import("distortguitar.pcms16"));
	}
}

void decodeBRRBlock(scope short[] buffer, short[2] lastSamples, const BRRBlock block) nothrow @safe pure {
	int range = block.range;
	int filter = block.filter;

	for (int i = 0; i < 16; i++) {
		int s = block.samples[i / 2];

		if (i % 2 == 0) {
			s >>= 4;
		} else {
			s &= 0x0F;
		}

		if (s >= 8) {
			s -= 16;
		}

		s <<= range - 1;
		if (range > 12) {
			s = (s < 0) ? -(1 << 11) : 0;
		}
		final switch (filter) {
			case BRRFilter.filter1: // + lastSamples[1] * 0.46875
				s += (lastSamples[1] >> 1) + ((-lastSamples[1]) >> 5);
				break;
			case BRRFilter.filter2: // + lastSamples[1] * 0.953125 - lastSamples[0] * 0.46875
				s += lastSamples[1] - (lastSamples[0] >> 1) + (lastSamples[0] >> 5) + ((lastSamples[1] * -3) >> 6);
				break;
			case BRRFilter.filter3:// + lastSamples[1] * 0.8984375 - lastSamples[0] * 0.40625
				s += lastSamples[1] - (lastSamples[0] >> 1) + ((lastSamples[1] * -13) >> 7) + (((lastSamples[0] >> 1) * 3) >> 4);
				break;
			case BRRFilter.filter0:
				break;
		}

		if (cast(short) s != s) {
			s = (s >> 31) ^ 0x7FFF;
		}

		s *= 2;


		lastSamples[0] = lastSamples[1];
		lastSamples[1] = cast(short) s;
		buffer[i] = cast(short) s;
	}
}

@safe pure unittest {
	import core.exception : AssertError;
	import std.stdio : writefln;
	void assertBlock(scope const BRRBlock input, short[16] output, short[2] prev = [0, 0], string file = __FILE__, size_t line = __LINE__) {
		short[16] buffer;
		decodeBRRBlock(buffer, prev, input);
		if(buffer != output) {
			debug writefln!"%(%04X %)"(buffer[]);
			debug writefln!"%(%04X %)"(output[]);
			throw new AssertError("Test failed", file, line);
		}
	}
	// filter 0
	assertBlock(BRRBlock([0xC2, 0x2C, 0x1E, 0xB3, 0x40, 0xEF, 0x10, 0x41, 0xE0]), [0x2000, -0x4000, 0x1000, -0x2000, -0x5000, 0x3000, 0x4000, 0x0000, -0x2000, -0x1000, 0x1000, 0x0000, 0x4000, 0x1000, -0x2000, 0x0000]);
	// filter 1
	assertBlock(BRRBlock([0xB6, 0x32, 0x11, 0x10, 0xDB, 0xDC, 0xC0, 0x11, 0x35]), [0x19C6, 0x2828, 0x2DA4, 0x32C8, 0x379A, 0x3420, 0x18DE, cast(short)0xEF50, cast(short)0xD85A, cast(short)0xBAD4, cast(short)0x9F26, cast(short)0xA532, cast(short)0xB2DE, cast(short)0xBFB0, cast(short)0xDBB4, 0x05F8], [0x00EA, 0x01E6]);
	// filter 2
	assertBlock(BRRBlock([0xAA, 0x4D, 0xA0, 0x10, 0x00, 0x00, 0x01, 0xF0, 0x01]), [0x10AC, 0x131E, -0x332, cast(short)0xE7FA, cast(short)0xD932, cast(short)0xCC8A, cast(short)0xC246, cast(short)0xBA92, cast(short)0xB584, cast(short)0xB318, cast(short)0xB338, cast(short)0xB9BA, cast(short)0xBE04, cast(short)0xC416, cast(short)0xCBA4, cast(short)0xD85A], [0x00B0, 0x00B2]);
	// filter 3
	assertBlock(BRRBlock([0x9E, 0x2B, 0xC3, 0xF0, 0x48, 0x43, 0xA6, 0x2F, 0x03]), [0x0400, cast(short)0xFD30, cast(short)0xEFB2, cast(short)0xEAFA, cast(short)0xE576, cast(short)0xE164, cast(short)0xE68E, cast(short)0xDB24, cast(short)0xDA70, cast(short)0xE072, cast(short)0xD9D0, cast(short)0xE102, cast(short)0xEB54, cast(short)0xF208, cast(short)0xF7B0, 0x0268]);
}

private const(BRRBlock)[] getBRRBlocks(const scope ubyte[] spcMemory, ushort start) pure @safe {
	const blocks = cast(const(BRRBlock)[])spcMemory[start .. start + ($ - start) / BRRBlock.sizeof * BRRBlock.sizeof];
	foreach (idx, block; blocks) {
		if (block.end) {
			return blocks[0 .. idx + 1];
		}
	}
	throw new Exception("Invalid BRR sample");
}

short interpolate(Interpolation style, scope const short[] buf, int position) nothrow @safe pure {
	final switch (style) {
		case Interpolation.none:
			return buf[0];
		case Interpolation.gaussian:
			return gaussianInterpolation(buf[0 .. 4], (position >> 4) & 0xFF);
		case Interpolation.cubic:
			return cubicInterpolation(buf[0 .. 4], (position >> 4) & 0xFF);
		case Interpolation.sinc:
			return sincInterpolation(buf[0 .. 8], (position & 0xFF0) >> 1);
		case Interpolation.linear:
			return linearInterpolation(buf[0 .. 2], position & 0xFFF);
	}
}

short linearInterpolation(short[2] latest, ushort val) nothrow @safe pure {
	int result = (4096 - val) * latest[0];
	result += val * latest[1];
	result >>= 12;
	return cast(short)result;
}

@safe pure unittest {
	assert(linearInterpolation([0, 0], 0) == 0);
	assert(linearInterpolation([0, 0], 4096) == 0);
	assert(linearInterpolation([0x7000, 0], 4096) == 0);
	assert(linearInterpolation([0x7000, 0], 0) == 0x7000);
	assert(linearInterpolation([0x7000, 0], 2048) == 0x3800);
}

short gaussianInterpolation(short[4] latest, ubyte index) nothrow @safe pure {
	const(short)[] fwd = gauss[255 - index .. 512 - index];
	const(short)[] rev = gauss[index .. index + 257]; // mirror left half of gaussian

	int result;
	result = (fwd[0] * latest[0]) >> 11;
	result += (fwd[256] * latest[1]) >> 11;
	result += (rev[256] * latest[2]) >> 11;
	result = cast(short)result;
	result += (rev[0] * latest[3]) >> 11;

	if (cast(short)result != result) {
		result = (result >> 31) ^ 0x7FFF;
	}
	result &= ~1;
	return cast(short)result;
}

@safe pure unittest {
	assert(gaussianInterpolation([0, 0, 0, 0], 0) == 0);
	assert(gaussianInterpolation([0, 0, 0x100, 0x600], 0x55) == 0x6E);
	assert(gaussianInterpolation([0, 0x100, 0x600, 0x400], 0) == 0x1BA);
}

short cubicInterpolation(short[4] latest, ubyte index) nothrow @safe pure {
	const(short)[] fwd = cubic[index .. index + 258];
	const(short)[] rev = cubic[256 - index  .. 514 - index]; // mirror left half

	int result;
	result = (fwd[0] * latest[0]);
	result += (fwd[257] * latest[1]);
	result += (rev[257] * latest[2]);
	result += (rev[0] * latest[3]);
	result >>= 11;

	if (cast(short)result != result) {
		result = (result >> 31) ^ 0x7FFF;
	}
	return cast(short)result;
}

@safe pure unittest {
	assert(cubicInterpolation([0, 0, 0, 0], 0) == 0);
	assert(cubicInterpolation([10, 100, 1000, 10000], 120) == -3);
}

short sincInterpolation(short[8] latest, ushort index) nothrow @safe pure {
	const(short)[] selection = sinc[index .. index + 8];

	int result;
	result = selection[0] * latest[0];
	result += selection[1] * latest[1];
	result += selection[2] * latest[2];
	result += selection[3] * latest[3];
	result += selection[4] * latest[4];
	result += selection[5] * latest[5];
	result += selection[6] * latest[6];
	result += selection[7] * latest[7];
	result >>= 14;

	if (cast(short)result != result) {
		result = (result >> 31) ^ 0x7FFF;
	}
	return cast(short)result;
}

@safe pure unittest {
	assert(sincInterpolation([0, 0, 0, 0, 0, 0, 0, 0], 0) == 0);
	assert(sincInterpolation([1, 2, 3, 4, 5, 6, 7, 8], 9) == 3);
}
