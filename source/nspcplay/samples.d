module nspcplay.samples;

import nspcplay.common;
import nspcplay.interpolationtables;
import std.exception;

private enum brrBlockSize = 9;
private enum brrFlagEnd = 1;
private enum brrFlagLoop = 2;

Sample decodeSample(scope const ubyte[] buffer, ushort start, ushort loop) @safe {
	Sample sample;
	sample.start = start;
	int length = sampleLength(buffer, start);
	if (length == -1) {
		throw new Exception("Invalid length");
	}

	int end = start + length;
	// The LOOP bit only matters for the last brr block
	if (buffer[start + length - brrBlockSize] & brrFlagLoop) {
		if (loop < start || loop >= end) {
			throw new Exception("Invalid loop");
		}
		sample.loopLength = ((end - loop) / brrBlockSize) * 16;
	} else {
		sample.loopLength = 0;
	}

	sample.data = new short[]((length / brrBlockSize) * 16);
	short[] p = sample.data;

	bool needsAnotherLoop;
	int decodingStart = start;

	short[2] pExtra;
	size_t idx;
	do {
		needsAnotherLoop = false;
		for (int pos = decodingStart; pos < end; pos += brrBlockSize) {
			enforce!NSPCException((idx + 1) * 16 <= p.length, "Invalid sample");
			decodeBRRBlock(p[idx * 16 .. (idx + 1) * 16], pExtra, buffer[pos .. pos + brrBlockSize]);
			pExtra[] = p[idx * 16 + 14 .. idx * 16 + 16];
			idx++;
		}

		if (sample.loopLength != 0) {
			decodingStart = loop;

			short[16] afterLoop;

			decodeBRRBlock(afterLoop, pExtra, buffer[loop .. loop + brrBlockSize]);
			int fullLoopLength = getFullLoopLength(sample, afterLoop[0 .. 2], (loop - start) / brrBlockSize * 16);

			if (fullLoopLength == -1) {
				needsAnotherLoop = true;
				const diff = sample.data.length;
				idx = 0;
				sample.data.length += sample.loopLength;
				p = sample.data[diff .. $];
			} else {
				sample.loopLength = fullLoopLength;
				// needsAnotherLoop is already false
			}
		}
	} while (needsAnotherLoop);
	return sample;
}

void decodeBRRBlock(scope short[] buffer, short[2] lastSamples, const scope ubyte[] block) nothrow @safe pure {
	int range = block[0] >> 4;
	int filter = (block[0] >> 2) & 3;

	for (int i = 0; i < 16; i++) {
		int s = block[i / 2 + 1];

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
		switch (filter) {
			case 1: // + lastSamples[1] * 0.46875
				s += (lastSamples[1] >> 1) + ((-lastSamples[1]) >> 5);
				break;
			case 2: // + lastSamples[1] * 0.953125 - lastSamples[0] * 0.46875
				s += lastSamples[1] - (lastSamples[0] >> 1) + (lastSamples[0] >> 5) + ((lastSamples[1] * -3) >> 6);
				break;
			case 3:// + lastSamples[1] * 0.8984375 - lastSamples[0] * 0.40625
				s += lastSamples[1] - (lastSamples[0] >> 1) + ((lastSamples[1] * -13) >> 7) + (((lastSamples[0] >> 1) * 3) >> 4);
				break;
			default:
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
	void assertBlock(scope const ubyte[] input, short[16] output, short[2] prev = [0, 0], string file = __FILE__, size_t line = __LINE__) {
		short[16] buffer;
		decodeBRRBlock(buffer, prev, input);
		if(buffer != output) {
			debug writefln!"%(%04X %)"(buffer[]);
			debug writefln!"%(%04X %)"(output[]);
			throw new AssertError("Test failed", file, line);
		}
	}
	// filter 0
	assertBlock([0xC2, 0x2C, 0x1E, 0xB3, 0x40, 0xEF, 0x10, 0x41, 0xE0], [0x2000, -0x4000, 0x1000, -0x2000, -0x5000, 0x3000, 0x4000, 0x0000, -0x2000, -0x1000, 0x1000, 0x0000, 0x4000, 0x1000, -0x2000, 0x0000]);
	// filter 1
	assertBlock([0xB6, 0x32, 0x11, 0x10, 0xDB, 0xDC, 0xC0, 0x11, 0x35], [0x19C6, 0x2828, 0x2DA4, 0x32C8, 0x379A, 0x3420, 0x18DE, cast(short)0xEF50, cast(short)0xD85A, cast(short)0xBAD4, cast(short)0x9F26, cast(short)0xA532, cast(short)0xB2DE, cast(short)0xBFB0, cast(short)0xDBB4, 0x05F8], [0x00EA, 0x01E6]);
	// filter 2
	assertBlock([0xAA, 0x4D, 0xA0, 0x10, 0x00, 0x00, 0x01, 0xF0, 0x01], [0x10AC, 0x131E, -0x332, cast(short)0xE7FA, cast(short)0xD932, cast(short)0xCC8A, cast(short)0xC246, cast(short)0xBA92, cast(short)0xB584, cast(short)0xB318, cast(short)0xB338, cast(short)0xB9BA, cast(short)0xBE04, cast(short)0xC416, cast(short)0xCBA4, cast(short)0xD85A], [0x00B0, 0x00B2]);
	// filter 3
	assertBlock([0x9E, 0x2B, 0xC3, 0xF0, 0x48, 0x43, 0xA6, 0x2F, 0x03], [0x0400, cast(short)0xFD30, cast(short)0xEFB2, cast(short)0xEAFA, cast(short)0xE576, cast(short)0xE164, cast(short)0xE68E, cast(short)0xDB24, cast(short)0xDA70, cast(short)0xE072, cast(short)0xD9D0, cast(short)0xE102, cast(short)0xEB54, cast(short)0xF208, cast(short)0xF7B0, 0x0268]);
}

private int sampleLength(const scope ubyte[] spcMemory, ushort start) nothrow @safe {
	int end = start;
	ubyte b;
	do {
		b = spcMemory[end];
		end += brrBlockSize;
	} while ((b & brrFlagEnd) == 0 && end < 0x10000 - brrBlockSize);

	if (end < 0x10000 - brrBlockSize) {
		return end - start;
	} else {
		return -1;
	}
}

private int getFullLoopLength(const Sample sa, const short[2] nextBlock, int firstLoopStart) nothrow @safe {
	auto loopStart = cast(int)(sa.data.length - sa.loopLength);
	bool noMatchFound = true;
	while (loopStart >= firstLoopStart && noMatchFound) {
		// If the first two samples in a loop are the same, the rest all will be too.
		// BRR filters can rely on, at most, two previous samples.
		if (sa.data[loopStart] == nextBlock[0] && sa.data[loopStart + 1] == nextBlock[1]) {
			noMatchFound = false;
		} else {
			loopStart -= sa.loopLength;
		}
	}

	if (loopStart >= firstLoopStart) {
		return cast(int)(sa.data.length - loopStart);
	} else {
		return -1;
	}
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
