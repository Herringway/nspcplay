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

void decodeBRRBlock(scope short[] buffer, short[2] lastSamples, const scope ubyte[] block) nothrow @safe {
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
			case 1:
				s += (cast(int) lastSamples[1] * 15) >> 5;
				break;
			case 2:
				s += ((cast(int) lastSamples[1] * 61) >> 6) - ((cast(int) lastSamples[0] * 15) >> 5);
				break;
			case 3:
				s += ((cast(int) lastSamples[1] * 115) >> 7) - ((cast(int) lastSamples[0] * 13) >> 5);
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
