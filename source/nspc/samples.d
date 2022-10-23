module nspc.samples;

private enum brrBlockSize = 9;
private enum brrFlagEnd = 1;
private enum brrFlagLoop = 2;

struct Sample {
	short[] data;
	int loopLength;
	bool isValid() const @safe pure nothrow {
		return data.length > 0;
	}
}

Sample decodeSample(scope const ubyte[] buffer, ushort start, ushort loop) @safe {
	Sample sample;
	//for (uint sn = 0; sn < 128; sn++) {
	//	const start = ptrtable[sn][0];
	//	const loop = ptrtable[sn][1];

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
	int times = 0;

	short[2] pExtra;
	size_t idx;
	do {
		needsAnotherLoop = false;
		for (int pos = decodingStart; pos < end; pos += brrBlockSize) {
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

		// In the vanilla game, the most iterations needed is 48 (for sample 0x17 in pack 5).
		// Most samples need less than 10.
		++times;
	} while (needsAnotherLoop && times < 128);

	assert(!needsAnotherLoop, "Sample took too many iterations to get into a cycle");
	return sample;
	//}
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

		s *= 2;

		if (cast(short) s != s) {
			s = (s >> 31) ^ 0x7FFF;
		}

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
