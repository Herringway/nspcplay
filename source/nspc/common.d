module nspc.common;

enum Variant : uint {
	standard = 0,
	prototype = 1, // Early SNES games from Nintendo
}

enum Interpolation {
	gaussian,
	linear,
	cubic,
	sinc,
}

enum ReleaseTable : ubyte {
	nintendo,
	hal1,
	hal2,
	hal3,
}

enum VolumeTable : ubyte {
	nintendo,
	hal1,
	hal2,
	hal3,
}

enum GainMode {
	linearDecreaseGain,
	expDecreaseGain,
	linearIncreaseGain,
	bentIncreaseGain,
}
enum ADSRGainMode {
	adsr,
	customGain,
	directGain,
}

align(1) struct ADSRGain {
	align(1):
	ushort adsr;
	ubyte gain;
	void toString(S)(ref S sink) const {
		import std.format: formattedWrite;
		try {
			sink.formattedWrite!"%s"(mode);
			final switch (mode) {
				case ADSRGainMode.adsr:
					sink.formattedWrite!" (%s, %s, %s, %s)"(attackRate, decayRate, sustainRate, sustainLevel);
					break;
				case ADSRGainMode.customGain:
					sink.formattedWrite!" (%s, %s)"(gainMode, gainRate);
					break;
				case ADSRGainMode.directGain:
					sink.formattedWrite!" (%s)"(fixedVolume);
					break;
			}
		} catch (Exception) {}
	}
	const @safe pure nothrow:
	ubyte attackRate() {
		return adsr & 0x0F;
	}
	ubyte decayRate() {
		return (adsr & 0x70) >> 4;
	}
	bool adsrGainSelect() {
		return !!(adsr & 0x80);
	}
	ubyte sustainRate() {
		return (adsr & 0x1F00) >> 8;
	}
	short sustainLevel() {
		return (((adsr & 0xE000) >> 13) + 1) << 8;
	}
	ADSRGainMode mode() {
		if (adsrGainSelect) {
			return ADSRGainMode.adsr;
		} else {
			if (!!(gain & 0x80)) {
				return ADSRGainMode.customGain;
			}
			return ADSRGainMode.directGain;
		}
	}
	GainMode gainMode() {
		switch ((gain & 0x7F) >> 5) {
			case 0:
				return GainMode.linearDecreaseGain;
			case 1:
				return GainMode.expDecreaseGain;
			case 2:
				return GainMode.linearIncreaseGain;
			case 3:
				return GainMode.bentIncreaseGain;
			default: assert(0);
		}
	}
	ubyte gainRate() {
		return gain & 0xF;
	}
	ubyte fixedVolume() {
		return gain & 0x3F;
	}
}
struct Sample {
	short[] data;
	int loopLength;
	bool isValid() const @safe pure nothrow {
		return data.length > 0;
	}
}
struct Instrument {
	align(1):
	ubyte sampleID;
	ADSRGain adsrGain;
	private ubyte _tuning;
	private ubyte _tuningFraction;
	ushort tuning() const pure @safe nothrow {
		return (_tuning << 8) + _tuningFraction;
	}
	void toString(S)(ref S sink) const {
		import std.format: formattedWrite;
		try {
			sink.formattedWrite!"Sample: %s, ADSR/gain: %s, tuning: %s"(sampleID, adsrGain, tuning);
		} catch (Exception) {}
	}
}

class NSPCException : Exception {
	this(string msg, string file = __FILE__, size_t line = __LINE__) @safe {
		super(msg, file, line);
	}
}

package T read(T)(const(ubyte)[] data, size_t offset = 0) {
	return (cast(const(T)[])(data[offset .. offset + T.sizeof]))[0];
}
