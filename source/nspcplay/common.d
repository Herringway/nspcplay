module nspcplay.common;

enum Variant : uint {
	standard = 0,
	prototype = 1, // Early SNES games from Nintendo
	konami = 2, // Konami's NSPC variant - NYI
	addmusick = 3, /// AddMusicK, a community-maintained extension to the prototype NSPC engine
	fzero = 4, /// An almost-final version of standard N-SPC. Several commands are missing, master volume is 25% lower
}

enum Interpolation {
	gaussian,
	none,
	linear,
	cubic,
	sinc,
}

enum ReleaseTable : ubyte {
	nintendo1,
	hal1,
	hal2,
	hal3,
	nintendo2,
	nintendoProto,
}

enum VolumeTable : ubyte {
	nintendo1,
	hal1,
	hal2,
	hal3,
	nintendo2,
	nintendoProto,
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
	ushort start; /// starting address
	short[] data; /// decoded signed 16-bit PCM data
	int loopLength; /// Number of samples in loop
	bool isValid() const @safe pure nothrow {
		return data.length > 0;
	}
	ubyte[16] hash() const @safe pure nothrow {
		import std.digest.md : md5Of;
		return md5Of(data);
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
	void tuning(ushort val) pure @safe nothrow {
		_tuning = (val & 0xFF00) >> 8;
		_tuningFraction = val & 0xFF;
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

ADSRGain konamiADSRGain(const ubyte[] parameters) nothrow pure @safe {
	ADSRGain tmp;
	if (parameters[0] < 160) {
		tmp.adsr = ((parameters[0] % 10) & 7) | (parameters[0] / 10) | 0x80;
	}
	tmp.adsr |= (((parameters[1] % 30) + 2) | ((parameters[1] / 30) << 5)) << 8;
	tmp.gain = parameters[2];
	return tmp;
}

ADSRGain amkADSRGain(const ubyte[] parameters) nothrow pure @safe {
	ADSRGain tmp;
	if (parameters[0] == 0x80) {
		tmp.gain = parameters[1];
	} else {
		tmp.adsr = (parameters[1] << 8) | parameters[0] | 0x80;
	}
	return tmp;
}
