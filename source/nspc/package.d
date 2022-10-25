module nspc;

import core.stdc.math;
import core.stdc.stdlib;
import core.stdc.string;
import core.memory;
import std.algorithm.comparison;
import std.algorithm.searching;
import std.algorithm.sorting;
import std.exception;
import std.experimental.logger;
import std.format;

public import nspc.common;
public import nspc.samples;
public import nspc.sequence;

private struct SongState {
	ChannelState[8] chan;
	byte transpose;
	Slider volume = Slider(0xC000);
	Slider tempo = Slider(0x2000);
	int nextTimerTick;
	int cycleTimer = 255;
	ubyte firstCAInst; // set with FA
	ubyte repeatCount;
	int ordnum = -1;
	int patpos; // Number of cycles since top of pattern
}

private struct Slider {
	ushort cur;
	ushort delta;
	ubyte cycles;
	ubyte target;

	private void slide() nothrow @safe pure {
		if (cycles) {
			if (--cycles == 0) {
				cur = target << 8;
			} else {
				cur += delta;
			}
		}
	}
}

enum ADSRPhase {
	attack,
	decay,
	sustain,
	gain,
	release
}

private struct ChannelState {
	int next; // time left in note

	Slider note;
	ubyte curPortStartCtr;
	ubyte noteLen;
	ubyte noteStyle;

	ubyte noteRelease; // time to release note, in cycles

	Parser parser;

	ubyte inst; // instrument
	ADSRGain instADSRGain;
	ubyte finetune;
	byte transpose;
	Slider panning = Slider(0x0A00);
	ubyte panFlags;
	Slider volume = Slider(0xFF00);
	ubyte totalVol;
	byte leftVol;
	byte rightVol;

	ubyte portType;
	ubyte portStart;
	ubyte portLength;
	ubyte portRange;
	ubyte vibratoStart;
	ubyte vibratoSpeed;
	ubyte vibratoMaxRange;
	ubyte vibratoFadeIn;
	ubyte tremoloStart;
	ubyte tremoloSpeed;
	ubyte tremoloRange;

	ubyte vibratoPhase;
	ubyte vibratoStartCtr;
	ubyte curVibRange;
	ubyte vibratoFadeInCtr;
	ubyte vibratoRangeDelta;
	ubyte tremoloPhase;
	ubyte tremoloStartCtr;

	Sample samp;
	int sampPos = -1;
	int noteFrequency;

	short gain;
	ADSRPhase adsrPhase;
	ushort adsrCounter;
	ubyte adsrRate;
	void setADSRPhase(ADSRPhase phase) @safe pure nothrow {
		adsrCounter = 0;
		adsrPhase = phase;
		final switch (phase) {
			case ADSRPhase.attack:
				adsrRate = cast(ubyte)(instADSRGain.attackRate * 2 + 1);
				break;
			case ADSRPhase.decay:
				adsrRate = cast(ubyte)(instADSRGain.decayRate * 2 + 16);
				break;
			case ADSRPhase.sustain:
				adsrRate = instADSRGain.sustainRate;
				break;
			case ADSRPhase.gain:
				if (instADSRGain.mode == ADSRGainMode.customGain) {
					adsrRate = instADSRGain.gainRate;
				}
				assert(instADSRGain.mode != ADSRGainMode.adsr);
				break;
			case ADSRPhase.release:
				adsrRate = 31;
				break;
		}
	}
}

private struct Parser {
	const(ubyte)[] ptr;
	const(ubyte)[] subRet;
	Track[ushort] subRoutines;
	ushort subStart;
	ubyte subCount;
	Variant variant;
	Command popCommand() nothrow @safe {
		bool _;
		return popCommand(_);
	}
	Command popCommand(out bool done) nothrow @safe {
		size_t nextOffset;
		const command = readCommand(variant, ptr, nextOffset);
		if (command.type == VCMDClass.terminator) {
			done = subCount == 0;
			if (!done) {
				ptr = --subCount ? subRoutines[subStart].track : subRet;
			}
		} else if ((command.type == VCMDClass.special) && (command.special == VCMD.subRoutine)) {
			subRet = ptr[nextOffset .. $];
			subStart = read!ushort(command.parameters);
			subCount = command.parameters[2];
			ptr = subRoutines[subStart].track;
		} else {
			ptr = ptr[nextOffset .. $];
		}
		return command;
	}
}

private struct Song {
	ushort address;
	ubyte changed;
	Phrase[] order;
	Track[8][] pattern;
	Track[ushort] subRoutines;
}

private struct Track {
	int size;
	const(ubyte)[] track; // null for inactive track
}

// Rates (in samples) until next step is applied
private immutable adsrGainRates = [ 0, 2048, 1536, 1280, 1024, 768, 640, 512, 384, 320, 256, 192, 160, 128, 96, 80, 64, 48, 40, 32, 24, 20, 16, 12, 10, 8, 6, 5, 4, 3, 2, 1 ];

// note style tables, from 6F80
private immutable ubyte[8][] releaseTables = [
	[0x32, 0x65, 0x7F, 0x98, 0xB2, 0xCB, 0xE5, 0xFC],
	[0x33, 0x66, 0x7F, 0x99, 0xB2, 0xCC, 0xE5, 0xFC], // HAL (Earthbound)
	[0x65, 0x7F, 0x98, 0xB2, 0xCB, 0xE5, 0xF2, 0xFC], // HAL (Kirby Super Star)
	[0x66, 0x7F, 0x99, 0xB2, 0xCC, 0xE5, 0xF2, 0xFC], // HAL (Kirby's Dream Course)
];
private immutable ubyte[16][] volumeTables = [
	[0x19, 0x32, 0x4C, 0x65, 0x72, 0x7F, 0x8C, 0x98, 0xA5, 0xB2, 0xBF, 0xCB, 0xD8, 0xE5, 0xF2, 0xFC],
	[0x19, 0x33, 0x4C, 0x66, 0x72, 0x7F, 0x8C, 0x99, 0xA5, 0xB2, 0xBF, 0xCC, 0xD8, 0xE5, 0xF2, 0xFC], // HAL (Earthbound)
	[0x4C, 0x59, 0x6D, 0x7F, 0x87, 0x8E, 0x98, 0xA0, 0xA8, 0xB2, 0xBF, 0xCB, 0xD8, 0xE5, 0xF2, 0xFC], // HAL (Kirby Super Star)
	[0x59, 0x66, 0x72, 0x7F, 0x87, 0x8E, 0x99, 0xA0, 0xA8, 0xB2, 0xBF, 0xCC, 0xD8, 0xE5, 0xF2, 0xFC], // HAL (Kirby's Dream Course)
];

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

private struct Instrument {
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

private struct PrototypeInstrument {
	align(1):
	ubyte sampleID;
	ADSRGain adsrGain;
	ubyte tuning;
	Instrument opCast(T: Instrument)() const {
		return Instrument(sampleID, adsrGain, tuning, 0);
	}
}
private struct PrototypePercussion {
	align(1):
	ubyte sampleID;
	ADSRGain adsrGain;
	ubyte tuning;
	ubyte note;
	Instrument opCast(T: Instrument)() const {
		return Instrument(sampleID, adsrGain, tuning, 0);
	}
}

enum PhraseType {
	pattern,
	jumpLimited,
	jump,
	end,
	fastForwardOn,
	fastForwardOff,
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

align(1) private struct ADSRGain {
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

private struct Phrase {
	PhraseType type;
	ushort id;
	ushort jumpTimes;
	bool unallocated;
	void toString(S)(ref S sink) const {
		final switch (type) {
			case PhraseType.pattern:
				sink.formattedWrite!"Play pattern %s"(id);
				break;
			case PhraseType.jumpLimited:
				sink.formattedWrite!"Jump to phrase %s %s times"(id, jumpTimes);
				break;
			case PhraseType.jump:
				sink.formattedWrite!"Jump to phrase %s"(id);
				break;
			case PhraseType.end:
				sink.formattedWrite!"End of song"();
				break;
			case PhraseType.fastForwardOn:
				sink.formattedWrite!"Fast forward on"();
				break;
			case PhraseType.fastForwardOff:
				sink.formattedWrite!"Fast forward off"();
				break;
		}
	}
}

///
struct NSPCFileHeader {
	align(1):
	static union Extra {
		struct { //Variant.prototype
			ushort percussionBase;
		}
		ubyte[20] reserved;
	}
	/// Which version of NSPC to use
	Variant variant;
	/// Base SPC address of the song's sequence data
	ushort songBase;
	/// Base SPC address of the instruments
	ushort instrumentBase;
	/// Base SPC address of the samples
	ushort sampleBase;
	/// Release table to use
	ReleaseTable releaseTable;
	/// Volume table to use
	VolumeTable volumeTable;
	/// Extra information for variants
	Extra extra;
}

void doADSR(ref ChannelState chan) nothrow @safe {
	ushort level() {
		return cast(ushort)(((chan.gain - 1) >> 8) + 1);
	}
	final switch (chan.adsrPhase) {
		case ADSRPhase.attack:
			chan.gain += (chan.adsrRate == 31) ? 1024 : 32;
			if (chan.gain > 0x7E0) {
				chan.setADSRPhase(ADSRPhase.decay);
			}
			break;
		case ADSRPhase.decay:
			chan.gain -= level;
			if (chan.gain < chan.instADSRGain.sustainLevel) {
				chan.setADSRPhase(ADSRPhase.sustain);
			}
			break;
		case ADSRPhase.sustain:
			chan.gain -= level;
			break;
		case ADSRPhase.gain:
			final switch(chan.instADSRGain.gainMode) {
				case GainMode.linearDecreaseGain:
					chan.gain -= 32;
					break;
				case GainMode.expDecreaseGain:
					chan.gain -= level;
					break;
				case GainMode.linearIncreaseGain:
					chan.gain += 32;
					break;
				case GainMode.bentIncreaseGain:
					chan.gain +=  (chan.gain < 0x600) ? 32 : 8;
					break;
			}
			break;
		case ADSRPhase.release:
			chan.gain -= 8;
			if (chan.gain < 0) {
				chan.sampPos = -1;
			}
			break;
	}
	chan.gain = clamp(chan.gain, cast(short)0, cast(short)0x7FF);
}

///
struct NSPCPlayer {
	enum defaultSpeed = 500;

	private Song currentSong;
	private SongState state;
	private int mixrate = 44100;
	private ubyte chmask = 0b11111111;
	private int timerSpeed = defaultSpeed;
	private bool songPlaying;

	private Sample[128] samp;

	private Instrument[] instruments;

	private bool loopEnabled = true;

	private enum maxInstruments = 64;
	private enum maxSampleCount = 128;
	private size_t volumeTable;
	private size_t releaseTable;
	private Variant variant;
	private ubyte[256] percussionNotes;
	private size_t percussionBase;
	///
	short[2][] fillBuffer(short[2][] buffer) nothrow @safe {
		if (!songPlaying) {
			return [];
		}
		size_t length;
		foreach (ref sample; buffer) {
			if ((state.nextTimerTick -= timerSpeed) < 0) {
				state.nextTimerTick += mixrate;
				if (!doTimer()) {
					break;
				}
			}
			length++;
			int left = 0, right = 0;
			foreach (i, ref chan; state.chan) {
				if (!(chmask & (1 << i))) {
					continue;
				}

				if (chan.sampPos < 0) {
					continue;
				}

				int ipos = chan.sampPos >> 15;

				if (ipos >= chan.samp.data.length) {
					assert(0, format!"Sample position exceeds sample length! %d > %d"(ipos, chan.samp.data.length));
				}

				if (chan.adsrRate && (++chan.adsrCounter >= cast(int)(adsrGainRates[chan.adsrRate] * (mixrate / 44100.0)))) {
					doADSR(chan);
					chan.adsrCounter = 0;
				}
				if (chan.gain == 0) {
					continue;
				}
				assert(chan.samp.data);
				int s1 = chan.samp.data[ipos];
				int s2 = (ipos + 1 == chan.samp.data.length) ? s1 : chan.samp.data[ipos + 1];
				s1 += (s2 - s1) * (chan.sampPos & 0x7FFF) >> 15;
				s1 = (s1 * chan.gain) >> 11;

				left += cast(int)(s1 * chan.leftVol / 128.0);
				right += cast(int)(s1 * chan.rightVol / 128.0);

				chan.sampPos += chan.noteFrequency;
				if ((chan.sampPos >> 15) >= chan.samp.data.length) {
					if (chan.samp.loopLength) {
						chan.sampPos -= chan.samp.loopLength << 15;
					} else {
						chan.sampPos = -1;
						chan.adsrPhase = ADSRPhase.release;
					}
				}
			}
			left = clamp(left, short.min, short.max);
			right = clamp(right, short.min, short.max);
			sample[0] = cast(short) left;
			sample[1] = cast(short) right;
		}
		return buffer[0 .. length];
	}

	private void calcTotalVolume(const SongState st, ref ChannelState c, byte tremoloPhase) nothrow @safe {
		ubyte v = (tremoloPhase << 1 ^ tremoloPhase >> 7) & 0xFF;
		v = ~(v * c.tremoloRange >> 8) & 0xFF;

		v = v * (st.volume.cur >> 8) >> 8;
		v = v * volumeTables[volumeTable][c.noteStyle & 15] >> 8;
		v = v * (c.volume.cur >> 8) >> 8;
		c.totalVol = v * v >> 8;
	}

	private int calcVolume3(const ChannelState c, int pan, int flag) nothrow @safe {
		static immutable ubyte[] panTable = [0x00, 0x01, 0x03, 0x07, 0x0D, 0x15, 0x1E, 0x29, 0x34, 0x42, 0x51, 0x5E, 0x67, 0x6E, 0x73, 0x77, 0x7A, 0x7C, 0x7D, 0x7E, 0x7F, 0x7F];
		const ubyte[] ph = panTable[pan >> 8 .. (pan >> 8) + 2];
		int v = ph[0] + ((ph[1] - ph[0]) * (pan & 255) >> 8);
		v = v * c.totalVol >> 8;
		if (c.panFlags & flag) {
			v = -v;
		}
		return v;
	}

	private void calcVolume2(ref ChannelState c, int pan) nothrow @safe {
		c.leftVol = cast(byte) calcVolume3(c, pan, 0x80);
		c.rightVol = cast(byte) calcVolume3(c, 0x1400 - pan, 0x40);
	}

	private void makeSlider(ref Slider s, int cycles, int target) nothrow @safe {
		s.delta = cast(ushort)(((target << 8) - (s.cur & 0xFF00)) / cycles);
		s.cycles = cast(ubyte) cycles;
		s.target = cast(ubyte) target;
	}

	private void setInstrument(ref SongState st, ref ChannelState c, size_t inst) nothrow @safe {
		if (inst >= instruments.length) {
			assert(0, format!"instrument %s out of bounds!"(inst));
		}
		const idata = instruments[inst];
		if (!samp[idata.sampleID].data) {
			assert(0, format!"no data for instrument %s"(inst));
		}

		c.inst = cast(ubyte) inst;
		c.instADSRGain = idata.adsrGain;
		if (idata.adsrGain.mode == ADSRGainMode.directGain) {
			c.gain = idata.adsrGain.fixedVolume;
		}
	}

	// calculate how far to advance the sample pointer on each output sample
	private void setFrequency(ref ChannelState c, int note16) const nothrow @safe {
		static immutable ushort[13] noteFrequencyTable = [0x085F, 0x08DF, 0x0965, 0x09F4, 0x0A8C, 0x0B2C, 0x0BD6, 0x0C8B, 0x0D4A, 0x0E14, 0x0EEA, 0x0FCD, 0x10BE];

		// What is this for???
		if (note16 >= 0x3400) {
			note16 += (note16 >> 8) - 0x34;
		} else if (note16 < 0x1300) {
			note16 += ((note16 >> 8) - 0x13) << 1;
		}

		if (cast(ushort) note16 >= 0x5400) {
			c.noteFrequency = 0;
			return;
		}

		int octave = (note16 >> 8) / 12;
		int tone = (note16 >> 8) % 12;
		int freq = noteFrequencyTable[tone];
		freq += (noteFrequencyTable[tone + 1] - freq) * (note16 & 0xFF) >> 8;
		freq <<= 1;
		freq >>= 6 - octave;


		freq *= instruments[c.inst].tuning;
		freq >>= 8;
		freq &= 0x3fff;

		c.noteFrequency = (freq * (32000U << (15 - 12))) / mixrate;
	}

	private int calcVibratoDisp(ref ChannelState c, int phase) nothrow @safe {
		int range = c.curVibRange;
		if (range > 0xF0) {
			range = (range - 0xF0) * 256;
		}

		int disp = (phase << 2) & 255; /* //// */
		if (phase & 0x40) {
			disp ^= 0xFF; /* /\/\ */
		}
		disp = (disp * range) >> 8;

		if (phase & 0x80) {
			disp = -disp; /* /\   */
		}
		return disp; /*   \/ */
	}
	// do a Ex/Fx code
	private void doCommand(ref SongState st, ref ChannelState c, const Command command) nothrow @safe {
		final switch (command.special) {
			case VCMD.instrument:
				setInstrument(st, c, absoluteInstrumentID(command.parameters[0], st.firstCAInst, false));
				break;
			case VCMD.panning:
				c.panFlags = command.parameters[0];
				c.panning.cur = (command.parameters[0] & 0x1F) << 8;
				break;
			case VCMD.panningFade:
				makeSlider(c.panning, command.parameters[0], command.parameters[1]);
				break;
			case VCMD.vibratoOn:
				c.vibratoStart = command.parameters[0];
				c.vibratoSpeed = command.parameters[1];
				c.curVibRange = c.vibratoMaxRange = command.parameters[2];
				c.vibratoFadeIn = 0;
				break;
			case VCMD.vibratoOff:
				c.curVibRange = c.vibratoMaxRange = 0;
				c.vibratoFadeIn = 0;
				break;
			case VCMD.songVolume:
				st.volume.cur = command.parameters[0] << 8;
				break;
			case VCMD.songVolumeFade:
				makeSlider(st.volume, command.parameters[0], command.parameters[1]);
				break;
			case VCMD.tempo:
				st.tempo.cur = command.parameters[0] << 8;
				break;
			case VCMD.tempoFade:
				makeSlider(st.tempo, command.parameters[0], command.parameters[1]);
				break;
			case VCMD.globalAbsoluteTransposition:
				st.transpose = command.parameters[0];
				break;
			case VCMD.channelAbsoluteTransposition:
				c.transpose = command.parameters[0];
				break;
			case VCMD.tremoloOn:
				c.tremoloStart = command.parameters[0];
				c.tremoloSpeed = command.parameters[1];
				c.tremoloRange = command.parameters[2];
				break;
			case VCMD.tremoloOff:
				c.tremoloRange = 0;
				break;
			case VCMD.volume:
				c.volume.cur = command.parameters[0] << 8;
				break;
			case VCMD.volumeFade:
				makeSlider(c.volume, command.parameters[0], command.parameters[1]);
				break;
			case VCMD.subRoutine:
				//c.parser.subStart = command.parameters[0] | (command.parameters[1] << 8);
				//c.parser.subRet = c.parser.ptr;
				//c.parser.subCount = command.parameters[2];
				//c.parser.ptr = currentSong.subRoutines[c.parser.subStart].track;
				break;
			case VCMD.vibratoFadeIn:
				c.vibratoFadeIn = command.parameters[0];
				c.vibratoRangeDelta = c.curVibRange / command.parameters[0];
				break;
			case VCMD.notePitchEnvelopeTo:
			case VCMD.notePitchEnvelopeFrom:
				c.portType = (command.special == VCMD.notePitchEnvelopeTo);
				c.portStart = command.parameters[0];
				c.portLength = command.parameters[1];
				c.portRange = command.parameters[2];
				break;
			case VCMD.notePitchEnvelopeOff:
				c.portLength = 0;
				break;
			case VCMD.fineTune:
				c.finetune = command.parameters[0];
				break;
			case VCMD.echoEnableBitsAndVolume:
			case VCMD.echoOff:
			case VCMD.echoParameterSetup:
			case VCMD.echoVolumeFade:
				debug(nspclogging) warningf("Unhandled command: %s", command);
				break;
			case VCMD.noop: //do nothing
				break;
			case VCMD.pitchSlideToNote:
				c.curPortStartCtr = command.parameters[0];
				int target = command.parameters[2] + st.transpose;
				if (target >= 0x100) {
					target -= 0xFF;
				}
				target += c.transpose;
				makeSlider(c.note, command.parameters[1], target & 0x7F);
				break;
			case VCMD.percussionBaseInstrumentRedefine:
				st.firstCAInst = command.parameters[0];
				break;
			case VCMD.channelMute:
			case VCMD.fastForwardOn:
			case VCMD.fastForwardOff:
				debug(nspclogging) warningf("Unhandled command: %s", command);
				break;
			case VCMD.invalid: //do nothing
				assumeWontThrow(errorf("Invalid command"));
				break;
		}
	}

	// $0654 + $08D4-$8EF
	private void doNote(ref SongState st, ref ChannelState c, const Command command) nothrow @safe {
		ubyte note = command.note;
		// using >=CA as a note switches to that instrument and plays a predefined note
		if (command.type == VCMDClass.percussion) {
			setInstrument(st, c, absoluteInstrumentID(note, st.firstCAInst, true));
			note = cast(ubyte)(percussionNotes[note]);
		}
		if (command.type.among(VCMDClass.percussion, VCMDClass.note)) {
			c.vibratoPhase = c.vibratoFadeIn & 1 ? 0x80 : 0;
			c.vibratoStartCtr = 0;
			c.vibratoFadeInCtr = 0;
			c.tremoloPhase = 0;
			c.tremoloStartCtr = 0;

			c.sampPos = 0;
			c.samp = samp[instruments[c.inst].sampleID];
			c.gain = 0;
			c.setADSRPhase((instruments[c.inst].adsrGain.mode == ADSRGainMode.adsr) ? ADSRPhase.attack : ADSRPhase.gain);

			note += st.transpose + c.transpose;
			c.note.cur = cast(ushort)(note << 8 | c.finetune);

			c.note.cycles = c.portLength;
			if (c.note.cycles) {
				int target = note;
				c.curPortStartCtr = c.portStart;
				if (c.portType == 0) {
					c.note.cur -= c.portRange << 8;
				} else {
					target += c.portRange;
				}
				makeSlider(c.note, c.portLength, target & 0x7F);
			}

			setFrequency(c, c.note.cur);
		}

		// Search forward for the next note (to see if it's C8). This is annoying
		// but necessary - C8 can continue the last note of a subroutine as well
		// as a normal note.
		VCMDClass nextNote;
		{
			auto p = c.parser;
			bool done;
			while (!done) {
				const tmpCommand = p.popCommand(done);
				if (tmpCommand.type.among(VCMDClass.note, VCMDClass.tie, VCMDClass.rest, VCMDClass.percussion)) {
					nextNote = tmpCommand.type;
					break;
				}
			}
		}

		int rel;
		if (nextNote == VCMDClass.tie) {
			// if the note will be continued, don't release yet
			rel = c.noteLen;
		} else {
			rel = (c.noteLen * releaseTables[releaseTable][c.noteStyle >> 4]) >> 8;
			if (rel > c.noteLen - 2) {
				rel = c.noteLen - 2;
			}
			if (rel < 1) {
				rel = 1;
			}
		}
		c.noteRelease = cast(ubyte) rel;
	}

	private void loadPattern() nothrow @safe {
		state.ordnum++;
		const nextPhrase = currentSong.order[state.ordnum];
		debug(nspclogging) tracef("Next phrase: %s", nextPhrase);
		final switch (nextPhrase.type) {
			case PhraseType.end:
				state.ordnum--;
				songPlaying = false;
				return;
			case PhraseType.jumpLimited:
				if (--state.repeatCount >= 0x80) {
					state.repeatCount = cast(ubyte)nextPhrase.jumpTimes;
				}
				if (state.repeatCount > 0) {
					state.ordnum = nextPhrase.id - 1;
				}
				debug(nspclogging) tracef("%s more repeats", state.repeatCount);
				loadPattern();
				break;
			case PhraseType.jump:
				if (loopEnabled) {
					state.ordnum = nextPhrase.id - 1;
					loadPattern();
				} else {
					state.ordnum--;
					songPlaying = false;
				}
				break;
			case PhraseType.fastForwardOn:
				assert(0, "Not yet implemented");
			case PhraseType.fastForwardOff:
				assert(0, "Not yet implemented");
			case PhraseType.pattern:
				foreach (idx, ref channel; state.chan) {
					channel.parser.ptr = currentSong.pattern[nextPhrase.id][idx].track;
					channel.parser.subCount = 0;
					channel.volume.cycles = 0;
					channel.panning.cycles = 0;
					channel.next = 0;
				}
				state.patpos = 0;
				break;
		}
	}

	private void CF7(ref ChannelState c) nothrow @safe {
		if (c.noteRelease) {
			c.noteRelease--;
		}
		if (!c.noteRelease) {
			c.setADSRPhase(ADSRPhase.release);
		}

		// 0D60
		if (c.note.cycles) {
			if (c.curPortStartCtr == 0) {
				c.note.slide();
				setFrequency(c, c.note.cur);
			} else {
				c.curPortStartCtr--;
			}
		}

		// 0D79
		if (c.curVibRange) {
			if (c.vibratoStartCtr == c.vibratoStart) {
				int range;
				if (c.vibratoFadeInCtr == c.vibratoFadeIn) {
					range = c.vibratoMaxRange;
				} else {
					range = c.curVibRange;
					if (c.vibratoFadeInCtr == 0) {
						range = 0;
					}
					range += c.vibratoRangeDelta;
					c.vibratoFadeInCtr++;
				} // DA0
				c.curVibRange = cast(ubyte) range;
				c.vibratoPhase += c.vibratoSpeed;
				setFrequency(c, c.note.cur + calcVibratoDisp(c, c.vibratoPhase));
			} else {
				c.vibratoStartCtr++;
			}
		}
	}

	// $07F9 + $0625
	private bool doCycle(ref SongState st) nothrow @safe {
		foreach (ref c; st.chan) {
			if (c.parser.ptr == null) {
				continue; //8F0
			}

			if (--c.next >= 0) {
				CF7(c);
			} else {
				loop: while (1) {
					bool done;
					const command = c.parser.popCommand(done);
					final switch (command.type) {
						case VCMDClass.terminator:
							if (done) {
								return false;
							}
							break;
						case VCMDClass.noteDuration:
							c.noteLen = command.noteDuration;
							if (command.parameters.length > 0) {
								c.noteStyle = command.parameters[0];
							}
							break;
						case VCMDClass.note:
						case VCMDClass.tie:
						case VCMDClass.rest:
						case VCMDClass.percussion:
							c.next = c.noteLen - 1;
							doNote(st, c, command);
							break loop;
						case VCMDClass.special:
							doCommand(st, c, command);
							break;
					}
				}
			}
			// $0B84
			if (c.note.cycles == 0) {
				size_t length;
				const command = readCommand(variant, c.parser.ptr, length);
				if (command.special == VCMD.pitchSlideToNote) {
					doCommand(st, c, command);
					c.parser.popCommand();
				}
			}
		}

		st.patpos++;

		st.tempo.slide();
		st.volume.slide();

		foreach (ref c; st.chan) {
			if (c.parser.ptr == null) {
				continue;
			}

			// @ 0C40
			c.volume.slide();

			// @ 0C4D
			int tphase = 0;
			if (c.tremoloRange) {
				if (c.tremoloStartCtr == c.tremoloStart) {
					if (c.tremoloPhase >= 0x80 && c.tremoloRange == 0xFF) {
						c.tremoloPhase = 0x80;
					} else {
						c.tremoloPhase += c.tremoloSpeed;
					}
					tphase = c.tremoloPhase;
				} else {
					c.tremoloStartCtr++;
				}
			}
			calcTotalVolume(st, c, cast(byte) tphase);

			// 0C79
			c.panning.slide();

			// 0C86: volume stuff
			calcVolume2(c, c.panning.cur);
		}
		return true;
	}

	private int subCycleCalc(const SongState st, int delta) nothrow @safe {
		if (delta < 0x8000) {
			return st.cycleTimer * delta >> 8;
		} else {
			return -(st.cycleTimer * (0x10000 - delta) >> 8);
		}
	}

	private void doSubCycle(ref SongState st) nothrow @safe {
		foreach (ref c; st.chan) {
			if (c.parser.ptr == null) {
				continue;
			}
			// $0DD0

			bool changed = false;
			if (c.tremoloRange && c.tremoloStartCtr == c.tremoloStart) {
				int p = c.tremoloPhase + subCycleCalc(st, c.tremoloSpeed);
				changed = true;
				calcTotalVolume(st, c, cast(byte) p);
			}
			int pan = c.panning.cur;
			if (c.panning.cycles) {
				pan += subCycleCalc(st, c.panning.delta);
				changed = true;
			}
			if (changed) {
				calcVolume2(c, pan);
			}

			changed = false;
			int note = c.note.cur; // $0BBC
			if (c.note.cycles && c.curPortStartCtr == 0) {
				note += subCycleCalc(st, c.note.delta);
				changed = true;
			}
			if (c.curVibRange && c.vibratoStartCtr == c.vibratoStart) {
				int p = c.vibratoPhase + subCycleCalc(st, c.vibratoSpeed);
				note += calcVibratoDisp(c, p);
				changed = true;
			}
			if (changed) {
				setFrequency(c, note);
			}
		}
	}

	private bool doTimer() nothrow @safe {
		state.cycleTimer += state.tempo.cur >> 8;
		if (state.cycleTimer >= 256) {
			state.cycleTimer -= 256;
			while (!doCycle(state)) {
				loadPattern();
				if (!songPlaying) {
					return false;
				}
			}
		} else {
			doSubCycle(state);
		}
		return true;
	}

	/// Initialize or reset the player state
	void initialize(int sampleRate) nothrow @safe {
		state = state.init;

		if (currentSong.order.length) {
			loadPattern();
		} else {
			songPlaying = false;
		}
		foreach (idx, ref channel; state.chan) {
			channel.parser.variant = variant;
			channel.parser.subRoutines = currentSong.subRoutines;
		}
		mixrate = sampleRate;
	}

	/// Start playing music
	void play() @safe nothrow {
		initialize(mixrate);
		songPlaying = true;
	}
	void stop() @safe nothrow {
		songPlaying = false;
	}
	/// Load a single sequence pack at a given address
	void loadSequencePack(const(ubyte)[] data, ushort base) @safe {
		loadSequence(data, base);
	}
	/// Load a single sequence pack, automatically detecting the address from the pack header
	void loadSequencePack(const(ubyte)[] data) @safe {
		ushort base = read!ushort(data, 2);
		loadSequence(data, base);
	}
	private void loadSequence(const(ubyte)[] data, ushort base) @safe {
		ubyte[65536] buffer;
		loadAllSubpacks(buffer, data);
		decompileSong(buffer[], currentSong, base, cast(int)(base + data.length));
	}
	/// Load instruments from provided packs at the given addresses
	void loadInstruments(const(ubyte)[][] packs, ushort instrumentBase, ushort sampleBase) @safe {
		ubyte[65536] buffer;
		foreach (pack; packs) {
			loadInstrumentPack(buffer, pack);
		}
		initializeInstruments(buffer, instrumentBase, sampleBase);
	}
	/// Load instruments from provided packs at the given addresses
	void loadInstrumentPack(scope ref ubyte[65536] buffer, const(ubyte)[] pack) @safe {
		loadAllSubpacks(buffer[], pack);
	}
	void initializeInstruments(scope const ref ubyte[65536] buffer, ushort instrumentBase, ushort sampleBase) @safe {
		NSPCFileHeader fakeHeader;
		fakeHeader.instrumentBase = instrumentBase;
		fakeHeader.sampleBase = sampleBase;
		processInstruments(buffer, fakeHeader);
	}
	private void loadAllSubpacks(scope ubyte[] buffer, const(ubyte)[] pack) @safe {
		ushort size, base;
		while (true) {
			if (pack.length == 0) {
				break;
			}
			size = read!ushort(pack);
			if (size == 0) {
				break;
			}
			base = read!ushort(pack, 2);
			debug(nspclogging) tracef("Loading subpack to %X (%s bytes)", base, size);
			enforce!NSPCException(base + size <= ushort.max, "Invalid pack - base + size exceeds 64KB memory limit");
			buffer[base .. base + size] = pack[4 .. size + 4];
			pack = pack[size + 4 .. $];
		}
	}

	/// Load an NSPC file
	void loadNSPCFile(const(ubyte)[] data) @safe {
		if (songPlaying) {
			stop();
		}
		ubyte[65536] buffer;
		auto header = read!NSPCFileHeader(data);
		debug(nspclogging) tracef("Loading NSPC - so: %X, i: %X, sa: %X, variant: %s", header.songBase, header.instrumentBase, header.sampleBase, header.variant);
		variant = header.variant;
		volumeTable = header.volumeTable;
		releaseTable = header.releaseTable;
		debug(nspclogging) tracef("Release table: %s, volume table: %s", header.releaseTable, header.volumeTable);
		assert(volumeTable < volumeTables.length, "Invalid volume table");
		assert(releaseTable < releaseTables.length, "Invalid release table");
		loadAllSubpacks(buffer[], data[NSPCFileHeader.sizeof .. $]);
		processInstruments(buffer, header);
		decompileSong(buffer[], currentSong, header.songBase, buffer.length - 1);
	}

	private void processInstruments(scope const ubyte[] buffer, const NSPCFileHeader header) @safe {
		decodeSamples(buffer, cast(const(ushort[2])[])(buffer[header.sampleBase .. header.sampleBase + maxSampleCount * 4]));
		instruments = [];
		instruments.reserve(maxInstruments);
		if (variant == Variant.prototype) {
			size_t instrumentCount = maxInstruments;
			if (header.instrumentBase < header.extra.percussionBase) {
				instrumentCount = min(instrumentCount, (header.extra.percussionBase - header.instrumentBase) / PrototypeInstrument.sizeof);
			}
			foreach (idx, instrument; cast(const(PrototypeInstrument)[])(buffer[header.instrumentBase .. header.instrumentBase + instrumentCount * PrototypeInstrument.sizeof])) {
				instruments ~= cast(Instrument)instrument;
			}
			percussionBase = instrumentCount;
			foreach (idx, percussion; cast(const(PrototypePercussion)[])(buffer[header.extra.percussionBase .. header.extra.percussionBase + maxInstruments * PrototypePercussion.sizeof])) {
				instruments ~= cast(Instrument)percussion;
				percussionNotes[idx] = cast(ubyte)(percussion.note - 0x80);
			}
		} else {
			foreach (idx, instrument; cast(const(Instrument)[])(buffer[header.instrumentBase .. header.instrumentBase + maxInstruments * Instrument.sizeof])) {
				instruments ~= instrument;
			}
			percussionNotes = 0x24;
		}
		debug(nspclogging) foreach (idx, instrument; instruments) {
			 if (samp[instrument.sampleID].isValid && instrument.tuning != 0) {
				tracef("%s (%s) - %s", idx, ((percussionBase > 0) && (idx > percussionBase)) ? "Percussion" : "Standard", instrument);
			}
		}
	}

	private void decompileSong(scope ubyte[] data, ref Song song, int startAddress, int endAddress) @safe {
		immutable copyData = data.idup;
		int patterns;
		song = song.init;
		song.address = cast(ushort) startAddress;
		song.changed = false;

		// Get order length and repeat info (at this point, we don't know how
		// many patterns there are, so the pattern pointers aren't validated yet)
		const ushort[] wpO = cast(ushort[]) data[startAddress .. $ -  ($ - startAddress) % 2];
		uint index;
		uint phraseCount;
		bool skipNextShort;
		while (wpO[index] != 0) {
			if (wpO[index] < 0x80) {
				skipNextShort = true;
			} else if ((wpO[index] > 0x81) && (wpO[index] < 0x100)) {
				skipNextShort = true;
			}
			if (!skipNextShort) {
				phraseCount++;
			} else {
				skipNextShort = false;
			}
			index++;
		}
		ushort phraseID(ushort address) {
			ushort id;
			const offset = (address - startAddress) >> 1;
			uint o;
			bool skip;
			while ((o < offset) && (wpO[o] != 0)) {
				if (wpO[o] < 0x80) {
					skip = true;
				} else if ((wpO[o] > 0x81) && (wpO[o] < 0x100)) {
					skip = true;
				}
				if (!skip) {
					id++;
				} else {
					skip = false;
				}
				o++;
			}
			return id;
		}
		enforce!NSPCException(phraseCount > 0, "No phrases in song");
		song.order.length = phraseCount + 1;
		index++;

		const fpIndex = index;
		const firstPattern = startAddress + index * 2;

		const phrases = wpO[0 .. index];
		// Now the number of patterns is known, so go back and get the order
		size_t idx;
		ushort[] extraPatterns;
		foreach (ref order; song.order) {
			if (phrases[idx] == 0) {
				order.type = PhraseType.end;
			} else if (phrases[idx] == 0x80) {
				order.type = PhraseType.fastForwardOn;
			} else if (phrases[idx] == 0x81) {
				order.type = PhraseType.fastForwardOff;
			} else if (phrases[idx] < 0x80) {
				order.type = PhraseType.jumpLimited;
				order.jumpTimes = phrases[idx];
				idx++;
				order.id = phraseID(phrases[idx]);
			} else if ((phrases[idx] > 0x81) && (phrases[idx] < 0x100)) {
				order.type = PhraseType.jump;
				idx++;
				order.id = phraseID(phrases[idx]);
			} else {
				order.type = PhraseType.pattern;
				if (phrases[idx] >= firstPattern) {
					int pat = phrases[idx] - firstPattern;
					order.id = cast(ushort)(pat >> 4);
					patterns = max((pat >> 4) + 1, patterns);
				} else {
					if (!extraPatterns.canFind(phrases[idx])) {
						debug(nspclogging) tracef("Allocating new phrase: %04X", phrases[idx]);
						extraPatterns ~= phrases[idx];
					}
					order.id = phrases[idx];
					order.unallocated = true;
				}
			}
			idx++;
		}
		sort(extraPatterns);
		foreach (newPatternID, extraPattern; extraPatterns) {
			foreach (ref phrase; song.order) {
				if (phrase.id == extraPattern) {
					assert(phrase.unallocated);
					debug infof("%s, %s", patterns, newPatternID);
					phrase.id = cast(ushort)(patterns + newPatternID);
					phrase.unallocated = false;
				}
			}
			patterns++;
		}

		debug(nspclogging) tracef("Phrases: %(%s, %)", song.order);
		validatePhrases();

		song.pattern = new Track[8][](patterns);
		song.subRoutines = null;

		index = fpIndex;
		ushort[] subTable;
		ubyte tmpPercussionBase;
		for (int trk = 0; trk < song.pattern.length * 8; trk++) {
			Track* t = &song.pattern[trk / 8][trk % 8];
			ushort start = wpO[index++];
			if (start == 0) {
				continue;
			}
			// Go through track list (patterns) and find first track that has an address higher than us.
			// If we find a track after us, we'll assume that this track doesn't overlap with that one.
			// If we don't find one, then next will remain at 0x10000 and we will search until the
			// end of memory to find a 00 byte to terminate the track.
			uint next = 0x10000; // offset of following track
			if (start < startAddress) {
				next = startAddress;
			}
			for (int trackIndex = 0; trackIndex < (song.pattern.length * 8); trackIndex += 1) {
				int trackAddress = wpO[fpIndex + trackIndex];
				if (trackAddress < next && trackAddress > start) {
					next = trackAddress;
				}
			}
			decompileTrack(copyData, start, next, *t, song, subTable, tmpPercussionBase);
		}
	}
	private void decompileTrack(immutable(ubyte)[] data, ushort start, uint next, ref Track t, ref Song song, ref ushort[] subTable, ref ubyte tmpPercussionBase) @safe {
		// Determine the end of the track.
		const(ubyte)[] trackEnd = data[start .. $];
		size_t length;
		size_t totalLength;
		while (true) {
			const command = readCommand(variant, trackEnd, length);
			trackEnd = trackEnd[length .. $];
			totalLength += length;
			if (command.type == VCMDClass.terminator) {
				break;
			}
			if (command.special == VCMD.subRoutine) { //decompile subroutines too
				ushort subPtr = read!ushort(command.parameters);

				song.subRoutines.require(subPtr, () {
					Track st;
					const(ubyte)[] subData = data[subPtr .. $];
					size_t subLength;
					size_t subTotalLength;
					while (readCommand(variant, subData, subLength).type != VCMDClass.terminator) {
						subData = subData[subLength .. $];
						subTotalLength += subLength;
					}
					st.size = cast(int)(subTotalLength + 1);
					st.track = data[subPtr .. subPtr + subTotalLength + 1];
					validateTrack(st.track, true, tmpPercussionBase);
					return st;
				}());
			}
		}
		t.size = cast(int)(totalLength);
		t.track = data[start .. start + t.size];

		validateTrack(t.track, false, tmpPercussionBase);
	}

	private void decodeSamples(scope const ubyte[] buffer, const scope ushort[2][] ptrtable) nothrow @safe {
		for (uint sn = 0; sn < 128; sn++) {
			const start = ptrtable[sn][0];
			const loop = ptrtable[sn][1];
			if (start == 0 || start == 0xffff) {
				continue;
			}
			try {
				samp[sn] = decodeSample(buffer, start, loop);
			} catch (Exception e) {
				debug tracef("Couldn't load sample %d: %s", sn, e.msg);
			}
		}
	}

	private void validateTrack(scope const ubyte[] track, bool isSub, ref ubyte tmpPercussionBase) @safe {
		const(ubyte)[] data = track;
		bool endReached;
		while (data.length > 0) {
			size_t length;
			const command = readCommand(variant, data, length);
			data = data[length .. $];
			enforce!NSPCException(!endReached, "Track must end with terminator");
			final switch (command.type) {
				case VCMDClass.terminator:
					endReached = true;
					break;
				case VCMDClass.noteDuration:
					enforce!NSPCException(data.length > 0, "Track can not end with note-length code");
					break;
				case VCMDClass.percussion:
					validateInstrument(absoluteInstrumentID(command.instrument, tmpPercussionBase, true));
					break;
				case VCMDClass.special:
					enforce!NSPCException(command.special != VCMD.invalid, format!"Invalid code command %s"(command));

					if (command.special == VCMD.instrument) {
						validateInstrument(absoluteInstrumentID(command.parameters[0], tmpPercussionBase, false));
					}
					if (command.special == VCMD.subRoutine) {
						enforce!NSPCException(!isSub, "Can't call sub from within a sub");
						ushort sub = read!ushort(command.parameters);
						enforce!NSPCException(sub in currentSong.subRoutines, format!"Subroutine %d not present"(sub));
						enforce!NSPCException(command.parameters[2] != 0, "Subroutine loop count can not be 0");
					}
					if (command.special == VCMD.percussionBaseInstrumentRedefine) {
						tmpPercussionBase = command.parameters[0];
					}
					break;
				case VCMDClass.note: // nothing to validate here
				case VCMDClass.tie:
				case VCMDClass.rest:
					break;
			}
		}
	}
	private void validatePhrases() const @safe {
		bool endFound;
		foreach (phrase; currentSong.order) {
			enforce!NSPCException(!endFound, "Phrases found after end of song");
			enforce!NSPCException(!phrase.unallocated, "Unallocated phrase found");
			final switch (phrase.type) {
				case PhraseType.end:
					endFound = true;
					break;
				case PhraseType.fastForwardOn:
				case PhraseType.fastForwardOff:
					throw new NSPCException("Fast forward not yet supported");
				case PhraseType.jump:
				case PhraseType.jumpLimited:
					enforce!NSPCException(phrase.id < currentSong.order.length, "Cannot jump past end of song");
					break;
				case PhraseType.pattern:
					break;
			}
		}
		enforce!NSPCException(currentSong.order.length > 0, "No phrases loaded");
		enforce!NSPCException(currentSong.order[$ - 1].type == PhraseType.end, "Phrase list must have an end phrase");
	}
	private void validateInstrument(size_t id) const @safe {
		enforce!NSPCException(id < instruments.length, format!"Invalid instrument %s - Index out of bounds"(id));
		const idata = instruments[id];
		enforce!NSPCException(samp[idata.sampleID].isValid, format!"Invalid instrument %s - Invalid sample %s"(id, idata.sampleID));
		if (idata.tuning == 0) {
			infof("Suspicious instrument %s - no tuning (will be silent)", id);
		}
	}
	private size_t absoluteInstrumentID(size_t id, size_t base, bool percussion) const @safe pure nothrow {
		if (id >= percussionID(variant)) {
			percussion = true;
			id -= percussionID(variant);
		}
		if (percussion) {
			id += base + percussionBase;
		}
		return id;
	}
	/// Sets the playback speed. Default value is NSPCPlayer.defaultSpeed.
	public void setSpeed(ushort rate) @safe nothrow {
		timerSpeed = rate;
	}
	/// Enable or disable song looping
	public void looping(bool enabled) @safe nothrow {
		loopEnabled = enabled;
	}
	/// Enable or disable a song channel
	public void setChannelEnabled(ubyte channel, bool enabled) @safe nothrow {
		const newChmask = 1 << channel;
		if (enabled) {
			chmask |= newChmask;
		} else {
			chmask &= ~newChmask;
		}
	}
	bool isPlaying() const pure @safe nothrow {
		return songPlaying;
	}
	const(Sample)[] getSamples() const pure @safe nothrow return {
		import std.algorithm.iteration : filter;
		import std.array : array;
		return samp[].filter!(x => x.isValid).array;
	}
}

private T read(T)(const(ubyte)[] data, size_t offset = 0) {
	return (cast(const(T)[])(data[offset .. offset + T.sizeof]))[0];
}

private bool inRange(T)(T val, T lower, T upper) {
	return ((val >= lower) && (val <= upper));
}

class NSPCException : Exception {
	this(string msg, string file = __FILE__, size_t line = __LINE__) @safe {
		super(msg, file, line);
	}
}
