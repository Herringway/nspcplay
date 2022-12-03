module nspcplay.sequence;

import std.experimental.logger;
import std.format : FormatSpec;

import nspcplay.common;

enum VCMDClass {
	terminator,
	noteDuration,
	note,
	tie,
	rest,
	percussion,
	special,
}

enum VCMD {
	instrument,
	panning,
	panningFade,
	vibratoOn,
	vibratoOff,
	songVolume,
	songVolumeFade,
	tempo,
	tempoFade,
	globalAbsoluteTransposition,
	channelAbsoluteTransposition,
	tremoloOn,
	tremoloOff,
	volume,
	volumeFade,
	subRoutine,
	vibratoFadeIn,
	notePitchEnvelopeTo,
	notePitchEnvelopeFrom,
	notePitchEnvelopeOff,
	fineTune,
	echoEnableBitsAndVolume,
	echoOff,
	echoParameterSetup,
	echoVolumeFade,
	pitchSlideToNote,
	percussionBaseInstrumentRedefine,
	noop0,
	noop1,
	noop2,
	channelMute,
	fastForwardOn,
	fastForwardOff,
	invalid,
	// konami vcmds
	konamiPanning,
	konamiPanningFade,
	konamiE4,
	konamiLoopStart,
	konamiLoopEnd,
	konamiE7,
	konamiF5,
	konamiADSRGain,
}

// number of bytes following a Ex/Fx code
private immutable ubyte[VCMD.max + 1] codeLength = [
	VCMD.instrument: 1, // E0
	VCMD.panning: 1, // E1
	VCMD.panningFade: 2, // E2
	VCMD.vibratoOn: 3, // E3
	VCMD.vibratoOff: 0, // E4
	VCMD.songVolume: 1, // E5
	VCMD.songVolumeFade: 2, // E6
	VCMD.tempo: 1, // E7
	VCMD.tempoFade: 2, // E8
	VCMD.globalAbsoluteTransposition: 1, // E9
	VCMD.channelAbsoluteTransposition: 1, // EA
	VCMD.tremoloOn: 3, // EB
	VCMD.tremoloOff: 0, // EC
	VCMD.volume: 1, // ED
	VCMD.volumeFade: 2, // EE
	VCMD.subRoutine: 3, // EF
	VCMD.vibratoFadeIn: 1, // F0
	VCMD.notePitchEnvelopeTo: 3, // F1
	VCMD.notePitchEnvelopeFrom: 3, // F2
	VCMD.notePitchEnvelopeOff: 0, // F3
	VCMD.fineTune: 1, // F4
	VCMD.echoEnableBitsAndVolume: 3, // F5
	VCMD.echoOff: 0, // F6
	VCMD.echoParameterSetup: 3, // F7
	VCMD.echoVolumeFade: 3, // F8
	VCMD.pitchSlideToNote: 3, // F9
	VCMD.percussionBaseInstrumentRedefine: 1, // FA
	VCMD.channelMute: 0, // FC
	VCMD.fastForwardOn: 0, // FD
	VCMD.fastForwardOff: 0, // FE
	/// konami vcmds
	VCMD.konamiPanning: 1, // E1
	VCMD.konamiPanningFade: 2, // E2
	VCMD.konamiE4: 2, // E4
	VCMD.konamiLoopStart: 0, // E5
	VCMD.konamiLoopEnd: 3, // E6
	VCMD.konamiE7: 1, // E7
	VCMD.konamiF5: 3, // F5
	VCMD.konamiADSRGain: 3, // FB
	// vcmds that don't actually do anything
	VCMD.noop0: 0,
	VCMD.noop1: 1,
	VCMD.noop2: 2,
	// error
	VCMD.invalid: 0,
];

VCMDClass getCommandClass(Variant variant, ubyte val, out ubyte base) nothrow @safe pure {
	final switch (variant) {
		case Variant.standard:
		case Variant.konami:
			if (val == 0) {
				return VCMDClass.terminator;
			} else if (val < 0x80) {
				return VCMDClass.noteDuration;
			} else if (val < 0xC8) {
				base = 0x80;
				return VCMDClass.note;
			} else if (val == 0xC8) {
				return VCMDClass.tie;
			} else if (val == 0xC9) {
				return VCMDClass.rest;
			} else if (val < 0xE0) {
				base = 0xCA;
				return VCMDClass.percussion;
			} else  {
				base = 0xE0;
				return VCMDClass.special;
			}
		case Variant.prototype:
			if (val == 0) {
				return VCMDClass.terminator;
			} else if (val < 0x80) {
				return VCMDClass.noteDuration;
			} else if (val < 0xC6) {
				base = 0x80;
				return VCMDClass.note;
			} else if (val == 0xC6) {
				return VCMDClass.tie;
			} else if (val == 0xC7) {
				return VCMDClass.rest;
			} else if (val < 0xDA) {
				base = 0xD0;
				return VCMDClass.percussion;
			} else  {
				base = 0xDA;
				return VCMDClass.special;
			}
	}
}
ubyte percussionID(Variant variant) nothrow pure @safe {
	if (variant == Variant.prototype) {
		return 0xD0;
	}
	return 0xCA;
}
VCMD getCommand(Variant variant, ubyte val) nothrow @safe pure {
	final switch(variant) {
		case Variant.konami:
			switch(val) {
				case 0xE0: return VCMD.instrument;
				case 0xE1: return VCMD.konamiPanning;
				case 0xE2: return VCMD.konamiPanningFade;
				case 0xE3: return VCMD.vibratoOn;
				case 0xE4: return VCMD.konamiE4;
				case 0xE5: return VCMD.konamiLoopStart;
				case 0xE6: return VCMD.konamiLoopEnd;
				case 0xE7: return VCMD.tempo;
				case 0xE8: return VCMD.noop2;
				case 0xE9: return VCMD.noop1;
				case 0xEA: return VCMD.channelAbsoluteTransposition;
				case 0xEB: return VCMD.tremoloOn;
				case 0xEC: return VCMD.tremoloOff;
				case 0xED: return VCMD.volume;
				case 0xEE: return VCMD.volumeFade;
				case 0xEF: return VCMD.subRoutine;
				case 0xF0: return VCMD.vibratoFadeIn;
				case 0xF1: return VCMD.notePitchEnvelopeTo;
				case 0xF2: return VCMD.notePitchEnvelopeFrom;
				case 0xF3: return VCMD.notePitchEnvelopeOff;
				case 0xF4: return VCMD.fineTune;
				case 0xF5: return VCMD.konamiF5;
				case 0xF6: return VCMD.konamiF5;
				case 0xF7: return VCMD.konamiF5;
				case 0xF8: return VCMD.konamiF5;
				case 0xF9: return VCMD.pitchSlideToNote;
				case 0xFA: return VCMD.percussionBaseInstrumentRedefine;
				case 0xFB: return VCMD.konamiADSRGain;
				case 0xFC: return VCMD.noop0;
				case 0xFD: return VCMD.noop0;
				case 0xFE: return VCMD.noop0;
				case 0xFF: return VCMD.invalid;
				default: break;
			}
			break;
		case Variant.standard:
			switch (val) {
				case 0xE0: return VCMD.instrument;
				case 0xE1: return VCMD.panning;
				case 0xE2: return VCMD.panningFade;
				case 0xE3: return VCMD.vibratoOn;
				case 0xE4: return VCMD.vibratoOff;
				case 0xE5: return VCMD.songVolume;
				case 0xE6: return VCMD.songVolumeFade;
				case 0xE7: return VCMD.tempo;
				case 0xE8: return VCMD.tempoFade;
				case 0xE9: return VCMD.globalAbsoluteTransposition;
				case 0xEA: return VCMD.channelAbsoluteTransposition;
				case 0xEB: return VCMD.tremoloOn;
				case 0xEC: return VCMD.tremoloOff;
				case 0xED: return VCMD.volume;
				case 0xEE: return VCMD.volumeFade;
				case 0xEF: return VCMD.subRoutine;
				case 0xF0: return VCMD.vibratoFadeIn;
				case 0xF1: return VCMD.notePitchEnvelopeTo;
				case 0xF2: return VCMD.notePitchEnvelopeFrom;
				case 0xF3: return VCMD.notePitchEnvelopeOff;
				case 0xF4: return VCMD.fineTune;
				case 0xF5: return VCMD.echoEnableBitsAndVolume;
				case 0xF6: return VCMD.echoOff;
				case 0xF7: return VCMD.echoParameterSetup;
				case 0xF8: return VCMD.echoVolumeFade;
				case 0xF9: return VCMD.pitchSlideToNote;
				case 0xFA: return VCMD.percussionBaseInstrumentRedefine;
				case 0xFB: return VCMD.noop2;
				case 0xFC: return VCMD.channelMute;
				case 0xFD: return VCMD.fastForwardOn;
				case 0xFE: return VCMD.fastForwardOff;
				case 0xFF: return VCMD.invalid;
				default: break;
			}
			break;
		case Variant.prototype:
			switch (val) {
				case 0xDA: return VCMD.instrument;
				case 0xDB: return VCMD.panning;
				case 0xDC: return VCMD.panningFade;
				case 0xDD: return VCMD.pitchSlideToNote;
				case 0xDE: return VCMD.vibratoOn;
				case 0xDF: return VCMD.vibratoOff;
				case 0xE0: return VCMD.songVolume;
				case 0xE1: return VCMD.songVolumeFade;
				case 0xE2: return VCMD.tempo;
				case 0xE3: return VCMD.tempoFade;
				case 0xE4: return VCMD.globalAbsoluteTransposition;
				case 0xE5: return VCMD.tremoloOn;
				case 0xE6: return VCMD.tremoloOff;
				case 0xE7: return VCMD.volume;
				case 0xE8: return VCMD.volumeFade;
				case 0xE9: return VCMD.subRoutine;
				case 0xEA: return VCMD.vibratoFadeIn;
				case 0xEB: return VCMD.notePitchEnvelopeTo;
				case 0xEC: return VCMD.notePitchEnvelopeFrom;
				case 0xED: return VCMD.invalid;
				case 0xEE: return VCMD.fineTune;
				case 0xEF: return VCMD.echoEnableBitsAndVolume;
				case 0xF0: return VCMD.echoOff;
				case 0xF1: return VCMD.echoParameterSetup;
				case 0xF2: return VCMD.echoVolumeFade;
				case 0xF3: .. case 0xFF: return VCMD.invalid;
				default: break;
			}
			break;
	}
	assert(0, "Unknown command");
}

struct Command {
	private ubyte _cmd;
	private ubyte _cmdBase;
	VCMDClass type;
	VCMD special;
	const(ubyte)[] parameters;
	private const(ubyte)[] raw;
	/// returns byte 0, minus the beginning of whatever class of command
	ubyte relative() const @safe pure nothrow scope {
		return cast(ubyte)(_cmd - _cmdBase);
	}
	///
	alias noteDuration = relative;
	///
	alias note = relative;
	///
	alias instrument = relative;
	private void _tmp() const {
		import std.range : nullSink;
		auto n = nullSink;
		toString(n, *(new FormatSpec!char()));
	}
	void toString(S, C)(ref S sink, scope const ref FormatSpec!C fmt) const @safe {
		import std.format : formattedWrite;
		import std.range : put;
		if (fmt.spec == 'x') {
			sink.formattedWrite!"[%(%02X %)] "(raw);
		}
		const commandClass = type;
		final switch (commandClass) {
			case VCMDClass.terminator:
				put(sink, "End of track");
				break;
			case VCMDClass.noteDuration:
				sink.formattedWrite!"Note duration: %s"(noteDuration);
				if (parameters.length > 0) {
					sink.formattedWrite!", quantization: %s, velocity: %s"(parameters[0] >> 4, parameters[0] & 0xF);
				}
				break;
			case VCMDClass.note:
				sink.formattedWrite!"Note: %02X"(note);
				break;
			case VCMDClass.percussion:
				sink.formattedWrite!"Percussion: %s"(instrument);
				break;
			case VCMDClass.tie:
				put(sink, "Tie");
				break;
			case VCMDClass.rest:
				put(sink, "Rest");
				break;
			case VCMDClass.special:
				final switch(special) {
					case VCMD.instrument:
						sink.formattedWrite!"Set instrument: %s"(parameters[0]);
						break;
					case VCMD.panning:
						sink.formattedWrite!"Set panning - inversion(%s,%s), value: %s"(!!(parameters[0] & 0x80), !!(parameters[0] & 0x40), parameters[0] & 0x3F);
						break;
					case VCMD.panningFade:
						sink.formattedWrite!"Set panning fade - ticks: %s, target: %s"(parameters[0], parameters[1]);
						break;
					case VCMD.vibratoOn:
						sink.formattedWrite!"Set vibrato - tick delay: %s, rate: %s, depth: %s"(parameters[0], parameters[1], (parameters[2] > 0xF0) ? ((parameters[2] & 0xF) * 256) : parameters[2]);
						break;
					case VCMD.vibratoOff:
						put(sink, "Vibrato off");
						break;
					case VCMD.songVolume:
						sink.formattedWrite!"Set song volume: %s"(parameters[0]);
						break;
					case VCMD.songVolumeFade:
						break;
					case VCMD.tempo:
						sink.formattedWrite!"Set song tempo: %s"(256.0 / parameters[0]);
						break;
					case VCMD.tempoFade:
						sink.formattedWrite!"Set song tempo fade: ticks: %s, target: %s"(parameters[0], 256.0 / parameters[1]);
						break;
					case VCMD.globalAbsoluteTransposition:
						sink.formattedWrite!"Set global absolute transposition: %s"(parameters[0]);
						break;
					case VCMD.channelAbsoluteTransposition:
						sink.formattedWrite!"Set channel absolute transposition: %s"(parameters[0]);
						break;
					case VCMD.tremoloOn:
						sink.formattedWrite!"Tremolo on: tick delay: %s, rate: %s, depth: %s"(parameters[0], parameters[1], parameters[2]);
						break;
					case VCMD.tremoloOff:
						put(sink, "Tremolo off");
						break;
					case VCMD.volume:
						sink.formattedWrite!"Set volume: %s"(parameters[0]);
						break;
					case VCMD.volumeFade:
						sink.formattedWrite!"Set volume fade: ticks: %s, target: %s"(parameters[0], parameters[1]);
						break;
					case VCMD.subRoutine:
						sink.formattedWrite!"Call subroutine: %04X %s times"((cast(const(ushort)[])(parameters[0 .. 2]))[0], parameters[2]);
						break;
					case VCMD.vibratoFadeIn:
						sink.formattedWrite!"Vibrato fade in: %s"(parameters[0]);
						break;
					case VCMD.notePitchEnvelopeTo:
						sink.formattedWrite!"Note pitch envelope to: delay: %s, duration: %s, target: %s"(parameters[0], parameters[1], cast(byte)parameters[2]);
						break;
					case VCMD.notePitchEnvelopeFrom:
						sink.formattedWrite!"Note pitch envelope from: delay: %s, duration: %s, target: %s"(parameters[0], parameters[1], cast(byte)parameters[2]);
						break;
					case VCMD.notePitchEnvelopeOff:
						put(sink, "Pitch envelope off");
						break;
					case VCMD.fineTune:
						sink.formattedWrite!"Fine tune: %s"(parameters[0]);
						break;
					case VCMD.echoEnableBitsAndVolume:
						sink.formattedWrite!"Echo enable: %08b, left: %s, right: %s"(parameters[0], parameters[1], parameters[2]);
						break;
					case VCMD.echoOff:
						put(sink, "Turn echo off");
						break;
					case VCMD.echoParameterSetup:
						sink.formattedWrite!"Set echo parameters: delay: %s, feedback: %s, FIR: %s"(parameters[0], parameters[1], parameters[2]);
						break;
					case VCMD.echoVolumeFade:
						sink.formattedWrite!"Echo volume fade: duration: %s, left: %s, right: %s"(parameters[0], parameters[1], parameters[2]);
						break;
					case VCMD.pitchSlideToNote:
						sink.formattedWrite!"Pitch slide to: delay: %s, duration: %s, target: %s"(parameters[0], parameters[1], parameters[2]);
						break;
					case VCMD.percussionBaseInstrumentRedefine:
						sink.formattedWrite!"Set percussion base instrument: %s"(parameters[0]);
						break;
					case VCMD.noop0:
					case VCMD.noop1:
					case VCMD.noop2:
						put(sink, "Do nothing");
						break;
					case VCMD.konamiE4:
					case VCMD.konamiE7:
					case VCMD.konamiF5:
						put(sink, "???");
						break;
					case VCMD.konamiLoopStart:
						put(sink, "Loop start?");
						break;
					case VCMD.konamiLoopEnd:
						sink.formattedWrite!"Loop end?: times: %s, volume delta: %s, pitch delta: %s"(parameters[0], parameters[1], parameters[2]);
						break;
					case VCMD.konamiADSRGain:
						sink.formattedWrite!"Set ADSR/gain: %s"(konamiADSRGain(parameters));
						break;
					case VCMD.konamiPanning:
						put(sink, "Panning?");
						break;
					case VCMD.konamiPanningFade:
						put(sink, "Panning fade?");
						break;
					case VCMD.channelMute:
						put(sink, "Mute channel");
						break;
					case VCMD.fastForwardOn:
						put(sink, "Fast forward enabled");
						break;
					case VCMD.fastForwardOff:
						put(sink, "Fast forward disabled");
						break;
					case VCMD.invalid:
						put(sink, "Invalid command");
						break;
				}
				break;
		}
	}
}

Command readCommand(Variant variant, return scope const(ubyte)[] p) nothrow @safe pure {
	size_t _;
	return readCommand(variant, p, _);
}
Command readCommand(Variant variant, return scope const(ubyte)[] p, out size_t readBytes) nothrow @safe pure {
	Command command;
	command.type = getCommandClass(variant, p[0], command._cmdBase);
	readBytes = 1;
	command._cmd = p[0];
	if (command.type == VCMDClass.noteDuration) {
		readBytes += p[1] < 0x80;
	} else if (command.type == VCMDClass.special) {
		command.special = getCommand(variant, p[0]);
		readBytes += codeLength[command.special];
	}
	command.parameters = p[1 .. readBytes];
	command.raw = p[0 .. readBytes];
	//debug tracef("Read command: %s", command);
	return command;
}

