module nspcplay.player;

import nspcplay.common;
import nspcplay.song;
import nspcplay.sequence;
import nspcplay.samples;

import std.algorithm.comparison;
import std.exception;
import std.experimental.logger;
import std.format;

enum uint nativeSamplingRate = 32000;

private struct SongState {
	ChannelState[8] channels;
	byte transpose;
	Slider volume = Slider(0xC000);
	Slider tempo = Slider(0x2000);
	int nextTimerTick;
	int cycleTimer = 255;
	ubyte percussionBase; // set with FA
	ubyte repeatCount;
	int phraseCounter = -1;

	ubyte fadeTicks;
	ubyte targetEchoVolumeLeft;
	ubyte targetEchoVolumeRight;
	bool echoWrites;
	ubyte echoOn;
	ubyte echoRemaining = 1;
	ubyte echoVolumeLeft;
	ubyte echoVolumeRight;
	ubyte echoDelay;
	ubyte echoFeedbackVolume;
	ushort echoBufferIndex;
	ubyte firBufferIndex;
	short[15000] echoBuffer;
	ubyte[8] firCoefficients;
	short[8] firLeft;
	short[8] firRight;
}

private struct Slider {
	ushort current;
	ushort delta;
	ubyte cycles;
	ubyte target;

	private void slide() nothrow @safe pure {
		if (cycles) {
			if (--cycles == 0) {
				current = target << 8;
			} else {
				current += delta;
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
	bool enabled = true;
	int next; // time left in note

	Slider note;
	ubyte currentPortStartCounter;
	ubyte noteLength;
	ubyte noteStyle;

	ubyte noteRelease; // time to release note, in cycles

	Parser parser;

	ubyte instrument; // instrument
	ADSRGain instrumentADSRGain;
	ubyte finetune;
	byte transpose;
	Slider panning = Slider(0x0A00);
	ubyte panFlags;
	Slider volume = Slider(0xFF00);
	ubyte totalVolume;
	byte leftVolume;
	byte rightVolume;

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
	ubyte vibratoStartCounter;
	ubyte currentVibratoRange;
	ubyte vibratoFadeInCounter;
	ubyte vibratoRangeDelta;
	ubyte tremoloPhase;
	ubyte tremoloStartCounter;

	Sample sample;
	int samplePosition = -1;
	int noteFrequency;

	short gain;
	ADSRPhase adsrPhase;
	ushort adsrCounter;
	ubyte adsrRate;

	short[8] interpolationBuffer;
	short lastSample;
	void setADSRPhase(ADSRPhase phase) @safe pure nothrow {
		adsrCounter = 0;
		adsrPhase = phase;
		final switch (phase) {
			case ADSRPhase.attack:
				adsrRate = cast(ubyte)(instrumentADSRGain.attackRate * 2 + 1);
				break;
			case ADSRPhase.decay:
				adsrRate = cast(ubyte)(instrumentADSRGain.decayRate * 2 + 16);
				break;
			case ADSRPhase.sustain:
				adsrRate = instrumentADSRGain.sustainRate;
				break;
			case ADSRPhase.gain:
				if (instrumentADSRGain.mode == ADSRGainMode.customGain) {
					adsrRate = instrumentADSRGain.gainRate;
				}
				assert(instrumentADSRGain.mode != ADSRGainMode.adsr);
				break;
			case ADSRPhase.release:
				adsrRate = 31;
				break;
		}
	}
}

private struct Parser {
	const(ubyte)[] sequenceData;
	/// the sequence data that will be restored upon return from subroutine
	const(ubyte)[] subroutineReturnData;
	Track[ushort] subroutines;
	/// Starting address of the subroutine
	ushort subroutineStartAddress;
	/// Number of times to repeat the subroutine
	ubyte subroutineCount;
	Variant variant;
	Command popCommand() nothrow @safe {
		bool _;
		return popCommand(_);
	}
	Command popCommand(out bool done) nothrow @safe {
		size_t nextOffset;
		const command = readCommand(variant, sequenceData, nextOffset);
		if (command.type == VCMDClass.terminator) {
			done = subroutineCount == 0;
			if (!done) {
				sequenceData = --subroutineCount ? subroutines[subroutineStartAddress].track : subroutineReturnData;
			}
		} else if ((command.type == VCMDClass.special) && (command.special == VCMD.subRoutine)) {
			subroutineReturnData = sequenceData[nextOffset .. $];
			subroutineStartAddress = read!ushort(command.parameters);
			subroutineCount = command.parameters[2];
			sequenceData = subroutines[subroutineStartAddress].track;
		} else {
			sequenceData = sequenceData[nextOffset .. $];
		}
		return command;
	}
}

// Rates (in samples) until next step is applied
private immutable adsrGainRates = [ 0, 2048, 1536, 1280, 1024, 768, 640, 512, 384, 320, 256, 192, 160, 128, 96, 80, 64, 48, 40, 32, 24, 20, 16, 12, 10, 8, 6, 5, 4, 3, 2, 1 ];

void doADSR(ref ChannelState channel) nothrow @safe {
	ushort level() {
		return cast(ushort)(((channel.gain - 1) >> 8) + 1);
	}
	final switch (channel.adsrPhase) {
		case ADSRPhase.attack:
			channel.gain += (channel.adsrRate == 31) ? 1024 : 32;
			if (channel.gain > 0x7E0) {
				channel.setADSRPhase(ADSRPhase.decay);
			}
			break;
		case ADSRPhase.decay:
			channel.gain -= level;
			if (channel.gain < channel.instrumentADSRGain.sustainLevel) {
				channel.setADSRPhase(ADSRPhase.sustain);
			}
			break;
		case ADSRPhase.sustain:
			channel.gain -= level;
			break;
		case ADSRPhase.gain:
			final switch(channel.instrumentADSRGain.gainMode) {
				case GainMode.linearDecreaseGain:
					channel.gain -= 32;
					break;
				case GainMode.expDecreaseGain:
					channel.gain -= level;
					break;
				case GainMode.linearIncreaseGain:
					channel.gain += 32;
					break;
				case GainMode.bentIncreaseGain:
					channel.gain +=  (channel.gain < 0x600) ? 32 : 8;
					break;
			}
			break;
		case ADSRPhase.release:
			channel.gain -= 8;
			if (channel.gain < 0) {
				channel.samplePosition = -1;
			}
			break;
	}
	channel.gain = clamp(channel.gain, cast(short)0, cast(short)0x7FF);
}

void doEcho(ref SongState state, ref short leftSample, ref short rightSample, int mixrate) nothrow @safe {
	const echoAddress = state.echoBufferIndex * 2;

	state.firLeft[state.firBufferIndex] = state.echoBuffer[echoAddress % state.echoBuffer.length] >> 1;

	state.firRight[state.firBufferIndex] = state.echoBuffer[(echoAddress + 1) % state.echoBuffer.length] >> 1;
	int sumLeft = 0;
	int sumRight = 0;
	for(int i = 0; i < 8; i++) {
		sumLeft += (state.firLeft[(state.firBufferIndex + i + 1) & 0x7] * state.firCoefficients[i]) >> 6;
		sumRight += (state.firRight[(state.firBufferIndex + i + 1) & 0x7] * state.firCoefficients[i]) >> 6;
	}
	sumLeft = clamp(sumLeft, short.min, short.max);
	sumRight = clamp(sumRight, short.min, short.max);

	leftSample = cast(short)clamp(leftSample + ((sumLeft * state.echoVolumeLeft) / 128.0), short.min, short.max);
	rightSample = cast(short)clamp(rightSample + ((sumRight * state.echoVolumeRight) / 128.0), short.min, short.max);

	int inLeft = 0;
	int inRight = 0;
	for(int i = 0; i < 8; i++) {
		if(state.echoOn & (1 << i)) {
			inLeft += cast(short)((state.channels[i].lastSample * state.channels[i].leftVolume) / 128.0);
			inRight += cast(short)((state.channels[i].lastSample * state.channels[i].rightVolume) / 128.0);
			inLeft = clamp(inLeft, short.min, short.max);
			inRight = clamp(inRight, short.min, short.max);
		}
	}
	inLeft += cast(int)((sumLeft * state.echoFeedbackVolume) / 128.0);
	inRight += cast(int)((sumRight * state.echoFeedbackVolume) / 128.0);
	inLeft = clamp(inLeft * nativeSamplingRate / mixrate, short.min, short.max);
	inRight = clamp(inRight * nativeSamplingRate / mixrate, short.min, short.max);
	inLeft &= 0xfffe;
	inRight &= 0xfffe;
	if(state.echoWrites) {
		state.echoBuffer[echoAddress] = cast(short)inLeft;
		state.echoBuffer[echoAddress + 1] = cast(short)inRight;
	}

	state.firBufferIndex = (state.firBufferIndex + 1) & 7;
	state.echoBufferIndex++;
	if(--state.echoRemaining == 0) {
		state.echoRemaining = state.echoDelay;
		state.echoBufferIndex = 0;
	}
}

///
struct NSPCPlayer {
	enum defaultSpeed = 500;

	package Song currentSong;
	private SongState state;
	private int mixrate = nativeSamplingRate;
	private int timerSpeed = defaultSpeed;
	private bool songPlaying;

	private bool loopEnabled = true;

	Interpolation interpolation = Interpolation.gaussian;
	///
	short[2][] fillBuffer(short[2][] buffer) nothrow @safe {
		enum left = 0;
		enum right = 1;
		if (!songPlaying) {
			return [];
		}
		size_t length;
		foreach (ref sample; buffer) {
			sample[] = 0;
			if ((state.nextTimerTick -= timerSpeed) < 0) {
				state.nextTimerTick += mixrate;
				if (!doTimer()) {
					break;
				}
			}
			length++;
			foreach (i, ref channel; state.channels) {
				if (!channel.enabled) {
					continue;
				}

				if (channel.samplePosition < 0) {
					continue;
				}

				int ipos = channel.samplePosition >> 15;

				foreach (idx, ref interpolationSample; channel.interpolationBuffer) {
					size_t offset = ipos + idx;
					while (channel.sample.loopLength && (offset >= channel.sample.data.length)) {
						offset -= channel.sample.loopLength;
					}
					if (offset < channel.sample.data.length) {
						interpolationSample = channel.sample.data[offset];
					} else {
						interpolationSample = channel.interpolationBuffer[idx - 1];
					}
				}
				int s1 = channel.lastSample = interpolate(interpolation, channel.interpolationBuffer[], channel.samplePosition >> 3);

				if (channel.adsrRate && (++channel.adsrCounter >= cast(int)(adsrGainRates[channel.adsrRate] * (mixrate / cast(double)nativeSamplingRate)))) {
					doADSR(channel);
					channel.adsrCounter = 0;
				}
				if (channel.gain == 0) {
					continue;
				}
				s1 = (s1 * channel.gain) >> 11;

				sample[left] += cast(int)(s1 * channel.leftVolume / 128.0);
				sample[right] += cast(int)(s1 * channel.rightVolume / 128.0);

				channel.samplePosition += channel.noteFrequency;
				if ((channel.samplePosition >> 15) >= channel.sample.data.length) {
					if (channel.sample.loopLength) {
						channel.samplePosition -= channel.sample.loopLength << 15;
					} else {
						channel.samplePosition = -1;
						channel.adsrPhase = ADSRPhase.release;
					}
				}
			}
			doEcho(state, sample[0], sample[1], mixrate);
		}
		return buffer[0 .. length];
	}

	private void calcTotalVolume(const SongState st, ref ChannelState c, byte tremoloPhase) nothrow @safe {
		ubyte v = (tremoloPhase << 1 ^ tremoloPhase >> 7) & 0xFF;
		v = ~(v * c.tremoloRange >> 8) & 0xFF;

		v = v * (st.volume.current >> 8) >> 8;
		v = v * currentSong.volumeTable[c.noteStyle & 15] >> 8;
		v = v * (c.volume.current >> 8) >> 8;
		c.totalVolume = v * v >> 8;
	}

	private int calcVolume3(const ChannelState c, int pan, int flag) nothrow @safe {
		static immutable ubyte[] panTable = [0x00, 0x01, 0x03, 0x07, 0x0D, 0x15, 0x1E, 0x29, 0x34, 0x42, 0x51, 0x5E, 0x67, 0x6E, 0x73, 0x77, 0x7A, 0x7C, 0x7D, 0x7E, 0x7F, 0x7F];
		const ubyte[] ph = panTable[pan >> 8 .. (pan >> 8) + 2];
		int v = ph[0] + ((ph[1] - ph[0]) * (pan & 255) >> 8);
		v = v * c.totalVolume >> 8;
		if (c.panFlags & flag) {
			v = -v;
		}
		return v;
	}

	private void calcVolume2(ref ChannelState c, int pan) nothrow @safe {
		c.leftVolume = cast(byte) calcVolume3(c, pan, 0x80);
		c.rightVolume = cast(byte) calcVolume3(c, 0x1400 - pan, 0x40);
	}

	private void makeSlider(ref Slider s, int cycles, int target) nothrow @safe {
		s.delta = cast(ushort)(((target << 8) - (s.current & 0xFF00)) / cycles);
		s.cycles = cast(ubyte) cycles;
		s.target = cast(ubyte) target;
	}

	private void setInstrument(ref SongState st, ref ChannelState c, size_t instrument) nothrow @safe {
		const idata = currentSong.instruments[instrument];
		c.instrument = cast(ubyte) instrument;
		c.instrumentADSRGain = idata.adsrGain;
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


		freq *= currentSong.instruments[c.instrument].tuning;
		freq >>= 8;
		freq &= 0x3fff;

		c.noteFrequency = (freq * (nativeSamplingRate << (15 - 12))) / mixrate;
	}

	private int calcVibratoDisp(ref ChannelState c, int phase) nothrow @safe {
		int range = c.currentVibratoRange;
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
				setInstrument(st, c, currentSong.absoluteInstrumentID(command.parameters[0], st.percussionBase, false));
				break;
			case VCMD.panning:
				c.panFlags = command.parameters[0];
				c.panning.current = (command.parameters[0] & 0x1F) << 8;
				break;
			case VCMD.panningFade:
				makeSlider(c.panning, command.parameters[0], command.parameters[1]);
				break;
			case VCMD.vibratoOn:
				c.vibratoStart = command.parameters[0];
				c.vibratoSpeed = command.parameters[1];
				c.currentVibratoRange = c.vibratoMaxRange = command.parameters[2];
				c.vibratoFadeIn = 0;
				break;
			case VCMD.vibratoOff:
				c.currentVibratoRange = c.vibratoMaxRange = 0;
				c.vibratoFadeIn = 0;
				break;
			case VCMD.songVolume:
				st.volume.current = command.parameters[0] << 8;
				break;
			case VCMD.songVolumeFade:
				makeSlider(st.volume, command.parameters[0], command.parameters[1]);
				break;
			case VCMD.tempo:
				st.tempo.current = command.parameters[0] << 8;
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
				c.volume.current = command.parameters[0] << 8;
				break;
			case VCMD.volumeFade:
				makeSlider(c.volume, command.parameters[0], command.parameters[1]);
				break;
			case VCMD.subRoutine:
				/// This is handled by the parser
				break;
			case VCMD.vibratoFadeIn:
				c.vibratoFadeIn = command.parameters[0];
				c.vibratoRangeDelta = c.currentVibratoRange / command.parameters[0];
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
				st.echoOn = command.parameters[0];
				st.echoVolumeLeft = command.parameters[1];
				st.echoVolumeRight = command.parameters[2];
				break;
			case VCMD.echoOff:
				st.echoOn = 0;
				break;
			case VCMD.echoParameterSetup:
				st.echoDelay = command.parameters[0];
				st.echoFeedbackVolume = command.parameters[1];
				st.firCoefficients = currentSong.firCoefficients[command.parameters[2]];
				break;
			case VCMD.echoVolumeFade:
				st.fadeTicks = command.parameters[0];
				st.targetEchoVolumeLeft = command.parameters[1];
				st.targetEchoVolumeRight = command.parameters[2];
				break;
			case VCMD.noop: //do nothing
				break;
			case VCMD.pitchSlideToNote:
				c.currentPortStartCounter = command.parameters[0];
				int target = command.parameters[2] + st.transpose;
				if (target >= 0x100) {
					target -= 0xFF;
				}
				target += c.transpose;
				makeSlider(c.note, command.parameters[1], target & 0x7F);
				break;
			case VCMD.percussionBaseInstrumentRedefine:
				st.percussionBase = command.parameters[0];
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
			setInstrument(st, c, currentSong.absoluteInstrumentID(note, st.percussionBase, true));
			note = cast(ubyte)(currentSong.percussionNotes[note]);
		}
		if (command.type.among(VCMDClass.percussion, VCMDClass.note)) {
			c.vibratoPhase = c.vibratoFadeIn & 1 ? 0x80 : 0;
			c.vibratoStartCounter = 0;
			c.vibratoFadeInCounter = 0;
			c.tremoloPhase = 0;
			c.tremoloStartCounter = 0;

			c.samplePosition = 0;
			c.sample = currentSong.samples[currentSong.instruments[c.instrument].sampleID];
			c.gain = 0;
			c.setADSRPhase((currentSong.instruments[c.instrument].adsrGain.mode == ADSRGainMode.adsr) ? ADSRPhase.attack : ADSRPhase.gain);

			note += st.transpose + c.transpose;
			c.note.current = cast(ushort)(note << 8 | c.finetune);

			c.note.cycles = c.portLength;
			if (c.note.cycles) {
				int target = note;
				c.currentPortStartCounter = c.portStart;
				if (c.portType == 0) {
					c.note.current -= c.portRange << 8;
				} else {
					target += c.portRange;
				}
				makeSlider(c.note, c.portLength, target & 0x7F);
			}

			setFrequency(c, c.note.current);
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
			rel = c.noteLength;
		} else {
			rel = (c.noteLength * currentSong.releaseTable[c.noteStyle >> 4]) >> 8;
			if (rel > c.noteLength - 2) {
				rel = c.noteLength - 2;
			}
			if (rel < 1) {
				rel = 1;
			}
		}
		c.noteRelease = cast(ubyte) rel;
	}

	private void loadPattern() nothrow @safe {
		state.phraseCounter++;
		const nextPhrase = currentSong.order[state.phraseCounter];
		debug(nspclogging) tracef("Next phrase: %s", nextPhrase);
		final switch (nextPhrase.type) {
			case PhraseType.end:
				state.phraseCounter--;
				songPlaying = false;
				return;
			case PhraseType.jumpLimited:
				if (--state.repeatCount >= 0x80) {
					state.repeatCount = cast(ubyte)nextPhrase.jumpTimes;
				}
				if (state.repeatCount > 0) {
					state.phraseCounter = nextPhrase.id - 1;
				}
				debug(nspclogging) tracef("%s more repeats", state.repeatCount);
				loadPattern();
				break;
			case PhraseType.jump:
				if (loopEnabled) {
					state.phraseCounter = nextPhrase.id - 1;
					loadPattern();
				} else {
					state.phraseCounter--;
					songPlaying = false;
				}
				break;
			case PhraseType.fastForwardOn:
				assert(0, "Not yet implemented");
			case PhraseType.fastForwardOff:
				assert(0, "Not yet implemented");
			case PhraseType.pattern:
				foreach (idx, ref channel; state.channels) {
					channel.parser.sequenceData = currentSong.pattern[nextPhrase.id][idx].track;
					channel.parser.subroutineCount = 0;
					channel.volume.cycles = 0;
					channel.panning.cycles = 0;
					channel.next = 0;
				}
				break;
		}
	}

	private void doKeySweepVibratoChecks(ref ChannelState c) nothrow @safe {
		// key off
		if (c.noteRelease) {
			c.noteRelease--;
		}
		if (!c.noteRelease) {
			c.setADSRPhase(ADSRPhase.release);
		}

		// sweep
		if (c.note.cycles) {
			if (c.currentPortStartCounter == 0) {
				c.note.slide();
				setFrequency(c, c.note.current);
			} else {
				c.currentPortStartCounter--;
			}
		}

		// vibrato
		if (c.currentVibratoRange) {
			if (c.vibratoStartCounter == c.vibratoStart) {
				int range;
				if (c.vibratoFadeInCounter == c.vibratoFadeIn) {
					range = c.vibratoMaxRange;
				} else {
					range = c.currentVibratoRange;
					if (c.vibratoFadeInCounter == 0) {
						range = 0;
					}
					range += c.vibratoRangeDelta;
					c.vibratoFadeInCounter++;
				} // DA0
				c.currentVibratoRange = cast(ubyte) range;
				c.vibratoPhase += c.vibratoSpeed;
				setFrequency(c, c.note.current + calcVibratoDisp(c, c.vibratoPhase));
			} else {
				c.vibratoStartCounter++;
			}
		}
	}

	// $07F9 + $0625
	private bool doCycle(ref SongState st) nothrow @safe {
		foreach (ref channel; st.channels) {
			if (channel.parser.sequenceData == null) {
				continue; //8F0
			}

			if (--channel.next >= 0) {
				doKeySweepVibratoChecks(channel);
			} else {
				loop: while (1) {
					bool done;
					const command = channel.parser.popCommand(done);
					final switch (command.type) {
						case VCMDClass.terminator:
							if (done) {
								return false;
							}
							break;
						case VCMDClass.noteDuration:
							channel.noteLength = command.noteDuration;
							if (command.parameters.length > 0) {
								channel.noteStyle = command.parameters[0];
							}
							break;
						case VCMDClass.note:
						case VCMDClass.tie:
						case VCMDClass.rest:
						case VCMDClass.percussion:
							channel.next = channel.noteLength - 1;
							doNote(st, channel, command);
							break loop;
						case VCMDClass.special:
							doCommand(st, channel, command);
							break;
					}
				}
			}
			// $0B84
			if (channel.note.cycles == 0) {
				size_t length;
				const command = readCommand(currentSong.variant, channel.parser.sequenceData, length);
				if (command.special == VCMD.pitchSlideToNote) {
					doCommand(st, channel, command);
					channel.parser.popCommand();
				}
			}
		}

		st.tempo.slide();
		st.volume.slide();

		foreach (ref channel; st.channels) {
			if (channel.parser.sequenceData == null) {
				continue;
			}

			// @ 0C40
			channel.volume.slide();

			// @ 0C4D
			int tphase = 0;
			if (channel.tremoloRange) {
				if (channel.tremoloStartCounter == channel.tremoloStart) {
					if (channel.tremoloPhase >= 0x80 && channel.tremoloRange == 0xFF) {
						channel.tremoloPhase = 0x80;
					} else {
						channel.tremoloPhase += channel.tremoloSpeed;
					}
					tphase = channel.tremoloPhase;
				} else {
					channel.tremoloStartCounter++;
				}
			}
			calcTotalVolume(st, channel, cast(byte) tphase);

			// 0C79
			channel.panning.slide();

			// 0C86: volume stuff
			calcVolume2(channel, channel.panning.current);
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
		foreach (ref channel; st.channels) {
			if (channel.parser.sequenceData == null) {
				continue;
			}
			// $0DD0

			bool changed = false;
			if (channel.tremoloRange && channel.tremoloStartCounter == channel.tremoloStart) {
				int p = channel.tremoloPhase + subCycleCalc(st, channel.tremoloSpeed);
				changed = true;
				calcTotalVolume(st, channel, cast(byte) p);
			}
			int pan = channel.panning.current;
			if (channel.panning.cycles) {
				pan += subCycleCalc(st, channel.panning.delta);
				changed = true;
			}
			if (changed) {
				calcVolume2(channel, pan);
			}

			changed = false;
			int note = channel.note.current; // $0BBC
			if (channel.note.cycles && channel.currentPortStartCounter == 0) {
				note += subCycleCalc(st, channel.note.delta);
				changed = true;
			}
			if (channel.currentVibratoRange && channel.vibratoStartCounter == channel.vibratoStart) {
				int p = channel.vibratoPhase + subCycleCalc(st, channel.vibratoSpeed);
				note += calcVibratoDisp(channel, p);
				changed = true;
			}
			if (changed) {
				setFrequency(channel, note);
			}
		}
	}

	private bool doTimer() nothrow @safe {
		state.cycleTimer += state.tempo.current >> 8;
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
		foreach (idx, ref channel; state.channels) {
			channel.parser.variant = currentSong.variant;
			channel.parser.subroutines = currentSong.subroutines;
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
	void loadSong(Song song) {
		if (songPlaying) {
			stop();
		}
		currentSong = song;
		state = state.init;
		state.percussionBase = cast(ubyte)song.percussionBase;
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
		state.channels[channel].enabled = enabled;
	}
	bool isPlaying() const pure @safe nothrow {
		return songPlaying;
	}
	const(Sample)[] getSamples() const pure @safe nothrow return {
		import std.algorithm.iteration : filter;
		import std.array : array;
		return currentSong.samples[].filter!(x => x.isValid).array;
	}
	void replaceSample(size_t index, short[] data, int newLoop) @safe pure nothrow {
		currentSong.samples[index].data = data;
		currentSong.samples[index].loopLength = newLoop;
	}
	void replaceSample(size_t index, short[] data) @safe pure nothrow {
		replaceSample(index, data, currentSong.samples[index].loopLength);
	}
}

private bool inRange(T)(T val, T lower, T upper) {
	return ((val >= lower) && (val <= upper));
}
