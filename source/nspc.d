module nspc;

import core.stdc.math;
import core.stdc.stdlib;
import core.stdc.string;
import core.memory;
import std.algorithm.comparison;
import std.exception;
import std.experimental.logger;
import std.format;

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

private struct ChannelState {
	ubyte[] ptr;

	int next; // time left in note

	Slider note;
	ubyte curPortStartCtr;
	ubyte noteLen;
	ubyte noteStyle;

	ubyte noteRelease; // time to release note, in cycles

	int subStart; // current subroutine number
	ubyte[] subRet; // where to return to after sub
	ubyte subCount; // number of loops

	ubyte inst; // instrument
	ubyte instADSR1;
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

	double envelopeHeight = 0.0;
	double decayRate = 0.0;
}

private struct Sample {
	short[] data;
	int loopLength;
}

private struct Parser {
	ubyte[] ptr;
	ubyte[] subRet;
	int subStart;
	ubyte subCount;
	ubyte noteLen;
}

private struct Song {
	ushort address;
	ubyte changed;
	Phrase[] order;
	Track[8][] pattern;
	Track[] sub;
}

private struct Track {
	int size;
	ubyte[] track; // null for inactive track
}

// note style tables, from 6F80
private immutable ubyte[8] releaseTable = [0x33, 0x66, 0x7f, 0x99, 0xb2, 0xcc, 0xe5, 0xfc];
private immutable ubyte[16] volumeTable = [0x19, 0x33, 0x4c, 0x66, 0x72, 0x7f, 0x8c, 0x99, 0xa5, 0xb2, 0xbf, 0xcc, 0xd8, 0xe5, 0xf2, 0xfc];

// number of bytes following a Ex/Fx code
private immutable ubyte[32] codeLength = [1, 1, 2, 3, 0, 1, 2, 1, 2, 1, 1, 3, 0, 1, 2, 3, 1, 3, 3, 0, 1, 3, 0, 3, 3, 3, 1, 2, 0, 0, 0, 0];

private enum brrBlockSize = 9;
private enum brrFlagEnd = 1;
private enum brrFlagLoop = 2;

private struct Instrument {
	align(1):
	ubyte sampleID;
	ubyte adsr0;
	ubyte adsr1;
	ubyte gain;
	ubyte tuning;
	ubyte tuningFraction;
}

enum PhraseType {
	pattern,
	jumpLimited,
	jump,
	end,
	fastForwardOn,
	fastForwardOff,
}

private struct Phrase {
	PhraseType type;
	ushort id;
	ushort jumpTimes;
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
	/// Which version of NSPC to use
	uint variant;
	/// Base SPC address of the song's sequence data
	ushort songBase;
	/// Base SPC address of the instruments
	ushort instrumentBase;
	/// Base SPC address of the samples
	ushort sampleBase;
	/// Reserved for future usage
	ubyte[22] reserved;
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
	///
	void fillBuffer(short[2][] buffer) nothrow @safe {
		if (!songPlaying) {
			return;
		}
		foreach (ref sample; buffer) {
			if ((state.nextTimerTick -= timerSpeed) < 0) {
				state.nextTimerTick += mixrate;
				if (!doTimer()) {
					break;
				}
			}

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

				if (chan.noteRelease != 0) {
					if (chan.instADSR1 & 0x1F) {
						chan.envelopeHeight *= chan.decayRate;
					}
				} else {
					// release takes about 15ms (not dependent on tempo)
					chan.envelopeHeight -= (32000 / 512.0) / mixrate;
					if (chan.envelopeHeight < 0) {
						chan.sampPos = -1;
						continue;
					}
				}
				double volume = chan.envelopeHeight / 128.0;
				assert(chan.samp.data);
				int s1 = chan.samp.data[ipos];
				int s2 = (ipos + 1 == chan.samp.data.length) ? s1 : chan.samp.data[ipos + 1];
				s1 += (s2 - s1) * (chan.sampPos & 0x7FFF) >> 15;

				left += cast(int)(s1 * chan.leftVol * volume);
				right += cast(int)(s1 * chan.rightVol * volume);

				chan.sampPos += chan.noteFrequency;
				if ((chan.sampPos >> 15) >= chan.samp.data.length) {
					if (chan.samp.loopLength) {
						chan.sampPos -= chan.samp.loopLength << 15;
					} else {
						chan.sampPos = -1;
					}
				}
			}
			left = clamp(left, short.min, short.max);
			right = clamp(right, short.min, short.max);
			sample[0] = cast(short) left;
			sample[1] = cast(short) right;
		}
	}

	private void parserInit(ref Parser p, ChannelState c) nothrow @safe {
		p.ptr = c.ptr;
		p.subStart = c.subStart;
		p.subRet = c.subRet;
		p.subCount = c.subCount;
		p.noteLen = c.noteLen;
	}

	private inout(ubyte)[] nextCode(return scope inout(ubyte)[] p) nothrow @safe {
		ubyte chr = p[0];
		p = p[1 .. $];
		if (chr < 0x80) {
			p = p[p[0] < 0x80 .. $];
		} else if (chr >= 0xE0) {
			p = p[codeLength[chr - 0xE0] .. $];
		}
		return p;
	}

	private bool parserAdvance(ref Parser p) nothrow @safe {
		int chr = p.ptr[0];
		if (chr == 0) {
			if (p.subCount == 0) {
				return false;
			}
			p.ptr = --p.subCount ? currentSong.sub[p.subStart].track : p.subRet;
		} else if (chr == 0xEF) {
			p.subRet = p.ptr[4 .. $];
			p.subStart = (cast(ushort[])p.ptr[1 .. 3])[0];
			p.subCount = p.ptr[3];
			p.ptr = currentSong.sub[p.subStart].track;
		} else {
			if (chr < 0x80) {
				p.noteLen = cast(ubyte) chr;
			}
			p.ptr = nextCode(p.ptr);
		}
		return true;
	}

	private void calcTotalVolume(const SongState st, ref ChannelState c, byte tremoloPhase) nothrow @safe {
		ubyte v = (tremoloPhase << 1 ^ tremoloPhase >> 7) & 0xFF;
		v = ~(v * c.tremoloRange >> 8) & 0xFF;

		v = v * (st.volume.cur >> 8) >> 8;
		v = v * volumeTable[c.noteStyle & 15] >> 8;
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

	private void setInstrument(ref SongState st, ref ChannelState c, uint inst) nothrow @safe {
		// CA and up is for instruments in the second pack (set with FA xx)
		if (inst >= 0x80) {
			inst += st.firstCAInst - 0xCA;
		}

		if (inst >= instruments.length) {
			assert(0, format!"instrument %s out of bounds!"(inst));
		}
		const idata = instruments[inst];
		if (!samp[idata.sampleID].data) {
			assert(0, format!"no data for instrument %s"(inst));
		}
		if ((idata.tuning == 0) && (idata.tuningFraction == 0)) {
			assert(0, format!"bad instrument %s"(inst));
		}

		c.inst = cast(ubyte) inst;
		c.instADSR1 = idata.adsr1;
		if (c.instADSR1 & 0x1F) {
			int i = c.instADSR1 & 0x1F;
			// calculate the constant to multiply envelope height by on each sample
			int halflife;
			if (i >= 30) {
				halflife = 32 - i;
			} else {
				halflife = ((512 >> (i / 3)) * (5 - i % 3));
			}
			c.decayRate = pow(2.0, -1.0 / (0.0055 * halflife * mixrate));
		}
	}

	// calculate how far to advance the sample pointer on each output sample
	private void calcFrequency(ref ChannelState c, int note16) nothrow @safe {
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


		freq *= (cast(ushort)instruments[c.inst].tuning << 8) + instruments[c.inst].tuningFraction;
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
	private void doCommand(ref SongState st, ref ChannelState c) nothrow @safe {
		const p = c.ptr;
		c.ptr = c.ptr[1 + codeLength[p[0] - 0xE0] .. $];
		switch (p[0]) {
			case 0xE0:
				setInstrument(st, c, p[1]);
				break;
			case 0xE1:
				c.panFlags = p[1];
				c.panning.cur = (p[1] & 0x1F) << 8;
				break;
			case 0xE2:
				makeSlider(c.panning, p[1], p[2]);
				break;
			case 0xE3:
				c.vibratoStart = p[1];
				c.vibratoSpeed = p[2];
				c.curVibRange = c.vibratoMaxRange = p[3];
				c.vibratoFadeIn = 0;
				break;
			case 0xE4:
				c.curVibRange = c.vibratoMaxRange = 0;
				c.vibratoFadeIn = 0;
				break;
			case 0xE5:
				st.volume.cur = p[1] << 8;
				break;
			case 0xE6:
				makeSlider(st.volume, p[1], p[2]);
				break;
			case 0xE7:
				st.tempo.cur = p[1] << 8;
				break;
			case 0xE8:
				makeSlider(st.tempo, p[1], p[2]);
				break;
			case 0xE9:
				st.transpose = p[1];
				break;
			case 0xEA:
				c.transpose = p[1];
				break;
			case 0xEB:
				c.tremoloStart = p[1];
				c.tremoloSpeed = p[2];
				c.tremoloRange = p[3];
				break;
			case 0xEC:
				c.tremoloRange = 0;
				break;
			case 0xED:
				c.volume.cur = p[1] << 8;
				break;
			case 0xEE:
				makeSlider(c.volume, p[1], p[2]);
				break;
			case 0xEF:
				c.subStart = p[1] | (p[2] << 8);
				c.subRet = c.ptr;
				c.subCount = p[3];
				c.ptr = currentSong.sub[c.subStart].track;
				break;
			case 0xF0:
				c.vibratoFadeIn = p[1];
				c.vibratoRangeDelta = c.curVibRange / p[1];
				break;
			case 0xF1:
			case 0xF2:
				c.portType = p[0] & 1;
				c.portStart = p[1];
				c.portLength = p[2];
				c.portRange = p[3];
				break;
			case 0xF3:
				c.portLength = 0;
				break;
			case 0xF4:
				c.finetune = p[1];
				break;
			case 0xF9: {
					c.curPortStartCtr = p[1];
					int target = p[3] + st.transpose;
					if (target >= 0x100) {
						target -= 0xFF;
					}
					target += c.transpose;
					makeSlider(c.note, p[2], target & 0x7F);
					break;
				}
			case 0xFA:
				st.firstCAInst = p[1];
				break;
			default:
				break;
		}
	}

	// $0654 + $08D4-$8EF
	private void doNote(ref SongState st, ref ChannelState c, int note) nothrow @safe {
		// using >=CA as a note switches to that instrument and plays note A4
		if (note >= 0xCA) {
			setInstrument(st, c, note);
			note = 0xA4;
		}

		if (note < 0xC8) {
			c.vibratoPhase = c.vibratoFadeIn & 1 ? 0x80 : 0;
			c.vibratoStartCtr = 0;
			c.vibratoFadeInCtr = 0;
			c.tremoloPhase = 0;
			c.tremoloStartCtr = 0;

			c.sampPos = 0;
			c.samp = samp[instruments[c.inst].sampleID];
			c.envelopeHeight = 1;

			note &= 0x7F;
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

			calcFrequency(c, c.note.cur);
		}

		// Search forward for the next note (to see if it's C8). This is annoying
		// but necessary - C8 can continue the last note of a subroutine as well
		// as a normal note.
		int nextNote;
		{
			Parser p;
			parserInit(p, c);
			do {
				if (p.ptr[0] >= 0x80 && p.ptr[0] < 0xE0) {
					break;
				}
			} while (parserAdvance(p));
			nextNote = p.ptr[0];
		}

		int rel;
		if (nextNote == 0xC8) {
			// if the note will be continued, don't release yet
			rel = c.noteLen;
		} else {
			rel = (c.noteLen * releaseTable[c.noteStyle >> 4]) >> 8;
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
		debug tracef("Now playing %s", nextPhrase);
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
				debug tracef("%s more repeats", state.repeatCount);
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
					channel.ptr = currentSong.pattern[nextPhrase.id][idx].track;
					channel.subCount = 0;
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

		// 0D60
		if (c.note.cycles) {
			if (c.curPortStartCtr == 0) {
				c.note.slide();
				calcFrequency(c, c.note.cur);
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
				calcFrequency(c, c.note.cur + calcVibratoDisp(c, c.vibratoPhase));
			} else {
				c.vibratoStartCtr++;
			}
		}
	}

	// $07F9 + $0625
	private bool doCycle(ref SongState st) nothrow @safe {
		foreach (ref c; st.chan) {
			if (c.ptr == null) {
				continue; //8F0
			}

			if (--c.next >= 0) {
				CF7(c);
			} else
				while (1) {
					const ubyte[] p = c.ptr[0 .. $];

					if (p[0] == 0) { // end of sub or pattern
						if (c.subCount) { // end of sub
							c.ptr = --c.subCount ? currentSong.sub[c.subStart].track : c.subRet;
						} else {
							return false;
						}
					} else if (p[0] < 0x80) {
						c.noteLen = p[0];
						if (p[1] < 0x80) {
							c.noteStyle = p[1];
							c.ptr = c.ptr[2 .. $];
						} else {
							c.ptr = c.ptr[1 .. $];
						}
					} else if (p[0] < 0xE0) {
						c.ptr = c.ptr[1 .. $];
						c.next = c.noteLen - 1;
						doNote(st, c, p[0]);
						break;
					} else { // E0-FF
						doCommand(st, c);
					}
				}
			// $0B84
			if (c.note.cycles == 0 && c.ptr[0] == 0xF9) {
				doCommand(st, c);
			}
		}

		st.patpos++;

		st.tempo.slide();
		st.volume.slide();

		foreach (ref c; st.chan) {
			if (c.ptr == null) {
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
			if (c.ptr == null) {
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
				calcFrequency(c, note);
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
		mixrate = sampleRate;
	}

	/// Start playing music
	void play() @safe {
		initialize(mixrate);
		songPlaying = true;
	}
	void stop() @safe {
		songPlaying = false;
	}
	/// Load a single sequence pack at a given address
	void loadSequencePack(const(ubyte)[] data, ushort base) @safe {
		loadSequence(data, base);
	}
	/// Load a single sequence pack, automatically detecting the address from the pack header
	void loadSequencePack(const(ubyte)[] data) @safe {
		ushort base = (cast(const(ushort)[])(data[2 .. 4]))[0];
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
			loadAllSubpacks(buffer[], pack);
		}
		processInstruments(buffer, instrumentBase, sampleBase);
	}
	private void loadAllSubpacks(scope ubyte[] buffer, const(ubyte)[] pack) @safe {
		ushort size, base;
		while (true) {
			if (pack.length == 0) {
				break;
			}
			size = (cast(const(ushort)[])(pack[0 .. 2]))[0];
			if (size == 0) {
				break;
			}
			base = (cast(const(ushort)[])(pack[2 .. 4]))[0];
			tracef("Loading subpack to %X (%s bytes)", base, size);
			enforce!NSPCException(base + size <= ushort.max, "Invalid pack - base + size exceeds 64KB memory limit");
			buffer[base .. base + size] = pack[4 .. size + 4];
			pack = pack[size + 4 .. $];
		}
	}

	/// Load an NSPC file
	void loadNSPCFile(const(ubyte)[] data) @safe {
		ubyte[65536] buffer;
		auto header = (cast(const(NSPCFileHeader)[])(data[0 .. NSPCFileHeader.sizeof]))[0];
		tracef("Loading NSPC - so: %X, i: %X, sa: %X", header.songBase, header.instrumentBase, header.sampleBase);
		loadAllSubpacks(buffer[], data[NSPCFileHeader.sizeof .. $]);
		processInstruments(buffer, header.instrumentBase, header.sampleBase);
		decompileSong(buffer[], currentSong, header.songBase, buffer.length - 1);
	}

	private void processInstruments(scope ubyte[] buffer, ushort instrumentBase, ushort sampleBase) @safe {
		decodeSamples(buffer, cast(ushort[2][])(buffer[sampleBase .. sampleBase + maxSampleCount * 4]));
		instruments.reserve(maxInstruments);
		foreach (idx, instrument; cast(Instrument[])(buffer[instrumentBase .. instrumentBase + maxInstruments * Instrument.sizeof])) {
			instruments ~= instrument;
			tracef("%s. %s", idx, instrument);
		}
	}

	private void decompileSong(scope ubyte[] data, ref Song song, int startAddress, int endAddress) @safe {
		ushort[] subTable;
		int firstPattern;
		int tracksStart;
		int tracksEnd;
		int patBytes;
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
		firstPattern = startAddress + index * 2;

		const phrases = wpO[0 .. index];
		// Now the number of patterns is known, so go back and get the order
		size_t idx;
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
				int pat = phrases[idx] - firstPattern;
				enforce!NSPCException(!(pat & 15), format!"Bad pattern pointer: %x"(pat + firstPattern));
				order.id = cast(ushort)(pat >> 4);
			}
			idx++;
		}
		// locate first track
		if (index >= wpO.length) {
			// no tracks in the song
			tracksStart = endAddress - 1;
		} else {
			tracksStart = wpO[index];
		}
		patBytes = tracksStart - firstPattern;
		enforce!NSPCException(patBytes > 0, format!"Bad first track pointer: %x"(tracksStart));

		if (startAddress + index * 2 + 1 >= endAddress) {
			// no tracks in the song
			tracksEnd = endAddress - 1;
		} else {
			// find the last track
			int tp, tpp = tracksStart;
			while ((tp = read!ushort(data, tpp -= 2)) == 0) {}

			enforce!NSPCException(tp.inRange(tracksStart, endAddress - 1), format!"Bad last track pointer: %x"(tp));

			// is the last track the first one in its pattern?
			bool first = true;
			int chan = (tpp - firstPattern) >> 1 & 7;
			for (; chan; chan--) {
				first &= read!ushort(data, tpp -= 2) == 0;
			}

			const(ubyte)[] end = data[tp .. endAddress];
			while (end[0]) {
				end = nextCode(end);
			}
			end = end[first .. $];
			tracksEnd = cast(ushort)(data.length - end.length - 1);
		}

		tracef("Phrases: %(%s, %)", song.order);
		validatePhrases();

		subTable = null;
		song.pattern = new Track[8][](patBytes >> 4);
		song.sub = null;

		index = fpIndex;
		for (int trk = 0; trk < song.pattern.length * 8; trk++) {
			Track* t = &song.pattern[trk / 8][trk % 8];
			int start = wpO[index++];
			if (start == 0) {
				continue;
			}
			enforce!NSPCException(start.inRange(tracksStart, tracksEnd - 1), format!"Bad track pointer: %x"(start));

			// Go through track list (patterns) and find first track that has an address higher than us.
			// If we find a track after us, we'll assume that this track doesn't overlap with that one.
			// If we don't find one, then next will remain at 0x10000 and we will search until the
			// end of memory to find a 00 byte to terminate the track.
			int next = 0x10000; // offset of following track
			for (int trackIndex = 0; trackIndex < (song.pattern.length * 8); trackIndex += 1) {
				int trackAddress = wpO[fpIndex + trackIndex];
				if (trackAddress < next && trackAddress > start) {
					next = trackAddress;
				}
			}
			// Determine the end of the track.
			const(ubyte)[] trackEnd = data[start .. next];
			while (trackEnd.length > 0 && trackEnd[0] != 0) {
				trackEnd = nextCode(trackEnd);
			}

			t.size = cast(int)(next - start - trackEnd.length);
			t.track = new ubyte[](t.size + 1);
			t.track[0 .. t.size] = data[start .. start + t.size];
			t.track[t.size] = 0;

			for (ubyte[] p = t.track[0 .. t.size]; p.length > 0; p = nextCode(p)) {
				if (p[0] != 0xEF) {
					continue;
				}
				int subPtr = (cast(const(ushort)[])(p[1 .. 3]))[0];
				int subEntry;

				// find existing entry in subTable
				for (subEntry = 0; subEntry < song.sub.length && subTable[subEntry] != subPtr; subEntry++) {
				}
				if (subEntry == song.sub.length) {
					// subEntry doesn't already exist in subTable; create it
					song.sub.length++;

					subTable = new ushort[](song.sub.length);
					subTable[subEntry] = cast(ushort) subPtr;

					Track* st = &song.sub[subEntry];

					ubyte[] substart = data[subPtr .. $];
					const(ubyte)[] subend = substart;
					while (subend[0] != 0) {
						subend = nextCode(subend);
					}
					st.size = cast(int)(&subend[0] - &substart[0]);
					st.track = new ubyte[](st.size + 1);
					st.track[0 .. st.size + 1] = substart[0 .. st.size + 1];
					validateTrack(st.track[0 .. st.size], true);
				}
				(cast(ushort[])(p[1 .. 3]))[0] = cast(ushort) subEntry;
			}
			validateTrack(t.track[0 .. t.size], false);
		}
	}

	private void decodeSamples(scope ubyte[] buffer, const scope ushort[2][] ptrtable) nothrow @safe {
		for (uint sn = 0; sn < 128; sn++) {
			const start = ptrtable[sn][0];
			const loop = ptrtable[sn][1];

			samp[sn].data = null;
			if (start == 0 || start == 0xffff) {
				continue;
			}

			int length = sampleLength(buffer, start);
			if (length == -1) {
				continue;
			}

			int end = start + length;
			// The LOOP bit only matters for the last brr block
			if (buffer[start + length - brrBlockSize] & brrFlagLoop) {
				if (loop < start || loop >= end) {
					continue;
				}
				samp[sn].loopLength = ((end - loop) / brrBlockSize) * 16;
			} else
				samp[sn].loopLength = 0;

			samp[sn].data = new short[]((length / brrBlockSize) * 16);
			short[] p = samp[sn].data;

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

				if (samp[sn].loopLength != 0) {
					decodingStart = loop;

					short[16] afterLoop;

					decodeBRRBlock(afterLoop, pExtra, buffer[loop .. loop + brrBlockSize]);
					int fullLoopLength = getFullLoopLength(samp[sn], afterLoop[0 .. 2], (loop - start) / brrBlockSize * 16);

					if (fullLoopLength == -1) {
						needsAnotherLoop = true;
						const diff = samp[sn].data.length;
						idx = 0;
						samp[sn].data.length += samp[sn].loopLength;
						p = samp[sn].data[diff .. $];
					} else {
						samp[sn].loopLength = fullLoopLength;
						// needsAnotherLoop is already false
					}
				}

				// In the vanilla game, the most iterations needed is 48 (for sample 0x17 in pack 5).
				// Most samples need less than 10.
				++times;
			} while (needsAnotherLoop && times < 128);

			assert(!needsAnotherLoop, "Sample took too many iterations to get into a cycle");
		}
	}

	private void validateTrack(ubyte[] data, bool isSub) @safe {
		ubyte percussionBase;
		for (int pos = 0; pos < data.length;) {
			int byte_ = data[pos];
			int next = pos + 1;

			if (byte_ < 0x80) {
				enforce!NSPCException(byte_ != 0, "Track can not contain [00]");
				if (next != data.length && data[next] < 0x80) {
					next++;
				}
				enforce!NSPCException(next != data.length, "Track can not end with note-length code");
			} else if ((byte_ >= 0xCA) && (byte_ < 0xE0)) {
				validateInstrument(byte_ - 0xCA, 0);
			} else if (byte_ >= 0xE0) {
				enforce!NSPCException(byte_ != 0xFF, "Invalid code [FF]");
				next += codeLength[byte_ - 0xE0];
				enforce!NSPCException(next <= data.length, format!"Incomplete code: [%(%02X %)]"(data[pos .. pos + data.length]));

				if (byte_ == 0xE0) {
					validateInstrument(data[pos + 1], percussionBase);
				}
				if (byte_ == 0xEF) {
					enforce!NSPCException(!isSub, "Can't call sub from within a sub");
					int sub = (cast(ushort[]) data[pos + 1 .. pos + 3])[0];
					enforce!NSPCException(sub < currentSong.sub.length, format!"Subroutine %d not present"(sub));
					enforce!NSPCException(data[pos + 3] != 0, "Subroutine loop count can not be 0");
				}
				if (byte_ == 0xFA) {
					percussionBase = data[pos + 1];
				}
			}

			pos = next;
		}
	}
	private void validatePhrases() const @safe {
		bool endFound;
		foreach (phrase; currentSong.order) {
			enforce!NSPCException(!endFound, "Phrases found after end of song");
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
	private void validateInstrument(size_t id, size_t base) const @safe {
		if (id >= 0x80) {
			id += base - 0xCA;
		}
		enforce!NSPCException(id <instruments.length, format!"instrument %s out of bounds!"(id));
		const idata = instruments[id];
		enforce!NSPCException(samp[idata.sampleID].data, format!"no data for instrument %s"(id));
		enforce!NSPCException((idata.tuning != 0) || (idata.tuningFraction != 0), format!"bad instrument %s"(id));
	}
	/// Sets the playback speed. Default value is NSPCPlayer.defaultSpeed.
	public void setSpeed(ushort rate) @safe {
		timerSpeed = rate;
	}
	/// Enable or disable song looping
	public void looping(bool enabled) @safe {
		loopEnabled = enabled;
	}
	/// Enable or disable a song channel
	public void setChannelEnabled(ubyte channel, bool enabled) @safe {
		const newChmask = 1 << channel;
		if (enabled) {
			chmask |= newChmask;
		} else {
			chmask &= ~newChmask;
		}
	}
}

private void decodeBRRBlock(scope short[] buffer, short[2] lastSamples, const scope ubyte[] block) nothrow @safe {
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

private T read(T)(const(ubyte)[] data, size_t offset) {
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
