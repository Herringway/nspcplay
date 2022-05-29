module nspc;

import core.stdc.math;
import core.stdc.stdlib;
import core.stdc.string;
import std.format;
import std.experimental.allocator;
struct SongState {
	ChannelState[16] chan;
	byte transpose;
	Slider volume;
	Slider tempo;
	int next_timer_tick, cycle_timer;
	ubyte first_CA_inst; // set with FA
	ubyte repeat_count;
	int ordnum;
	int patpos; // Number of cycles since top of pattern
}
struct Slider {
	ushort cur, delta;
	ubyte cycles, target;
}

struct ChannelState {
	ubyte *ptr;

	int next; // time left in note

	Slider note; ubyte cur_port_start_ctr;
	ubyte note_len, note_style;

	ubyte note_release; // time to release note, in cycles

	int sub_start; // current subroutine number
	ubyte *sub_ret; // where to return to after sub
	ubyte sub_count; // number of loops

	ubyte inst; // instrument
	ubyte inst_adsr1;
	ubyte finetune;
	byte transpose;
	Slider panning; ubyte pan_flags;
	Slider volume;
	ubyte total_vol;
	byte left_vol, right_vol;

	ubyte port_type, port_start, port_length, port_range;
	ubyte vibrato_start, vibrato_speed, vibrato_max_range, vibrato_fadein;
	ubyte tremolo_start, tremolo_speed, tremolo_range;

	ubyte vibrato_phase, vibrato_start_ctr, cur_vib_range;
	ubyte vibrato_fadein_ctr, vibrato_range_delta;
	ubyte tremolo_phase, tremolo_start_ctr;

	Sample *samp;
	int samp_pos, note_freq;

	double env_height; // envelope height
	double decay_rate;
}
struct Sample {
	short *data;
	int length;
	int loop_len;
}
struct Parser {
	const(ubyte)* ptr;
	const(ubyte)* sub_ret;
	int sub_start;
	ubyte sub_count;
	ubyte note_len;
}

struct Song {
	ushort address;
	ubyte changed;
	int[] order;
	int repeat, repeat_pos;
	Track[8][] pattern;
	int subs;
	Track *sub;
}

struct Track {
	int size;
	ubyte *track; // null for inactive track
}
struct Pack {
	int start_address;
	int status;	// See constants above
	Block[] blocks;
}
struct Block {
	ushort size, spc_address;
	ubyte[] data; // only used for inmem packs
}
// note style tables, from 6F80
immutable ubyte[8] release_table = [
	0x33, 0x66, 0x7f, 0x99, 0xb2, 0xcc, 0xe5, 0xfc
];
immutable ubyte[16] volume_table = [
	0x19, 0x33, 0x4c, 0x66, 0x72, 0x7f, 0x8c, 0x99,
	0xa5, 0xb2, 0xbf, 0xcc, 0xd8, 0xe5, 0xf2, 0xfc
];

// number of bytes following a Ex/Fx code
immutable ubyte[32] code_length = [
	1, 1, 2, 3, 0, 1, 2, 1, 2, 1, 1, 3, 0, 1, 2, 3,
	1, 3, 3, 0, 1, 3, 0, 3, 3, 3, 1, 2, 0, 0, 0, 0
];

enum NUM_SONGS = 0xBF;
enum NUM_PACKS = 0xA9;
enum BGM_PACK_TABLE = 0x4F70A;
enum PACK_POINTER_TABLE = 0x4F947;
enum SONG_POINTER_TABLE = 0x26298C;

enum AREA_END = -4;
enum AREA_NOT_IN_FILE = -3;
enum AREA_NON_SPC = -2;
enum AREA_FREE = -1;

enum MAX_TITLE_LEN = 60;
enum MAX_TITLE_LEN_STR = "60";

enum IPACK_INMEM = 1;	// blocks[i].data valid if set
enum IPACK_CHANGED = 2;

enum BRR_BLOCK_SIZE = 9;
enum BRR_FLAG_END = 1;
enum BRR_FLAG_LOOP = 2;

struct NSPCPlayer {
	Song cur_song;
	SongState state;
	SongState pattop_state;
	int mixrate = 44100;
	int bufsize = 2205;
	int chmask = 255;
	int timer_speed = 500;
	bool song_playing;

	int bufs_used;
	ubyte[65536] spc;
	int inst_base = 0x6E00;
	Sample[128] samp;
	private int pat_length;

	const(ubyte)[] romData;

	ubyte[3][NUM_SONGS] pack_used;
	ushort[NUM_SONGS] song_address;
	Pack[NUM_PACKS] rom_packs;
	Pack[NUM_PACKS] inmem_packs;

	int area_count;
	ubyte[3] packs_loaded = [ 0xFF, 0xFF, 0xFF ];
	int current_block = -1;

	int selected_bgm;

	char[60] errbuf;
	string decomp_error;

	void fill_buffer(short[2][] buffer) nothrow @system {
		short[2]* bufp = &buffer[0];
		int bytes_left = cast(int)(buffer.length * 2 * short.sizeof);
		while (bytes_left > 0) {
			if ((state.next_timer_tick -= timer_speed) < 0) {
				state.next_timer_tick += mixrate;
				if (!do_timer()) {
					break;
				}
			}

	//		for (int blah = 0; blah < 50; blah++) {
			int left = 0, right = 0;
			ChannelState *c = &state.chan[0];
			for (int cm = chmask; cm; c++, cm >>= 1) {
				if (!(cm & 1)) continue;

				if (c.samp_pos < 0) continue;

				int ipos = c.samp_pos >> 15;

				Sample *s = c.samp;
				if (ipos > s.length) {
					assert(0, format!"Sample position exceeds sample length! %d > %d"(ipos, s.length));
				}

				if (c.note_release != 0) {
					if (c.inst_adsr1 & 0x1F)
						c.env_height *= c.decay_rate;
				} else {
					// release takes about 15ms (not dependent on tempo)
					c.env_height -= (32000 / 512.0) / mixrate;
					if (c.env_height < 0) {
						c.samp_pos = -1;
						continue;
					}
				}
				double volume = c.env_height / 128.0;
				assert(s.data);
				int s1 = s.data[ipos];
				s1 += (s.data[ipos+1] - s1) * (c.samp_pos & 0x7FFF) >> 15;

				left  += cast(int)(s1 * c.left_vol  * volume);
				right += cast(int)(s1 * c.right_vol * volume);

	//			int sp = c.samp_pos;

				c.samp_pos += c.note_freq;
				if ((c.samp_pos >> 15) >= s.length) {
					if (s.loop_len)
						c.samp_pos -= s.loop_len << 15;
					else
						c.samp_pos = -1;
				}
	//			if (blah != 1) c.samp_pos = sp;
			}
			if (left < -32768) left = -32768;
			else if (left > 32767) left = 32767;
			if (right < -32768) right = -32768;
			else if (right > 32767) right = 32767;
			(*bufp)[0] = cast(short)left;
			(*bufp)[1] = cast(short)right;
	//		}
			bufp++;
			bytes_left -= 4;
		}
		bufs_used++;
	}

	void parser_init(Parser *p, ChannelState c) nothrow @safe {
		p.ptr = cast(const(ubyte)*)c.ptr;
		p.sub_start = c.sub_start;
		p.sub_ret = cast(const(ubyte)*)c.sub_ret;
		p.sub_count = c.sub_count;
		p.note_len = c.note_len;
	}

	const(ubyte)* next_code(const(ubyte)* p) nothrow @system {
		ubyte chr = *p++;
		if (chr < 0x80)
			p += *p < 0x80;
		else if (chr >= 0xE0)
			p += code_length[chr - 0xE0];
		return p;
	}

	bool parser_advance(Parser *p) nothrow @system {
		int chr = *p.ptr;
		if (chr == 0) {
			if (p.sub_count == 0) return false;
			p.ptr = --p.sub_count ? cur_song.sub[p.sub_start].track : p.sub_ret;
		} else if (chr == 0xEF) {
			p.sub_ret = p.ptr + 4;
			p.sub_start = *cast(ushort *)&p.ptr[1];
			p.sub_count = p.ptr[3];
			p.ptr = cur_song.sub[p.sub_start].track;
		} else {
			if (chr < 0x80)
				p.note_len = cast(ubyte)chr;
			p.ptr = next_code(p.ptr);
		}
		return true;
	}
	private void calc_total_vol(const SongState st, ref ChannelState c, byte trem_phase) nothrow @safe {
		ubyte v = (trem_phase << 1 ^ trem_phase >> 7) & 0xFF;
		v = ~(v * c.tremolo_range >> 8) & 0xFF;

		v = v * (st.volume.cur >> 8) >> 8;
		v = v * volume_table[c.note_style & 15] >> 8;
		v = v * (c.volume.cur >> 8) >> 8;
		c.total_vol = v * v >> 8;
	}

	private int calc_vol_3(const ChannelState c, int pan, int flag) nothrow @system {
		static immutable ubyte[21] pan_table = [
			0x00, 0x01, 0x03, 0x07, 0x0D, 0x15, 0x1E, 0x29,
			0x34, 0x42, 0x51, 0x5E, 0x67, 0x6E, 0x73, 0x77,
			0x7A, 0x7C, 0x7D, 0x7E, 0x7F
		];
		const ubyte *ph = &pan_table[pan >> 8];
		int v = ph[0] + ((ph[1] - ph[0]) * (pan & 255) >> 8);
		v = v * c.total_vol >> 8;
		if (c.pan_flags & flag) v = -v;
		return v;
	}

	private void calc_vol_2(ChannelState *c, int pan) nothrow @system {
		c.left_vol  = cast(byte)calc_vol_3(*c, pan,          0x80);
		c.right_vol = cast(byte)calc_vol_3(*c, 0x1400 - pan, 0x40);
	}

	private void make_slider(Slider *s, int cycles, int target) nothrow @system {
		s.delta = cast(ushort)(((target << 8) - (s.cur & 0xFF00)) / cycles);
		s.cycles = cast(ubyte)cycles;
		s.target = cast(ubyte)target;
	}

	private void slide(Slider *s) nothrow {
		if (s.cycles) {
			if (--s.cycles == 0)
				s.cur = s.target << 8;
			else
				s.cur += s.delta;
		}
	}

	void set_inst(SongState *st, ChannelState *c, int inst) nothrow @system {
		// CA and up is for instruments in the second pack (set with FA xx)
		if (inst >= 0x80)
			inst += st.first_CA_inst - 0xCA;

		ubyte *idata = &spc[inst_base + 6*inst];
		if (inst < 0 || inst >= 64 || !samp[idata[0]].data ||
			(idata[4] == 0 && idata[5] == 0))
		{
			assert(0, format!"ch %s: bad inst %X"(c - &st.chan[0], inst));
		}

		c.inst = cast(ubyte)inst;
		c.inst_adsr1 = idata[2];
		if (c.inst_adsr1 & 0x1F) {
			int i = c.inst_adsr1 & 0x1F;
			// calculate the constant to multiply envelope height by on each sample
			int halflife;
			if (i >= 30)
				halflife = 32 - i;
			else
				halflife = ((512 >> (i / 3)) * (5 - i % 3));
			c.decay_rate = pow(2.0, -1.0/(0.0055 * halflife * mixrate));
		}
	}

	// calculate how far to advance the sample pointer on each output sample
	void calc_freq(ChannelState *c, int note16) nothrow @system {
		static const ushort[13] note_freq_table = [
			0x085F, 0x08DF, 0x0965, 0x09F4, 0x0A8C, 0x0B2C, 0x0BD6, 0x0C8B,
			0x0D4A, 0x0E14, 0x0EEA, 0x0FCD, 0x10BE
		];

		// What is this for???
		if (note16 >= 0x3400)     note16 += (note16 >> 8) - 0x34;
		else if (note16 < 0x1300) note16 += ((note16 >> 8) - 0x13) << 1;

		if (cast(ushort)note16 >= 0x5400) {
			c.note_freq = 0;
			return;
		}

		int octave = (note16 >> 8) / 12;
		int tone = (note16 >> 8) % 12;
		int freq = note_freq_table[tone];
		freq += (note_freq_table[tone+1] - freq) * (note16 & 0xFF) >> 8;
		freq <<= 1;
		freq >>= 6 - octave;

		ubyte *inst_freq = &spc[inst_base + 6*c.inst + 4];
		freq *= (inst_freq[0] << 8 | inst_freq[1]);
		freq >>= 8;
		freq &= 0x3fff;

		c.note_freq = (freq * (32000U << (15 - 12))) / mixrate;
	}

	private int calc_vib_disp(ChannelState *c, int phase) nothrow @system {
		int range = c.cur_vib_range;
		if (range > 0xF0)
			range = (range - 0xF0) * 256;

		int disp = (phase << 2) & 255;   /* //// */
		if (phase & 0x40) disp ^= 0xFF;  /* /\/\ */
		disp = (disp * range) >> 8;

		if (phase & 0x80) disp = -disp;  /* /\   */
		return disp;                     /*   \/ */
	}

	// do a Ex/Fx code
	private void do_command(SongState *st, ChannelState *c) nothrow @system {
		ubyte* p = c.ptr;
		c.ptr += 1 + code_length[*p - 0xE0];
		switch (*p) {
			case 0xE0:
				set_inst(st, c, p[1]);
				break;
			case 0xE1:
				c.pan_flags = p[1];
				c.panning.cur = (p[1] & 0x1F) << 8;
				break;
			case 0xE2:
				make_slider(&c.panning, p[1], p[2]);
				break;
			case 0xE3:
				c.vibrato_start = p[1];
				c.vibrato_speed = p[2];
				c.cur_vib_range = c.vibrato_max_range = p[3];
				c.vibrato_fadein = 0;
				break;
			case 0xE4:
				c.cur_vib_range = c.vibrato_max_range = 0;
				c.vibrato_fadein = 0;
				break;
			case 0xE5:
				st.volume.cur = p[1] << 8;
				break;
			case 0xE6:
				make_slider(&st.volume, p[1], p[2]);
				break;
			case 0xE7:
				st.tempo.cur = p[1] << 8;
				break;
			case 0xE8:
				make_slider(&st.tempo, p[1], p[2]);
				break;
			case 0xE9:
				st.transpose = p[1];
				break;
			case 0xEA:
				c.transpose = p[1];
				break;
			case 0xEB:
				c.tremolo_start = p[1];
				c.tremolo_speed = p[2];
				c.tremolo_range = p[3];
				break;
			case 0xEC:
				c.tremolo_range = 0;
				break;
			case 0xED:
				c.volume.cur = p[1] << 8;
				break;
			case 0xEE:
				make_slider(&c.volume, p[1], p[2]);
				break;
			case 0xEF:
				c.sub_start = p[1] | (p[2] << 8);
				c.sub_ret = c.ptr;
				c.sub_count = p[3];
				c.ptr = cur_song.sub[c.sub_start].track;
				break;
			case 0xF0:
				c.vibrato_fadein = p[1];
				c.vibrato_range_delta = c.cur_vib_range / p[1];
				break;
			case 0xF1: case 0xF2:
				c.port_type = *p & 1;
				c.port_start = p[1];
				c.port_length = p[2];
				c.port_range = p[3];
				break;
			case 0xF3:
				c.port_length = 0;
				break;
			case 0xF4:
				c.finetune = p[1];
				break;
			case 0xF9: {
				c.cur_port_start_ctr = p[1];
				int target = p[3] + st.transpose;
				if (target >= 0x100) target -= 0xFF;
				target += c.transpose;
				make_slider(&c.note, p[2], target & 0x7F);
				break;
			}
			case 0xFA:
				st.first_CA_inst = p[1];
				break;
			default: break;
		}
	}

	// $0654 + $08D4-$8EF
	private void do_note(SongState *st, ChannelState *c, int note) nothrow @system {
		// using >=CA as a note switches to that instrument and plays note A4
		if (note >= 0xCA) {
			set_inst(st, c, note);
			note = 0xA4;
		}

		if (note < 0xC8) {
			c.vibrato_phase = c.vibrato_fadein & 1 ? 0x80 : 0;
			c.vibrato_start_ctr = 0;
			c.vibrato_fadein_ctr = 0;
			c.tremolo_phase = 0;
			c.tremolo_start_ctr = 0;

			c.samp_pos = 0;
			c.samp = &samp[spc[inst_base + 6*c.inst]];
			c.env_height = 1;

			note &= 0x7F;
			note += st.transpose + c.transpose;
			c.note.cur = cast(ushort)(note << 8 | c.finetune);

			c.note.cycles = c.port_length;
			if (c.note.cycles) {
				int target = note;
				c.cur_port_start_ctr = c.port_start;
				if (c.port_type == 0)
					c.note.cur -= c.port_range << 8;
				else
					target += c.port_range;
				make_slider(&c.note, c.port_length, target & 0x7F);
			}

			calc_freq(c, c.note.cur);
		}

		// Search forward for the next note (to see if it's C8). This is annoying
		// but necessary - C8 can continue the last note of a subroutine as well
		// as a normal note.
		int next_note;
		{	Parser p;
			parser_init(&p, *c);
			do {
				if (*p.ptr >= 0x80 && *p.ptr < 0xE0)
					break;
			} while (parser_advance(&p));
			next_note = *p.ptr;
		}

		int rel;
		if (next_note == 0xC8) {
			// if the note will be continued, don't release yet
			rel = c.note_len;
		} else {
			rel = (c.note_len * release_table[c.note_style >> 4]) >> 8;
			if (rel > c.note_len - 2)
				rel = c.note_len - 2;
			if (rel < 1)
				rel = 1;
		}
		c.note_release = cast(ubyte)rel;
	}


	void load_pattern() nothrow @system {
		state.ordnum++;
		if (state.ordnum >= cur_song.order.length) {
			if (--state.repeat_count >= 0x80)
				state.repeat_count = cast(ubyte)cur_song.repeat;
			if (state.repeat_count == 0) {
				state.ordnum--;
				song_playing = false;
				return;
			}
			state.ordnum = cur_song.repeat_pos;
		}

		int pat = cur_song.order[state.ordnum];

		int ch;
		for (ch = 0; ch < 8; ch++) {
			state.chan[ch].ptr = cur_song.pattern[pat][ch].track;
			state.chan[ch].sub_count = 0;
			state.chan[ch].volume.cycles = 0;
			state.chan[ch].panning.cycles = 0;
			state.chan[ch].next = 0;
		}
		state.patpos = 0;

		pattop_state = state;
	}

	private void CF7(ChannelState *c) nothrow @system {
		if (c.note_release)
			c.note_release--;

		// 0D60
		if (c.note.cycles) {
			if (c.cur_port_start_ctr == 0) {
				slide(&c.note);
				calc_freq(c, c.note.cur);
			} else {
				c.cur_port_start_ctr--;
			}
		}

		// 0D79
		if (c.cur_vib_range) {
			if (c.vibrato_start_ctr == c.vibrato_start) {
				int range;
				if (c.vibrato_fadein_ctr == c.vibrato_fadein) {
					range = c.vibrato_max_range;
				} else {
					range = c.cur_vib_range;
					if (c.vibrato_fadein_ctr == 0)
						range = 0;
					range += c.vibrato_range_delta;
					c.vibrato_fadein_ctr++;
				} // DA0
				c.cur_vib_range = cast(ubyte)range;
				c.vibrato_phase += c.vibrato_speed;
				calc_freq(c, c.note.cur + calc_vib_disp(c, c.vibrato_phase));
			} else {
				c.vibrato_start_ctr++;
			}
		}
	}

	// $07F9 + $0625
	private bool do_cycle(SongState *st) nothrow @system {
		int ch;
		ChannelState *c;
		for (ch = 0; ch < 8; ch++) {
			c = &st.chan[ch];
			if (c.ptr == null) continue; //8F0

			if (--c.next >= 0) {
				CF7(c);
			} else while (1) {
				ubyte *p = c.ptr;

				if (*p == 0) { // end of sub or pattern
					if (c.sub_count) // end of sub
						c.ptr = --c.sub_count
							? cur_song.sub[c.sub_start].track
							: c.sub_ret;
					else
						return false;
				} else if (*p < 0x80) {
					c.note_len = *p;
					if (p[1] < 0x80) {
						c.note_style = p[1];
						c.ptr += 2;
					} else {
						c.ptr++;
					}
				} else if (*p < 0xE0) {
					c.ptr++;
					c.next = c.note_len - 1;
					do_note(st, c, *p);
					break;
				} else { // E0-FF
					do_command(st, c);
				}
			}
			// $0B84
			if (c.note.cycles == 0 && *c.ptr == 0xF9)
				do_command(st, c);
		}

		st.patpos++;

		slide(&st.tempo);
		slide(&st.volume);

		for (c = &st.chan[0]; c != &st.chan[8]; c++) {
			if (c.ptr == null) continue;

			// @ 0C40
			slide(&c.volume);

			// @ 0C4D
			int tphase = 0;
			if (c.tremolo_range) {
				if (c.tremolo_start_ctr == c.tremolo_start) {
					if (c.tremolo_phase >= 0x80 && c.tremolo_range == 0xFF)
						c.tremolo_phase = 0x80;
					else
						c.tremolo_phase += c.tremolo_speed;
					tphase = c.tremolo_phase;
				} else {
					c.tremolo_start_ctr++;
				}
			}
			calc_total_vol(*st, *c, cast(byte)tphase);

			// 0C79
			slide(&c.panning);

			// 0C86: volume stuff
			calc_vol_2(c, c.panning.cur);
		}
		return true;
	}

	bool do_cycle_no_sound(SongState *st) nothrow @system {
		bool ret = do_cycle(st);
		if (ret) {
			int ch;
			for (ch = 0; ch < 8; ch++)
				if (st.chan[ch].note_release == 0)
					st.chan[ch].samp_pos = -1;
		}
		return ret;
	}

	private int sub_cycle_calc(const SongState st, int delta) nothrow @safe {
		if (delta < 0x8000)
			return st.cycle_timer * delta >> 8;
		else
			return -(st.cycle_timer * (0x10000 - delta) >> 8);
	}

	private void do_sub_cycle(SongState *st) nothrow @system {
		ChannelState *c;
		for (c = &st.chan[0]; c != &st.chan[8]; c++) {
			if (c.ptr == null) continue;
			// $0DD0

			bool changed = false;
			if (c.tremolo_range && c.tremolo_start_ctr == c.tremolo_start) {
				int p = c.tremolo_phase + sub_cycle_calc(*st, c.tremolo_speed);
				changed = true;
				calc_total_vol(*st, *c, cast(byte)p);
			}
			int pan = c.panning.cur;
			if (c.panning.cycles) {
				pan += sub_cycle_calc(*st, c.panning.delta);
				changed = true;
			}
			if (changed) calc_vol_2(c, pan);

			changed = false;
			int note = c.note.cur; // $0BBC
			if (c.note.cycles && c.cur_port_start_ctr == 0) {
				note += sub_cycle_calc(*st, c.note.delta);
				changed = true;
			}
			if (c.cur_vib_range && c.vibrato_start_ctr == c.vibrato_start) {
				int p = c.vibrato_phase + sub_cycle_calc(*st, c.vibrato_speed);
				note += calc_vib_disp(c, p);
				changed = true;
			}
			if (changed) calc_freq(c, note);
		}
	}

	bool do_timer() nothrow @system {
		state.cycle_timer += state.tempo.cur >> 8;
		if (state.cycle_timer >= 256) {
			state.cycle_timer -= 256;
			while (!do_cycle(&state)) {
				load_pattern();
				if (!song_playing) return false;
			}
		} else {
			do_sub_cycle(&state);
		}
		return true;
	}

	void initialize() nothrow @system {
		memset(&state, 0, state.sizeof);
		int i;
		for (i = 0; i < 8; i++) {
			state.chan[i].volume.cur = 0xFF00;
			state.chan[i].panning.cur = 0x0A00;
			state.chan[i].samp_pos = -1;
		}
		state.volume.cur = 0xC000;
		state.tempo.cur = 0x2000;
		state.cycle_timer = 255;

		state.ordnum = -1;
		if (cur_song.order.length) {
			load_pattern();
		} else {
			pattop_state = state;
			song_playing = false;
		}
	}
	void play() @system {
		song_playing = true;
		song_selected(147);
	}
	void loadSong(ubyte[] data) @safe {

	}

	bool open_rom(const(ubyte)[] data) @system {
		romData = data;
		if (romData.length < 0x300000) {
			assert(0, "An EarthBound ROM must be at least 3 MB");
		}

		pack_used = read!(typeof(pack_used))(romData, BGM_PACK_TABLE);
		static align(1) struct PackPtr {
			align(1):
			ubyte bank;
			ushort nearAddr;
		}
		auto packs = read!(PackPtr[NUM_PACKS])(romData, BGM_PACK_TABLE + pack_used.sizeof);
		// pack pointer table follows immediately after
		for (int i = 0; i < NUM_PACKS; i++) {
			rom_packs[i].start_address = (packs[i].bank << 16) + packs[i].nearAddr;
		}

		song_address = read!(typeof(song_address))(romData, SONG_POINTER_TABLE);

		for (int i = 0; i < NUM_PACKS; i++) {
			int size;
			int count = 0;
			Block *blocks = null;
			bool valid = true;
			Pack *rp = &rom_packs[i];

			int offset = rp.	start_address - 0xC00000;
			if (offset >= romData.length) {
				valid = false;
				goto bad_pointer;
			}

			while ((size = read!ushort(romData, offset)) > 0) {
				int spc_addr = read!ushort(romData, offset + 2);
				if (spc_addr + size > 0x10000) { valid = false; break; }
				offset += 4 + size;
				if (offset > romData.length) { valid = false; break; }

				count++;
				blocks = cast(Block*)realloc(blocks, Block.sizeof * count);
				blocks[count-1].size = cast(ushort)size;
				blocks[count-1].spc_address = cast(ushort)spc_addr;

	/*			if (spc_addr == 0x0500) {
					int back = ftell(f);
					fseek(f, 0x2E4A - 0x500, SEEK_CUR);
					fread(song_address, NUM_SONGS, 2, f);
					fseek(f, back, SEEK_SET);
				}*/

				spc[spc_addr .. spc_addr + size] = romData[offset + 4 .. offset + 4 + size];
			}
	bad_pointer:
			rp.blocks = blocks[0 .. count];
			inmem_packs[i].status = 0;
		}
		return true;
	}

	void free_song(Song *s) nothrow @system {
		int pat, ch, sub;
		if (!s.order.length) return;
		s.changed = false;
		for (pat = 0; pat < s.pattern.length; pat++)
			for (ch = 0; ch < 8; ch++)
				theAllocator.dispose(s.pattern[pat][ch].track);
		theAllocator.dispose(s.pattern);
		for (sub = 0; sub < s.subs; sub++)
			theAllocator.dispose(s.sub[sub].track);
		theAllocator.dispose(s.sub);
	}
	void free_pack(Pack *p) nothrow @system {
		for (int i = 0; i < p.blocks.length; i++)
			theAllocator.dispose(&p.blocks[i].data[0]);
		theAllocator.dispose(&p.blocks[0]);
		p.status = 0;
	}
	void free_samples() nothrow @system {
		for (int sn = 0; sn < 128; sn++) {
			theAllocator.dispose(samp[sn].data);
			samp[sn].data = null;
		}
	}
	private void load_music(ubyte *packs_used, int spc_addr) @system {
		packs_loaded[0] = packs_used[0];
		packs_loaded[1] = packs_used[1];
		load_songpack(packs_used[2]);
		select_block_by_address(spc_addr);
		load_instruments();
	}

	private void song_selected(int index) @system {
		selected_bgm = index;
		load_music(&pack_used[index][0], song_address[index]);
	}

	void load_instruments() nothrow @system {
		free_samples();
		memset(&spc[0], 0, 0x10000);
		for (int i = 0; i < 2; i++) {
			int p = packs_loaded[i];
			if (p >= NUM_PACKS) continue;
			int addr, size;
			auto base = rom_packs[p].start_address - 0xC00000;
			while ((size = (cast(ushort[])(romData[base .. base + 2]))[0]) != 0) {
				addr = (cast(ushort[])(romData[base + 2 .. base + 4]))[0];
				if (size + addr >= 0x10000) {
					assert(0, "Invalid SPC block");
				}
				spc[addr .. addr + size] = romData[base + 4 .. base + 4 + size];
				base += size + 4;
			}
		}
		decode_samples(spc[0x6C00 .. 0x6E00]);
		inst_base = 0x6E00;
		if (samp[0].data == null)
			song_playing = false;
		initialize();
	}
	void load_songpack(int new_pack) nothrow @system {
		if (packs_loaded[2] == new_pack)
			return;

		// Unload the current songpack unless it has been changed
		if (packs_loaded[2] < NUM_PACKS) {
			Pack *old = &inmem_packs[packs_loaded[2]];
			if (!(old.status & IPACK_CHANGED))
				free_pack(old);
		}

		packs_loaded[2] = cast(ubyte)new_pack;
		if (new_pack >= NUM_PACKS)
			return;

		load_pack(new_pack);
	}
	Pack *load_pack(int pack_) nothrow @safe {
		Pack *mp = &inmem_packs[pack_];
		if (!(mp.status & IPACK_INMEM)) {
			Pack *rp = &rom_packs[pack_];
			mp.start_address = rp.start_address;
			mp.blocks = new Block[](rp.blocks.length);
			mp.blocks[] = rp.blocks[];
			Block[] b = mp.blocks;
			auto base = mp.start_address - 0xC00000;
			for (int i = 0; i < mp.blocks.length; i++) {
				b[i].data = new ubyte[](b[i].size);
				b[i].data[0 .. b[i].size] = romData[base + 4 .. base + 4 + b[i].size];
				base += 4 + b[i].size;
			}
			mp.status |= IPACK_INMEM;
		}

		return mp;
	}
	void select_block(int block_) @system {
		current_block = block_;

		free_song(&cur_song);

		Block *b = get_cur_block();
		if (b != null) {
			spc[b.spc_address .. b.spc_address + b.size] = b.data[0 .. b.size];
			decompile_song(cur_song, b.spc_address, b.spc_address + b.size);
		}
		initialize();
	}

	void select_block_by_address(int spc_addr) @system {
		int bnum = -1;
		if (packs_loaded[2] < NUM_PACKS) {
			Pack *p = &inmem_packs[packs_loaded[2]];
			for (bnum = cast(int)p.blocks.length - 1; bnum >= 0; bnum--) {
				Block *b = &p.blocks[bnum];
				if (cast(uint)(spc_addr - b.spc_address) < b.size) break;
			}
		}
		select_block(bnum);
	}
	void decompile_song(ref Song song, int start_addr, int end_addr) @system {
		ushort *sub_table;
		int first_pattern;
		int tracks_start;
		int tracks_end;
		int pat_bytes;
		string error;
		song.address = cast(ushort)start_addr;
		song.changed = false;

		// Get order length and repeat info (at this point, we don't know how
		// many patterns there are, so the pattern pointers aren't validated yet)
		ushort[] wp = cast(ushort[])spc[start_addr .. $];
		while (wp[0] >= 0x100) wp = wp[1 .. $];
		song.order.length = cast(int)(&wp[0] - cast(ushort *)&spc[start_addr]);
		if (song.order.length == 0) {
			throw new Exception("Order length is 0");
		}
		song.repeat = wp[0];
		wp = wp[1 .. $];
		if (song.repeat == 0) {
			song.repeat_pos = 0;
		} else {
			int repeat_off = wp[0] - start_addr;
			wp = wp[1 .. $];
			if (repeat_off & 1 || repeat_off < 0 || repeat_off >= song.order.length*2) {
				throw new Exception(format!"Bad repeat pointer: %x"(repeat_off + start_addr));
			}
			if (wp[0] != 0) {
				throw new Exception("Repeat not followed by end of song");
			}
			wp = wp[1 .. $];
			song.repeat_pos = repeat_off >> 1;
		}

		first_pattern = cast(int)(cast(ubyte *)&wp[0] - &spc[0]);

		// locate first track, determine number of patterns
		while ((cast(ubyte *)&wp[0])+1 < &spc[end_addr] && wp[0] == 0) wp = wp[1 .. $];
		if ((cast(ubyte *)&wp[0])+1 >= &spc[end_addr]) {
			// no tracks in the song
			tracks_start = end_addr - 1;
		} else {
			tracks_start = wp[0];
		}

		pat_bytes = tracks_start - first_pattern;
		if (pat_bytes <= 0 || pat_bytes & 15) {
			throw new Exception(format!"Bad first track pointer: %x"(tracks_start));
		}

		if ((cast(ubyte *)wp)+1 >= &spc[end_addr]) {
			// no tracks in the song
			tracks_end = end_addr - 1;
		} else {
			// find the last track
			int tp, tpp = tracks_start;
			while ((tp = *cast(ushort *)&spc[tpp -= 2]) == 0) {}

			if (tp < tracks_start || tp >= end_addr) {
				throw new Exception(format!"Bad last track pointer: %x"(tp));
			}


			// is the last track the first one in its pattern?
			bool first = true;
			int chan = (tpp - first_pattern) >> 1 & 7;
			for (; chan; chan--)
				first &= *cast(ushort *)&spc[tpp -= 2] == 0;

			const(ubyte)* end = &spc[tp];
			while (*end) end = next_code(end);
			end += first;
			tracks_end = cast(ushort)(end - &spc[0]);
		}

		// Now the number of patterns is known, so go back and get the order
		song.order = new int[](song.order.length);
		wp = cast(ushort[])spc[start_addr .. $];
		for (int i = 0; i < song.order.length; i++) {
			int pat = wp[0] - first_pattern;
			wp = wp[1 .. $];
			if (pat < 0 || pat >= pat_bytes || pat & 15) {
				throw new Exception(format!"Bad pattern pointer: %x"(pat + first_pattern));
			}
			song.order[i] = pat >> 4;
		}

		sub_table = null;
		song.pattern = new Track[8][](pat_bytes >> 4);
		song.subs = 0;
		song.sub = null;

		wp = cast(ushort[])spc[first_pattern .. $];
		for (int trk = 0; trk < song.pattern.length * 8; trk++) {
			Track *t = &song.pattern[0][0] + trk;
			int start = wp[0];
			wp = wp[1 .. $];
			if (start == 0) continue;
			if (start < tracks_start || start >= tracks_end) {
				throw new Exception(format!"Bad track pointer: %x"(start));
			}

			// Go through track list (patterns) and find first track that has an address higher than us.
			// If we find a track after us, we'll assume that this track doesn't overlap with that one.
			// If we don't find one, then next will remain at 0x10000 and we will search until the
			// end of memory to find a 00 byte to terminate the track.
			int next = 0x10000; // offset of following track
			for (int track_ind = 0; track_ind < (song.pattern.length * 8); track_ind += 1) {
				int track_addr = (cast(ushort *)(&spc[first_pattern]))[track_ind];
				if (track_addr < next && track_addr > start) {
					next = track_addr;
				}
			}
			// Determine the end of the track.
			const(ubyte)* track_end;
			for (track_end = &spc[start]; track_end < &spc.ptr[next] && *track_end != 0; track_end = next_code(track_end)) {}

			t.size = cast(int)((track_end - &spc[0]) - start);
			t.track = &theAllocator.makeArray!ubyte(t.size + 1)[0];
			scope(failure) {
				theAllocator.dispose(t.track);
			}
			t.track[0 .. t.size] = spc[start .. start + t.size];
			t.track[t.size] = 0;

			for (const(ubyte)* p = t.track; p < t.track + t.size; p = next_code(p)) {
				if (*p != 0xEF) continue;
				int sub_ptr = *cast(ushort *)(p + 1);
				int sub_entry;

				// find existing entry in sub_table
				for (sub_entry = 0; sub_entry < song.subs && sub_table[sub_entry] != sub_ptr; sub_entry++) {}
				if (sub_entry == song.subs) {
					// sub_entry doesn't already exist in sub_table; create it
					sub_entry = song.subs++;

					sub_table = &theAllocator.makeArray!ushort(song.subs)[0];
					scope(failure) {
						theAllocator.dispose(sub_table);
					}
					sub_table[sub_entry] = cast(ushort)sub_ptr;

					song.sub = cast(Track*)realloc(song.sub, Track.sizeof * song.subs);
					Track *st = &song.sub[sub_entry];

					ubyte *substart = &spc[sub_ptr];
					const(ubyte)* subend = substart;
					while (*subend != 0) subend = next_code(subend);
					st.size = cast(int)(subend - substart);
					st.track = &theAllocator.makeArray!ubyte(st.size + 1)[0];
					scope(failure) {
						theAllocator.dispose(st.track);
					}
					st.track[0 .. st.size + 1] = substart[0 .. st.size + 1];
					internal_validate_track(st.track[0 .. st.size], true);
				}
				*cast(ushort *)(p + 1) = cast(ushort)sub_entry;
			}
			internal_validate_track(t.track[0 .. t.size], false);
		}
		theAllocator.dispose(sub_table);
	}
	void decode_samples(const(ubyte)[] ptrtable) nothrow @system {
		for (uint sn = 0; sn < 128; sn++) {
			Sample *sa = &samp[sn];
			int start = ptrtable[0] | (ptrtable[1] << 8);
			int loop  = ptrtable[2] | (ptrtable[3] << 8);
			ptrtable = ptrtable[4 .. $];

			sa.data = null;
			if (start == 0 || start == 0xffff)
				continue;

			int length = sample_length(spc, cast(ushort)start);
			if (length == -1)
				continue;

			int end = start + length;
			sa.length = (length / BRR_BLOCK_SIZE) * 16;
			// The LOOP bit only matters for the last brr block
			if (spc[start + length - BRR_BLOCK_SIZE] & BRR_FLAG_LOOP) {
				if (loop < start || loop >= end)
					continue;
				sa.loop_len = ((end - loop) / BRR_BLOCK_SIZE) * 16;
			} else
				sa.loop_len = 0;

			size_t allocation_size = sa.length + 1;

			short* p = cast(short*)malloc(allocation_size * short.sizeof);
			if (!p) {
				debug assert(0, "malloc failed in BRR decoding");
			}
	/*		printf("Sample %2d: %04X(%04X)-%04X length %d looplen %d\n",
				sn, start, loop, end, sa.length, sa.loop_len);*/

			sa.data = p;

			int needs_another_loop;
			int first_block = true;
			int decoding_start = start;
			int times = 0;

			do {
				needs_another_loop = false;

				for (int pos = decoding_start; pos < end; pos += BRR_BLOCK_SIZE) {
					decode_brr_block(p[0 .. 16], [p[-2],p[-1]], spc[pos .. pos + BRR_BLOCK_SIZE], !!first_block);
					p += 16;
					first_block = false;
				}

				if (sa.loop_len != 0) {
					decoding_start = loop;

					short[18] after_loop;
					after_loop[0] = p[-2];
					after_loop[1] = p[-1];

					decode_brr_block(after_loop[2 .. 18], after_loop[0 .. 2], spc[loop .. loop + BRR_BLOCK_SIZE], false);
					int full_loop_len = get_full_loop_len(*sa, after_loop[2 .. 4], (loop - start) / BRR_BLOCK_SIZE * 16);

					if (full_loop_len == -1) {
						needs_another_loop = true;
						//printf("We need another loop! sample %02X (old loop start samples: %d %d)\n", (unsigned)sn,
						//	sa.data[sa.length - sa.loop_len],
						//	sa.data[sa.length - sa.loop_len + 1]);
						ptrdiff_t diff = p - sa.data;
						short *new_stuff = cast(short*)realloc(sa.data, (sa.length + sa.loop_len + 1) * short.sizeof);
						if (new_stuff == null) {
							debug { assert(0, "realloc failed in BRR decoding"); } else {
								// TODO What do we do now? Replace this with something better
								needs_another_loop = false;
								break;
							}
						}
						p = new_stuff + diff;
						sa.length += sa.loop_len;
						sa.data = new_stuff;
					} else {
						sa.loop_len = full_loop_len;
						// needs_another_loop is already false
					}
				}

				// In the vanilla game, the most iterations needed is 48 (for sample 0x17 in pack 5).
				// Most samples need less than 10.
				++times;
			} while (needs_another_loop && times < 64);

			if (needs_another_loop) {
				debug assert(0, "Sample took too many iterations to get into a cycle");
			}

			// Put an extra sample at the end for easier interpolation
			*p = sa.loop_len != 0 ? sa.data[sa.length - sa.loop_len] : 0;
		}
	}
	void internal_validate_track(ubyte[] data, bool is_sub) @safe {
		for (int pos = 0; pos < data.length; ) {
			int byte_ = data[pos];
			int next = pos + 1;

			if (byte_ < 0x80) {
				if (byte_ == 0) throw new Exception("Track can not contain [00]");
				if (next != data.length && data[next] < 0x80) next++;
				if (next == data.length) throw new Exception("Track can not end with note-length code");
			} else if (byte_ >= 0xE0) {
				if (byte_ == 0xFF) throw new Exception("Invalid code [FF]");
				next += code_length[byte_ - 0xE0];
				if (next > data.length) {
					throw new Exception(format!"Incomplete code: [%(%02X %)]"(data[pos .. pos + data.length]));
				}

				if (byte_ == 0xEF) {
					if (is_sub) throw new Exception("Can't call sub from within a sub");
					int sub = (cast(ushort[])data[pos+1 .. pos + 3])[0];
					if (sub >= cur_song.subs) {
						throw new Exception(format!"Subroutine %d not present"(sub));
					}
					if (data[pos+3] == 0) throw new Exception("Subroutine loop count can not be 0");
				}
			}

			pos = next;
		}
	}

	Block* get_cur_block() @safe {
		if (packs_loaded[2] >= NUM_PACKS) {
			throw new Exception("Pack out of range");
		}
		Pack *p = &inmem_packs[packs_loaded[2]];
		if ((current_block < 0) || (current_block >= p.blocks.length)) {
			throw new Exception("Block out of range");
		}
		return &p.blocks[current_block];
	}
}

private void decode_brr_block(short[] buffer, short[2] initial, const ubyte[] block, bool first_block) nothrow @safe {
	int range = block[0] >> 4;
	int filter = (block[0] >> 2) & 3;

	if (first_block) {
		// According to SPC_DSP, the header is ignored on key on.
		// Not enforcing this could result in a read out of bounds, if the filter is nonzero.
		range = 0;
		filter = 0;
	}
	short[2] lastSamples = initial;
	for (int i = 2; i < 18; i++) {
		int s = block[i / 2];

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
			case 1: s += (cast(int)lastSamples[1] * 15) >> 5; break;
			case 2: s += ((cast(int)lastSamples[1] * 61) >> 6) - ((cast(int)lastSamples[0] * 15) >> 5); break;
			case 3: s += ((cast(int)lastSamples[1] * 115) >> 7) - ((cast(int)lastSamples[0] * 13) >> 5); break;
			default: break;
		}

		s *= 2;

		// Clamp to [-65536, 65534] and then have it wrap around at
		// [-32768, 32767]
		if (s < -0x10000) s = (-0x10000 + 0x10000);
		else if (s > 0xFFFE) s = (0xFFFE - 0x10000);
		else if (s < -0x8000) s += 0x10000;
		else if (s > 0x7FFF) s -= 0x10000;

		lastSamples[0] = lastSamples[1];
		lastSamples[1] = cast(short)s;
		buffer[0] = cast(short)s;
		buffer = buffer[1 .. $];
	}
}

private int sample_length(const ubyte[] spc_memory, ushort start) nothrow @safe {
	int end = start;
	ubyte b;
	do {
		b = spc_memory[end];
		end += BRR_BLOCK_SIZE;
	} while ((b & BRR_FLAG_END) == 0 && end < 0x10000 - BRR_BLOCK_SIZE);

	if (end < 0x10000 - BRR_BLOCK_SIZE)
		return end - start;
	else
		return -1;
}

static int get_full_loop_len(const Sample sa, const short[2] next_block, int first_loop_start) nothrow @system {
	int loop_start = sa.length - sa.loop_len;
	int no_match_found = true;
	while (loop_start >= first_loop_start && no_match_found) {
		// If the first two samples in a loop are the same, the rest all will be too.
		// BRR filters can rely on, at most, two previous samples.
		if (sa.data[loop_start] == next_block[0] &&
				sa.data[loop_start + 1] == next_block[1]) {
			no_match_found = false;
		} else {
			loop_start -= sa.loop_len;
		}
	}

	if (loop_start >= first_loop_start)
		return sa.length - loop_start;
	else
		return -1;
}

T read(T)(const(ubyte)[] data, size_t offset) {
	return (cast(const(T)[])(data[offset .. offset + T.sizeof]))[0];
}
