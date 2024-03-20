import nspcplay;
import nspcplay.util;

import std.algorithm.comparison;
import std.algorithm.iteration;
import std.algorithm.searching;
import std.conv;
import std.digest : toHexString;
import std.experimental.logger;
import std.exception;
import std.file;
import std.format;
import std.getopt;
import std.path;
import std.range;
import std.stdio;
import std.string;
import std.utf;
import bindbc.sdl : SDL_AudioCallback, SDL_AudioDeviceID;

import midi;

extern(C) int kbhit();
extern(C) int getch();

bool initAudio(SDL_AudioCallback fun, ubyte channels, uint sampleRate, void* userdata = null) {
	SDL_AudioDeviceID dev;
	import bindbc.sdl;

	enforce(loadSDL() == sdlSupport);
	if (SDL_Init(SDL_INIT_AUDIO) != 0) {
		criticalf("SDL init failed: %s", SDL_GetError().fromStringz);
		return false;
	}
	SDL_AudioSpec want, have;
	want.freq = sampleRate;
	want.format = AUDIO_S16;
	want.channels = channels;
	want.samples = 512;
	want.callback = fun;
	want.userdata = userdata;
	dev = SDL_OpenAudioDevice(null, 0, &want, &have, 0);
	if (dev == 0) {
		criticalf("SDL_OpenAudioDevice failed: %s", SDL_GetError().fromStringz);
		return false;
	}
	SDL_PauseAudioDevice(dev, 0);
	return true;
}

extern (C) void _sampling_func(void* user, ubyte* buf, int bufSize) nothrow {
	NSPCPlayer* nspc = cast(NSPCPlayer*) user;
	try {
		nspc.fillBuffer(cast(short[2][])(buf[0 .. bufSize]));
	} catch (Error e) {
		assumeWontThrow(writeln(e));
		throw e;
	}
}
__gshared int midiChannel = 0;

int main(string[] args) {
	enum channels = 2;
	bool verbose;
	int sampleRate = 44100;
	ushort speed = NSPCPlayer.defaultSpeed;
	string outfile;
	string replaceSamples;
	bool printSong;
	bool dumpBRRFiles;
	string channelsEnabled = "11111111";
	string phraseString;
	Interpolation interpolation;
	uint midiDevice = uint.max;
	if (args.length < 2) {
		return 1;
	}

	auto help = getopt(args,
		"c|channels", "Enables/disables channels", &channelsEnabled,
		"m|mididevice", "Opens a midi device for input", &midiDevice,
		"q|midichannel", "MIDI channel offset for midi device", &midiChannel,
		"f|samplerate", "Sets sample rate (Hz)", &sampleRate,
		"i|interpolation", "Sets interpolation (linear, gaussian, sinc, cubic)", &interpolation,
		"b|brrdump", "Dumps BRR samples used", &dumpBRRFiles,
		"p|print", "Print song in a vaguely human-readable format", &printSong,
		"o|outfile", "Dumps output to file", &outfile,
		"r|replacesamples", "Replaces built-in samples with samples found in directory", &replaceSamples,
		"v|verbose", "Print more verbose information", &verbose,
		"z|phrases", "Override phrase list with custom one", &phraseString,
		"s|speed", "Sets playback speed (500 is default)", &speed);
	if (help.helpWanted) {
		defaultGetoptPrinter("NSPC player", help.options);
		return 1;
	}
	if (verbose) {
		(cast()sharedLog).logLevel = LogLevel.trace;
	}

	auto filePath = args[1];
	auto file = cast(ubyte[])read(args[1]);

	auto nspc = NSPCPlayer(sampleRate);
	// initialization

	trace("Loading NSPC file");
	// Load files
	ushort[] phrases;
	foreach (phrasePortion; phraseString.splitter(",")) {
		phrases ~= phrasePortion.to!ushort(16);
	}
	Song song;
	if (filePath.extension == ".spc") {
		song = loadSPCFile(file);
	} else {
		song = loadNSPCFile(file, phrases);
	}
	if (song.tags) {
		info("Tags:");
		foreach (pair; song.tags) {
			if (pair.key.startsWith("_")) {
				continue;
			}
			infof("%s: %s", pair.key, pair.str);
		}
	}
	if (channelsEnabled.length != 8) {
		stderr.writeln("Channel string must be exactly 8 characters long!");
		return 1;
	}
	nspc.setSpeed(speed);

	nspc.interpolation = interpolation;
	if (replaceSamples != "") {
		foreach(idx, sample; song.getSamples) {
			const glob = format!"%s.brr.wav"(sample.hash.toHexString);
			auto matched = dirEntries(replaceSamples, glob, SpanMode.shallow);
			if (!matched.empty) {
				int newLoop;
				uint loopEnd;
				short[] newSample;
				auto data = cast(ubyte[])read(matched.front.name);
				auto riffFile = RIFFFile(data);
				bool validated;
				bool downMix;
				foreach (chunk; riffFile.chunks) {
					if (chunk.fourCC == "fmt ") {
						const wavHeader = readWaveHeader(chunk.data);
						if (wavHeader.channels == 2) {
							downMix = true;
						} else if (wavHeader.channels == 1) {
						} else {
							errorf("Sample must be mono or stereo!");
							continue;
						}
						if (wavHeader.bitsPerSample != 16){
							errorf("Sample must be 16-bit!");
							continue;
						}
						validated = true;
					}
					if (chunk.fourCC == "smpl") {
						auto smpl = readSampleChunk(chunk.data);
						foreach (loop; smpl.loops) {
							if (loop.type != 0) {
								warningf("Ignoring unsupported loop type %s", loop.type);
								continue;
							}
							newLoop = loop.end - loop.start;
							loopEnd = loop.end;
						}
					}
					if (chunk.fourCC == "data") {
						newSample = cast(short[])chunk.data;
						if (downMix) {
							const old = newSample;
							newSample = newSample[0 .. $ / 2];
							foreach (sampleIndex, chanSamples; old.chunks(2).enumerate) {
								newSample[sampleIndex] = cast(short)((chanSamples[0] + chanSamples[1]) / 2);
							}
						}
					}
				}
				if (!validated) {
					infof("Skipping invalid sample %s", matched.front.name);
					continue;
				}
				if ((newLoop != 0) && (loopEnd != newSample.length)) {
					warningf("Loop end (%s) != sample count (%s), unexpected results may occur!", loopEnd, newSample.length);
				}
				infof("Replacing sample with %s", matched.front.name);
				song.replaceSample(idx, newSample, newLoop);
			}
		}
	}

	nspc.loadSong(song);
	foreach (idx, channel; channelsEnabled) {
		nspc.setChannelEnabled(cast(ubyte)idx, channel != '0');
	}
	nspc.play();
	trace("Playing NSPC music");


	if (!dumpBRRFiles && (outfile != "")) {
		dumpWav(nspc, sampleRate, channels, outfile);
	} else if (printSong) {
		writeln("Instruments:");
		foreach (idx, instrument; song.instruments) {
			if ((instrument.sampleID < song.samples.length) && song.samples[instrument.sampleID].isValid && (instrument.tuning != 0)) {
				const sample = song.samples[instrument.sampleID];
				writef!"%s (%s) - Sample: %s (%s, samples: %s, "(idx, ((song.percussionBase > 0) && (idx > song.percussionBase)) ? "Percussion" : "Standard", instrument.sampleID, sample.hash.toHexString, sample.data.length);
				if (sample.loopLength) {
					writef!"Loop: %s-%s"(sample.loopLength, sample.data.length);
				} else {
					write("No loop");
				}
				writefln!"), ADSR/Gain: %s, tuning: %s"(instrument.adsrGain, instrument.tuning);
			}
		}
		writeln("Sequence:");
		writeln(song);
	} else if (dumpBRRFiles) {
		if (!outfile.exists) {
			mkdirRecurse(outfile);
		}
		foreach(idx, sample; song.getSamples) {
			const filename = buildPath(outfile, format!"%s.brr.wav"(sample.hash.toHexString));
			if (!filename.exists) {
				SampleChunk smpl;
				if (sample.loopLength > 0) {
					SampleLoop loop;
					loop.id = 0;
					loop.type = 0;
					loop.start = cast(uint)(sample.data.length - sample.loopLength);
					loop.end = cast(uint)sample.data.length;
					loop.fraction = 0;
					loop.count = 0; // infinite
					smpl.loops ~= loop;
				}

				nspcplay.util.wav.dumpWav(sample.data, sampleRate, 1, filename, smpl);
				infof("Writing %s samples to %s", sample.data.length, filename);
			}
		}
	} else {
		// Prepare to play music
		if (!initAudio(&_sampling_func, channels, sampleRate, &nspc)) {
			return 1;
		}
		trace("SDL audio init success");

		static void midiCallback(NSPCPlayer* player, MidiEvent event) {
			import std.algorithm.comparison : min;
			const channel = min(cast(ubyte)(midiChannel + (event.eventType & 0xF)), ubyte(7));
			switch (event.type) {
				case EventType.noteOn:
					if (event.param2 > 0) {
						const command = Command(cast(ubyte)(event.param1 + (2 - 4) * 12), 0, VCMDClass.note);
						player.executeCommand(channel, command);
					} else {
						const command = Command(0, 0, VCMDClass.noteDuration);
						player.executeCommand(channel, command);
					}
					break;
				case EventType.noteOff:
					const command = Command(0, 0, VCMDClass.noteDuration);
					player.executeCommand(channel, command);
					break;
				default:
					tracef("MIDI event: %s", event);
					break;
			}
		}

		if (midiDevice != midiDevice.max) {
			try {
				openMidiInDevice!midiCallback(midiDevice, &nspc);
				infof("Opened MIDI device: %s", getMidiInDeviceName(midiDevice));
			} catch (Exception e) {
				errorf("Could not open MIDI device %s: %s", midiDevice, e.msg);
			}
		}

		writeln("Press enter to exit");
		while(true) {
			if (kbhit()) {
				getch(); //make sure the key press is actually consumed
				break;
			}
			if (!nspc.isPlaying) {
				break;
			}
		}
	}
	nspc.stop();

	return 0;
}

void dumpWav(ref NSPCPlayer player, uint sampleRate, ushort channels, string filename) {
	player.looping = false;
	short[2][] samples;
	while (player.isPlaying) {
		short[2][4096] buffer;
		samples ~= player.fillBuffer(buffer[]);
	}
	SampleChunk smpl;
	nspcplay.util.wav.dumpWav(samples, sampleRate, channels, filename, smpl);
}
