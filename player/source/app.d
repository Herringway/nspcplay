import nspcplay;

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
	want.format = SDL_AudioFormat.AUDIO_S16;
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
	if (args.length < 2) {
		return 1;
	}

	auto help = getopt(args,
		"c|channels", "Enables/disables channels", &channelsEnabled,
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
	auto song = loadNSPCFile(file, phrases);
	if (channelsEnabled.length != 8) {
		stderr.writeln("Channel string must be exactly 8 characters long!");
		return 1;
	}
	foreach (idx, channel; channelsEnabled) {
		nspc.setChannelEnabled(cast(ubyte)idx, channel != '0');
	}
	nspc.setSpeed(speed);

	nspc.interpolation = interpolation;
	if (replaceSamples != "") {
		foreach(idx, sample; song.getSamples) {
			const glob = format!"%s.*.brr.wav"(sample.hash.toHexString);
			auto matched = dirEntries(replaceSamples, glob, SpanMode.shallow);
			if (!matched.empty) {
				WAVFile header;
				int newLoop;
				if (auto loopSplit1 = matched.front.name.findSplit(".")) {
					if (auto loopSplit2 = loopSplit1[2].findSplit(".")) {
						newLoop = loopSplit2[0].to!int;
					}
				}

				auto newSample = readWav(matched.front.name, header);
				if (header.channels == 2) {
					const old = newSample;
					newSample = newSample[0 .. $ / 2];
					foreach (sampleIndex, chanSamples; old.chunks(2).enumerate) {
						newSample[sampleIndex] = cast(short)((chanSamples[0] + chanSamples[1]) / 2);
					}
				} else if (header.channels == 1) {
				} else {
					assert(0, "Sample must be mono or stereo!");
				}
				assert(header.bitsPerSample == 16, "Sample must be 16-bit!");
				infof("Replacing sample with %s", matched.front.name);
				song.replaceSample(idx, newSample, newLoop);
			}
		}
	}

	nspc.loadSong(song);
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
			const filename = buildPath(outfile, format!"%s.%s.brr.wav"(sample.hash.toHexString, sample.loopLength));
			if (!filename.exists) {
				dumpWav(sample.data, sampleRate, 1, filename);
				writeln("Writing ", filename);
			}
		}
	} else {
		// Prepare to play music
		if (!initAudio(&_sampling_func, channels, sampleRate, &nspc)) {
			return 1;
		}
		trace("SDL audio init success");


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

struct WAVFile {
	align(1):
	char[4] riffSignature = "RIFF";
	uint fileSize;
	char[4] wavSignature = "WAVE";
	char[4] fmtChunkSignature = "fmt ";
	uint fmtLength = 16;
	ushort format = 1;
	ushort channels;
	uint sampleRate;
	uint secondSize;
	ushort sampleSize;
	ushort bitsPerSample;
	char[4] dataSignature = "data";
	uint dataSize;
	void recalcSizes(size_t sampleCount) @safe pure {
		assert(sampleCount <= uint.max, "Too many samples");
		sampleSize = cast(ushort)(channels * bitsPerSample / 8);
		secondSize = sampleRate * sampleSize;
		dataSize = cast(uint)(sampleCount * sampleSize);
		fileSize = cast(uint)(WAVFile.sizeof - 8 + dataSize);
	}
}

void dumpWav(ref NSPCPlayer player, uint sampleRate, ushort channels, string filename) {
	player.looping = false;
	short[2][] samples;
	while (player.isPlaying) {
		short[2][4096] buffer;
		samples ~= player.fillBuffer(buffer[]);
	}
	dumpWav(samples, sampleRate, channels, filename);
}

void dumpWav(T)(T[] samples, uint sampleRate, ushort channels, string filename) {
	auto file = File(filename, "w");
	WAVFile header;
	header.sampleRate = sampleRate;
	header.channels = channels;
	header.bitsPerSample = 16;
	header.recalcSizes(samples.length);
	file.rawWrite([header]);
	file.rawWrite(samples);
}

short[] readWav(const string filename, out WAVFile header) {
	auto file = cast(const(ubyte)[])read(filename);
	header = (cast(const(WAVFile)[])(file[0 .. WAVFile.sizeof]))[0];
	assert((header.riffSignature == "RIFF") && (header.wavSignature == "WAVE"), "Invalid WAV file!");
	return cast(short[])(file[WAVFile.sizeof .. WAVFile.sizeof + header.dataSize]);
}
