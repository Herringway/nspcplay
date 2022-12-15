module nspcplay.c;
import nspcplay;
import std.experimental.allocator;
import std.string;
import core.sys.windows.windows;
import core.sys.windows.dll;

private const(char)* lastError;

version(unittest) {} else {
	version(Windows) mixin SimpleDllMain;
}

export nothrow extern(C):

NSPCPlayer* nspcplayNew(int sampleRate) {
	NSPCPlayer* player = theAllocator.make!NSPCPlayer(sampleRate);
	return player;
}

void nspcplayDestroy(NSPCPlayer* player) {
	player.stop();
	std.experimental.allocator.dispose(theAllocator, player);
}

size_t nspcplayFillBuffer(scope NSPCPlayer* player, short[2]* buffer, size_t bufferLength) {
	return player.fillBuffer(buffer[0 .. bufferLength]).length;
}

int nspcplayLoadSequencePack(scope Song* song, const ubyte* data, size_t dataLength) {
	try {
		song.loadSequencePack(data[0 .. dataLength]);
	} catch (Exception e) {
		lastError = e.msg.toStringz;
		return 1;
	}
	return 0;
}
int nspcplayLoadInstrumentPack(scope Song* song, scope ubyte* buffer, scope const ubyte* data, size_t dataLength) {
	try {
		song.loadInstrumentPack(buffer[0 .. 65536], data[0 .. dataLength]);
	} catch (Exception e) {
		lastError = e.msg.toStringz;
		return 1;
	}
	return 0;
}
int nspcplayInitializeInstruments(scope Song* song, const scope ubyte* buffer, ushort instrumentBase, ushort sampleBase) {
	try {
		song.initializeInstruments(buffer[0 .. 65536], instrumentBase, sampleBase);
	} catch (Exception e) {
		lastError = e.msg.toStringz;
		return 1;
	}
	return 0;
}
int nspcplayLoadNSPCFile(scope NSPCPlayer* player, const scope ubyte* data, size_t dataLength) {
	try {
		player.loadSong(loadNSPCFile(data[0 .. dataLength]));
	} catch (Exception e) {
		lastError = e.msg.toStringz;
		return 1;
	}
	return 0;
}

void nspcplayPlay(scope NSPCPlayer* player) @safe {
	player.play();
}

void nspcplayStop(scope NSPCPlayer* player) @safe {
	player.stop();
}

void nspcplaySetSpeed(scope NSPCPlayer* player, ushort rate) @safe {
	player.setSpeed(rate);
}

void nspcplaySetLooping(scope NSPCPlayer* player, int enabled) @safe {
	player.looping = !!enabled;
}

void nspcplaySetChannelEnabled(scope NSPCPlayer* player, ubyte channel, int enabled) @safe {
	player.setChannelEnabled(channel, !!enabled);
}

int nspcplayIsPlaying(const scope NSPCPlayer* player) @safe {
	return player.isPlaying;
}

const(char)* nspcplayGetError() {
	return lastError;
}
