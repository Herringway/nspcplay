module midi;
import nspcplay : NSPCPlayer;
version(Windows) {
	import core.sys.windows.windows;
	import core.sys.windows.mmsystem;
	pragma(lib, "winmm");
}

uint getNumMidiDevices() {
	return midiInGetNumDevs();
}

private string outputMidiError(uint err) {
	import std.string : fromStringz;
	import std.utf : toUTF8;
	wchar[256] errmsg;
	midiInGetErrorTextW(err, &errmsg[0], 255);
	return errmsg.fromStringz.toUTF8;
}
string getMidiInDeviceName(uint id) {
	import std.string : fromStringz;
	import std.utf : toUTF8;
	MIDIINCAPS caps;
	const result = midiInGetDevCapsW(id, &caps, caps.sizeof);
	return caps.szPname.fromStringz().toUTF8;
}

void closeMidiInDevice() {
	if (hMidiIn != null) {
		midiInStop(hMidiIn);
		midiInClose(hMidiIn);
		hMidiIn = null;
	}
}
private __gshared HMIDIIN hMidiIn = null;
void openMidiInDevice(alias callback)(uint deviceId, NSPCPlayer* player) {
	import std.exception : enforce;
	static void midiEnforce(uint ret) {
		enforce(ret == 0, outputMidiError(ret));
	}
	midiEnforce(midiInOpen(&hMidiIn, deviceId, cast(DWORD_PTR)cast(void*)&makeMidiFunc!callback, cast(DWORD_PTR)player, CALLBACK_FUNCTION));
	scope(failure) midiInClose(hMidiIn);
	midiEnforce(midiInStart(hMidiIn));
}
alias MidiFuncImpl = extern(C) void function(HMIDIIN, UINT, DWORD_PTR, DWORD_PTR, DWORD_PTR);

extern(C) void makeMidiFunc(alias func)(HMIDIIN, UINT wMsg, DWORD_PTR dwInstance, DWORD_PTR dwParam1, DWORD_PTR) {
	if (wMsg == MIM_DATA) {
		func(cast(NSPCPlayer*)dwInstance, MidiEvent(
			dwParam1 & 0xFF,
			(dwParam1 >> 8) & 0xFF,
			(dwParam1 >> 16) & 0xFF,
		));
	}
}

enum EventType {
	invalid,
	noteOff,
	noteOn,
	aftertouch,
	continuousController,
	patchChange,
	channelPressure,
	pitchBend,
	system
}

struct MidiEvent {
	ubyte eventType;
	ubyte param1;
	ubyte param2;
	EventType type() @safe const pure nothrow {
		if (eventType >= 0xF0) {
			return EventType.system;
		}
		switch (eventType >> 4) {
			case 8: return EventType.noteOff;
			case 9: return EventType.noteOn;
			case 10: return EventType.aftertouch;
			case 11: return EventType.continuousController;
			case 12: return EventType.patchChange;
			case 13: return EventType.channelPressure;
			case 14: return EventType.pitchBend;
			default: return EventType.invalid;
		}
	}
}

alias MidiFunc = void function(NSPCPlayer*, MidiEvent);
