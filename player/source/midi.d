module midi;
import nspcplay : NSPCPlayer;
version(Windows) {
	import core.sys.windows.windows;
	import core.sys.windows.mmsystem;
	pragma(lib, "winmm");
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

uint getNumMidiDevices() {
	version(Windows) {
		return midiInGetNumDevs();
	} else {
		throw new Exception("Unimplemented");
	}
}

private string outputMidiError(uint err) {
	version(Windows) {
		import std.string : fromStringz;
		import std.utf : toUTF8;
		wchar[256] errmsg;
		midiInGetErrorTextW(err, &errmsg[0], 255);
		return errmsg.fromStringz.toUTF8;
	} else {
		throw new Exception("Unimplemented");
	}
}
string getMidiInDeviceName(uint id) {
	version(Windows) {
		import std.string : fromStringz;
		import std.utf : toUTF8;
		MIDIINCAPS caps;
		const result = midiInGetDevCapsW(id, &caps, caps.sizeof);
		return caps.szPname.fromStringz().toUTF8;
	} else {
		throw new Exception("Unimplemented");
	}
}

void closeMidiInDevice() {
	version(Windows) {
		if (hMidiIn != null) {
			midiInStop(hMidiIn);
			midiInClose(hMidiIn);
			hMidiIn = null;
		}
	} else {
		throw new Exception("Unimplemented");
	}
}
void openMidiInDevice(alias callback)(uint deviceId, NSPCPlayer* player) {
	version(Windows) {
		import std.exception : enforce;
		static void midiEnforce(uint ret) {
			enforce(ret == 0, outputMidiError(ret));
		}
		midiEnforce(midiInOpen(&hMidiIn, deviceId, cast(DWORD_PTR)cast(void*)&makeMidiFunc!callback, cast(DWORD_PTR)player, CALLBACK_FUNCTION));
		scope(failure) midiInClose(hMidiIn);
		midiEnforce(midiInStart(hMidiIn));
	} else {
		throw new Exception("Unimplemented");
	}
}
version(Windows) {
	private __gshared HMIDIIN hMidiIn = null;

	extern(C) void makeMidiFunc(alias func)(HMIDIIN, UINT wMsg, DWORD_PTR dwInstance, DWORD_PTR dwParam1, DWORD_PTR) {
		if (wMsg == MIM_DATA) {
			func(cast(NSPCPlayer*)dwInstance, MidiEvent(
				dwParam1 & 0xFF,
				(dwParam1 >> 8) & 0xFF,
				(dwParam1 >> 16) & 0xFF,
			));
		}
	}
}
