module nspcplay.spc;

union ID666Tags {
	TextID666Tags text;
	BinaryID666Tags binary;
}

struct TextID666Tags {
	align(1):
	char[32] songTitle;
	char[32] gameTitle;
	char[16] dumperName;
	char[32] comments;
	char[11] dumpDate;
	char[3] fadeStart;
	char[5] fadeLength;
	char[32] songArtist;
	ubyte defaultChannelEnables;
	ubyte emulator;
	ubyte[45] reserved;
}
struct BinaryID666Tags {
	align(1):
	char[32] songTitle;
	char[32] gameTitle;
	char[16] dumperName;
	char[32] comments;
	ubyte[4] dumpDate;
	ubyte[7] reserved;
	char[3] fadeStart;
	char[4] fadeLength;
	char[32] songArtist;
	ubyte defaultChannelEnables;
	ubyte emulator;
	ubyte[46] reserved2;
}