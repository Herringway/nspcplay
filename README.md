# NSPCPlay

## Introduction

NSPCPlay is an implementation of the NSPC engine used and extended by many Super Nintendo games. Currently, only variants compatible with Earthbound's player are compatible, but this may be extended in the future.

## Building

### As a Dependency

Simply use `dub add nspcplay` in a D project. C compatible functions and header are not yet available.

### Standalone Player

Run `dub build :player` in the nspcplay directory.

## NSPC Format

An NSPC file has a simple structure:

```d
struct NSPC {
	uint variant; // Which version of NSPC this file uses
	ushort songAddress; // Base SPC address of the song's sequence data
	ushort instrumentAddress; // Base SPC address of the instruments
	ushort sampleAddress; // Base SPC address of the samples
	ubyte[22] reserved; // Currently unused
	// NSPC packs follow...
}

struct NSPCPack {
	ushort size;
	ushort address;
	ubyte[0] data; //'size' bytes of data
}

```

## Variants

### 0 - Earthbound

Used in Earthbound. Compatible with many other variants.

## Credits

NSPCPlay is based upon the code used in the Earthbound Music Editor, EBMusEd, and extended by Cameron 'Herringway' Ross.
