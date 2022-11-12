# NSPCPlay

## Introduction

NSPCPlay is an implementation of the NSPC engine used and extended by many Super Nintendo games. Currently, only variants compatible with Earthbound's player are compatible, but this may be extended in the future.

## Building

### As a Dependency

Simply use `dub add nspcplay` in a D project.

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
	ubyte releaseTable; // Release table to use
	ubyte volumeTable; // Volume table to use
	ubyte[19] reserved; // Currently unused
	ubyte firCoefficientTableCount; // Number of FIR coefficient tables in this file
	// NSPC packs here...
	ubyte[8][0] firCoefficientTables; // length determined by firCoefficientTableCount
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

### 1 - Prototype

Used in Super Mario World and Pilotwings.

## Credits

NSPCPlay is based upon the code used in the Earthbound Music Editor, EBMusEd, and extended by Cameron 'Herringway' Ross.
