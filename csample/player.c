#include "nspcplay.h"
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

char* readFile(const char* const path, size_t* size) {
	FILE* f = fopen(path, "rb");
	fseek(f, 0, SEEK_END);
	*size = ftell(f);
	fseek(f, 0, SEEK_SET);

	char* data = malloc(*size);
	fread(data, *size, 1, f);
	fclose(f);
	return data;
}

struct WAV {
	char riffSignature[4];
	uint32_t fileSize;
	char wavSignature[4];
	char fmtChunkSignature[4];
	uint32_t fmtLength;
	uint16_t format;
	uint16_t channels;
	uint32_t sampleRate;
	uint32_t secondSize;
	uint16_t sampleSize;
	uint16_t bitsPerSample;
	char dataSignature[4];
	uint32_t dataSize;
};

int main(int argc, char* argv[]) {
	struct NSPCPlayer* player;
	size_t dataSize;
	char* data;
	FILE* wavFile;
	int16_t buffer[4096][2];
	struct WAV wavStruct = {"RIFF", 0, "WAVE", "fmt ", 16, 1, 2, 44100, 44100 * 2 * 2, 2 * 2, 16, "data", 0};
	uint32_t bytes = 0;

	//prepare wav file
	wavFile = fopen("out.wav", "wb");
	fwrite(&wavStruct, sizeof(struct WAV), 1, wavFile);

	// create a new NSPC player with 44100hz sample rate
	player = nspcplayNew(44100);
	data = readFile(argv[1], &dataSize);
	// load an NSPC file
	nspcplayLoadNSPCFile(player, data, dataSize);

	// disable looping and start playing
	nspcplaySetLooping(player, 0);
	nspcplayPlay(player);

	// write samples until nothing is left
	while (nspcplayIsPlaying(player)) {
		bytes += nspcplayFillBuffer(player, buffer, 4096) * sizeof(int16_t) * 2;
		fwrite(&buffer, sizeof(buffer), 1, wavFile);
	}

	//update sizes in WAV header
	fseek(wavFile, offsetof(struct WAV, dataSize), SEEK_SET);
	fwrite(&bytes, sizeof(uint32_t), 1, wavFile);

	bytes += sizeof(struct WAV) - 8;
	fseek(wavFile, offsetof(struct WAV, fileSize), SEEK_SET);
	fwrite(&bytes, sizeof(uint32_t), 1, wavFile);

	//ok we're done here
	fclose(wavFile);

	nspcplayDestroy(player);
}
