#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

struct NSPCPlayer;
struct Song;

struct NSPCPlayer* nspcplayNew(int32_t sampleRate);
void nspcplayDestroy(struct NSPCPlayer* player);
size_t nspcplayFillBuffer(struct NSPCPlayer* player, int16_t buffer[][2], size_t bufferLength);
int32_t nspcplayLoadSequencePack(struct Song* player, const uint8_t* const data, size_t dataLength);
int32_t nspcplayLoadInstrumentPack(struct Song* player, uint8_t* buffer, uint8_t const * const data, size_t dataLength);
int32_t nspcplayInitializeInstruments(struct Song* player, uint8_t const * const buffer, uint16_t instrumentBase, uint16_t sampleBase);
int32_t nspcplayLoadNSPCFile(struct NSPCPlayer* player, uint8_t const * const data, size_t dataLength);
void nspcplayPlay(struct NSPCPlayer* player);
void nspcplayStop(struct NSPCPlayer* player);
void nspcplaySetSpeed(struct NSPCPlayer* player, uint16_t rate);
void nspcplaySetLooping(struct NSPCPlayer* player, int32_t enabled);
void nspcplaySetChannelEnabled(struct NSPCPlayer* player, uint8_t channel, int32_t enabled);
int32_t nspcplayIsPlaying(struct NSPCPlayer const * player);
char const * const nspcplayGetError();

#ifdef __cplusplus
}
#endif
