#pragma once

#include <stdint.h>
#include <soundio/soundio.h>

// Return an array of single channel samples from headered audio bytes. Number
// of samples is written in n_samples
double* samples_from_buffer(char* buffer, size_t buffer_size, size_t* n_samples);

// Start a background recording process (not process process) and return
// instream with context. `buffer_duration_seconds` defines the length of last n
// seconds to keep in memory.
struct SoundIoInStream* start_background_recording(size_t sample_rate, size_t buffer_duration_seconds);

// Read PCM data from recording buffer and return. Length of output is written in `output_size`
char* read_background_recording(struct SoundIoInStream* instream, size_t* output_size);

// Stop recording specified by the instream.
void stop_background_recording(struct SoundIoInStream *instream);
