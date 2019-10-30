#pragma once

#include "sndfile.h"
#include <math.h>
#include <soundio/soundio.h>
#include <stdbool.h>
#include <stdint.h>
#include <unistd.h>
#include <pthread.h>

typedef struct {
  sf_count_t offset, length;
  char *data;
} VIO_DATA;

sf_count_t vfget_filelen(void *user_data) {
  VIO_DATA *vf = (VIO_DATA *)user_data;
  return vf->length;
}

sf_count_t vfseek(sf_count_t offset, int whence, void *user_data) {
  VIO_DATA *vf = (VIO_DATA *)user_data;

  switch (whence) {
  case SEEK_SET:
    vf->offset = offset;
    break;

  case SEEK_CUR:
    vf->offset = vf->offset + offset;
    break;

  case SEEK_END:
    vf->offset = vf->length + offset;
    break;

  default:
    break;
  };

  return vf->offset;
}

sf_count_t vfread(void *ptr, sf_count_t count, void *user_data) {
  VIO_DATA *vf = (VIO_DATA *)user_data;

  if (vf->offset + count > vf->length)
    count = vf->length - vf->offset;

  memcpy(ptr, vf->data + vf->offset, count);
  vf->offset += count;

  return count;
}

sf_count_t vftell(void *user_data) {
  VIO_DATA *vf = (VIO_DATA *)user_data;
  return vf->offset;
}

double* samples_from_buffer(char* buffer, size_t buffer_size, sf_count_t* n_samples) {
  VIO_DATA vio_data;
  SF_VIRTUAL_IO vio;

  vio.get_filelen = vfget_filelen;
  vio.seek = vfseek;
  vio.read = vfread;
  vio.tell = vftell;

  vio_data.offset = 0;
  vio_data.length = buffer_size - 1;
  vio_data.data = buffer;

  SNDFILE *sfile;
  SF_INFO file_info;

  sfile = sf_open_virtual(&vio, SFM_READ, &file_info, &vio_data);

  sf_count_t frames_count = file_info.frames * file_info.channels;
  double *frames = malloc(sizeof(double) * frames_count);
  sf_readf_double(sfile, frames, frames_count);

  // KLUDGE: This is for picking up first channel data. There is too much
  //         copying happening overall.
  double *samples = malloc(sizeof(double) * file_info.frames);
  for (size_t i = 0; i < file_info.frames; i++) {
    samples[i] = frames[i * file_info.channels];
  }

  *n_samples = file_info.frames;

  sf_close(sfile);
  free(frames);
  return samples;
}


static int min_int(int a, int b) {
    return (a < b) ? a : b;
}


// Custom ring buffer. I might be wiser in using sio's implementation but there
// I need to keep read pointer n-steps ahead and stuff to make it behave how I
// want it to.
struct buffer {
  char* array;
  size_t capacity;
  size_t margin;
  size_t write_offset;
  size_t read_offset;
};

size_t s_max(size_t a, size_t b) {
  return (b > a) ? b : a;
}

struct buffer* buffer_init(size_t capacity) {
  struct buffer* buf = malloc(sizeof(struct buffer));

  // TODO: Make this smaller and adaptive. At present this covers one second of
  //       float-32 16k audio.
  size_t margin = s_max(capacity, 32 * 16000);

  buf->capacity = capacity;
  buf->margin = margin;
  buf->array = calloc(capacity + margin, sizeof(char));

  buf->write_offset = 0;
  buf->read_offset = buf->write_offset + buf->margin;

  return buf;
}

void buffer_destroy(struct buffer* buf) {
  free(buf->array);
  free(buf);
}

// Write data_len bytes from data in buffer.
bool buffer_write(struct buffer* buf, char* data, size_t data_len) {
  size_t input_offset = 0;
  if (data_len > buf->capacity) {
    fprintf(stderr, "Data more than the capacity, taking the last chunks provided");
    input_offset = (data_len - buf->capacity);
    data_len = buf->capacity;
  }

  size_t buffer_length = buf->capacity + buf->margin;
  for (size_t i = 0; i < data_len; i++) {
    buf->array[(buf->write_offset + i) % buffer_length] = data[input_offset + i];
  }

  buf->write_offset = (buf->write_offset + data_len) % buffer_length;
  buf->read_offset = (buf->write_offset + buf->margin) % buffer_length;
  return true;
}

// Read upto a max of capacity chars from buffer.
char* buffer_read(struct buffer* buf, size_t *output_size) {
  char* output = malloc(sizeof(char) * buf->capacity);

  size_t offset = buf->read_offset;
  size_t buffer_length = buf->capacity + buf->margin;

  for (size_t i = 0; i < buf->capacity; i++) {
    output[i] = buf->array[(offset + i) % buffer_length];
  }

  *output_size = buf->capacity;
  return output;
}

struct RecordContext {
  struct buffer *buf;
  struct SoundIo *soundio;
  struct SoundIoDevice *selected_device;
  pthread_t *poll_thread;
  bool keep_recording;
};

void recording_read_callback(struct SoundIoInStream *instream, int frame_count_min, int frame_count_max) {
  struct RecordContext *rc = instream->userdata;
  struct SoundIoChannelArea *areas;
  int err;

  size_t free_count = rc->buf->capacity / instream->bytes_per_frame;

  int write_frames = min_int(free_count, frame_count_max);
  int frames_left = write_frames;

  while (true) {
    int frame_count = frames_left;

    if ((err = soundio_instream_begin_read(instream, &areas, &frame_count))) {
      fprintf(stderr, "begin read error: %s", soundio_strerror(err));
      exit(1);
    }

    if (!frame_count)
      break;

    if (!areas) {
      fprintf(stderr, "are error thing\n");
    } else {
      for (int frame = 0; frame < frame_count; frame += 1) {
        for (int ch = 0; ch < instream->layout.channel_count; ch += 1) {
          buffer_write(rc->buf, areas[ch].ptr, instream->bytes_per_sample);
          areas[ch].ptr += areas[ch].step;
        }
      }
    }

    if ((err = soundio_instream_end_read(instream))) {
      fprintf(stderr, "end read error: %s", soundio_strerror(err));
      exit(1);
    }

    frames_left -= frame_count;
    if (frames_left <= 0)
      break;
  }
}

void recording_overflow_callback(struct SoundIoInStream *instream) {
  static int count = 0;
  fprintf(stderr, "overflow %d\n", ++count);
}

void *recording_poll_fn(void *arg) {
  struct SoundIoInStream *instream = (struct SoundIoInStream*)arg;
  struct RecordContext *rc = instream->userdata;

  while (rc->keep_recording) {
    soundio_flush_events(rc->soundio);
    sleep(1);
  }

  soundio_instream_destroy(instream);
  soundio_device_unref(rc->selected_device);
  soundio_destroy(rc->soundio);
  buffer_destroy(rc->buf);
  free(rc->poll_thread);
  free(rc);
  pthread_exit(NULL);
}

bool stop_background_recording(struct SoundIoInStream *instream) {
  struct RecordContext *rc = instream->userdata;
  rc->keep_recording = false;
  pthread_join(*(rc->poll_thread), NULL);
}

struct SoundIoInStream* start_background_recording(size_t sample_rate, size_t buffer_duration_seconds,
                                                    char *output_buffer) {
  enum SoundIoBackend backend = SoundIoBackendPulseAudio;
  enum SoundIoFormat fmt = SoundIoFormatS16LE;
  struct RecordContext *rc = malloc(sizeof(struct RecordContext));
  struct SoundIo *soundio = soundio_create();
  int err;

  if (!soundio) {
    fprintf(stderr, "OOM while creating soundio\n");
    return NULL;
  }

  if (soundio_connect_backend(soundio, backend)) {
    fprintf(stderr, "Error connecting: %s\n", soundio_strerror(err));
    return NULL;
  }

  soundio_flush_events(soundio);

  struct SoundIoDevice *selected_device = NULL;

  int device_index = soundio_default_input_device_index(soundio);
  selected_device = soundio_get_input_device(soundio, device_index);

  if (!selected_device) {
    fprintf(stderr, "No input devices available\n");
    return NULL;
  }

  if (selected_device->probe_error) {
    fprintf(stderr, "Unable to probe device: %s\n", soundio_strerror(selected_device->probe_error));
    return NULL;
  }

  soundio_device_sort_channel_layouts(selected_device);

  struct SoundIoInStream *instream = soundio_instream_create(selected_device);

  if (!instream) {
    fprintf(stderr, "OOM while creating instream\n");
    return NULL;
  }

  instream->format = fmt;
  instream->sample_rate = sample_rate;
  instream->read_callback = recording_read_callback;
  instream->overflow_callback = recording_overflow_callback;
  instream->userdata = rc;

  if ((err = soundio_instream_open(instream))) {
    fprintf(stderr, "Unable to open input stream: %s\n", soundio_strerror(err));
    return NULL;
  }

  size_t buffer_capacity = buffer_duration_seconds * instream->sample_rate * instream->bytes_per_frame;
  rc->buf = buffer_init(buffer_capacity);
  rc->soundio = soundio;
  rc->selected_device = selected_device;
  rc->keep_recording = true;
  rc->poll_thread = malloc(sizeof(pthread_t));

  if ((err = soundio_instream_start(instream))) {
    fprintf(stderr, "Unable to start input device: %s\n", soundio_strerror(err));
    return NULL;
  }

  if (pthread_create(rc->poll_thread, NULL, recording_poll_fn, (void*)(instream))) {
    fprintf(stderr, "Error creating thread\n");
    return NULL;
  }

  return instream;
}
