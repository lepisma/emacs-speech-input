#pragma once

#include "sndfile.h"

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

float* samples_from_buffer(char* buffer, size_t buffer_size, sf_count_t* n_samples) {
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
  float *frames = malloc(sizeof(float) * frames_count);
  sf_readf_float(sfile, frames, frames_count);

  // KLUDGE: This is for picking up first channel data. There is too much
  //         copying happening overall.
  float *samples = malloc(sizeof(float) * file_info.frames);
  for (int i = 0; i < file_info.frames; i++) {
    samples[i] = frames[i * file_info.channels];
  }

  *n_samples = file_info.frames;

  sf_close(sfile);
  free(frames);
  return samples;
}
