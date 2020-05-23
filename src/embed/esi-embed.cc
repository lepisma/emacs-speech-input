#include <torch/script.h>
#include "esi-embed.h"
#include <stdint.h>

// KLUDGE: Get the user_pointer piece working
torch::jit::script::Module* gmod;

embed_model_t* load_embed_model(char* filepath) {
  embed_model_t* m;

  try {
    gmod = new torch::jit::script::Module(torch::jit::load(filepath));
    // NOTE: The returned pointer doesn't work
    m->obj = gmod;
    return m;
  } catch (const c10::Error& e) {
    std::cerr << "error loading model\n" << e.msg() << std::endl;
    return m;
  }
}

// Run the module on given matrix (single item batch) and return a vector
double* embed_model_run(embed_model_t* m, double* mel_sg, size_t n_frames, size_t *n_out) {
  // torch::jit::script::Module* model;
  // model = static_cast<torch::jit::script::Module*>(m->obj);

  std::vector<torch::jit::IValue> inputs;
  size_t n_mels = 40;

  // NOTE: Float is needed for compute
  float* mel_sg_f = (float*)malloc(n_frames * n_mels * sizeof(float));
  for (size_t i = 0; i < n_frames * n_mels; i++) {
      mel_sg_f[i] = mel_sg[i];
  }

  // NOTE: The transpose is needed because of general librosa style dimension
  //       ordering.
  auto mel_sg_tensor = torch::from_blob(mel_sg_f, {1, n_mels, n_frames});
  inputs.push_back(at::transpose(mel_sg_tensor, 1, 2));

  *n_out = 256;
  at::Tensor output = gmod->forward(inputs).toTensor();
  auto slice = output.slice(1, 0, (*n_out))[0];

  double* output_vector = (double*)malloc((*n_out) * sizeof(double));
  for (size_t i = 0; i < (*n_out); i++) {
    output_vector[i] = slice[i].item().toDouble();
  }

  free(mel_sg_f);
  return output_vector;
}

void embed_model_destroy(embed_model_t *m) {
  // delete static_cast<torch::jit::script::Module *>(m->obj);
  delete m;
}
