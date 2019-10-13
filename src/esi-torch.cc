#include <torch/script.h>
#include "esi-torch.h"

struct _torch_module {
  void *obj;
};

torch_module_t* load_torch_module(char* filepath) {
  torch_module_t* m;
  torch::jit::script::Module module;

  try {
    module = torch::jit::load(filepath);
    auto m_ptr = std::make_shared<torch::jit::script::Module>(module);
    // m->obj = m_ptr.get();
    return m;
  } catch (const c10::Error& e) {
    std::cerr << "error loading the module\n" << e.msg() << std::endl;
    return m;
  }
}

void torch_module_forward(torch_module_t* m, double* x) {
  torch::jit::script::Module* typed_module;
  typed_module = static_cast<torch::jit::script::Module*>(m->obj);

  std::vector<torch::jit::IValue> inputs;
  inputs.push_back(torch::rand({1, 10, 40}));
  at::Tensor output = typed_module->forward(inputs).toTensor();
  std::cout << output.slice(1, 0, 256) << std::endl;
}
