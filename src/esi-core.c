#include <stdlib.h>
#include <string.h>
#include "emacs-module.h"

int plugin_is_GPL_compatible;
const char* esi_core_version = "0.0.1";

static emacs_value Fesi_core_version (emacs_env *env, ptrdiff_t n, emacs_value args[], void *data) {
  return env->make_string(env, esi_core_version, strlen(esi_core_version));
}

static void provide (emacs_env *env, const char *feature) {
  emacs_value Qfeat = env->intern(env, feature);
  emacs_value Qprovide = env->intern(env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall(env, Qprovide, 1, args);
}

static void bind_function (emacs_env *env, const char *name, emacs_value Sfun) {
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);
  emacs_value args[] = { Qsym, Sfun };
  env->funcall(env, Qfset, 2, args);
}

int emacs_module_init (struct emacs_runtime *ert) {
  emacs_env *env = ert->get_environment(ert);

  emacs_value vfun = env->make_function(env, 0, 0, Fesi_core_version, "Return version of esi-core.", NULL);
  bind_function(env, "esi-core-version", vfun);

  provide(env, "esi-core");

  return 0;
}
