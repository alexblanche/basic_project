#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include <SDL2/SDL_render.h>
#include "sdlmissing_stub.h"

CAMLprim value
caml_SDL_DestroyRenderer(value renderer) {
    SDL_DestroyRenderer(SDL_Renderer_val(renderer));
    return Val_unit;
}

