#include "tss2-shim.h"
#include <tss2/tss2_fapi.h>

// `ForeignPtr a` wants a:
// `FunPtr (Ptr a -> IO ())` for cleanup, so we define a small one here
// to help.
void tss_shim_Fapi_Finalize(FAPI_CONTEXT *context) { Fapi_Finalize(&context); }
