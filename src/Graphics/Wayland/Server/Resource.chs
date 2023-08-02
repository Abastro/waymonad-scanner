module Graphics.Wayland.Server.Resource (
  Resource(..),
  Client(..),
  DestroyCallback,
  resourceGetClient,
  resourcePostEventArray,
  resourceCreate,
  resourceSetDispatcher,
)
where

import Foreign
import Foreign.C.Types

{# import Graphics.Wayland.Util.Types #}

#include <wayland-server.h>
{# context prefix="wl" #}

-- Why do I have to have this everywhere
{# typedef uint32_t Word32 #}

{# pointer *resource as Resource newtype #}
{# pointer *client as Client newtype #}

type DestroyCallback = Resource -> IO ()
foreign import ccall unsafe "wrapper" makeDestroyCallback :: DestroyCallback -> IO (FunPtr DestroyCallback)
withDestroyCallback :: DestroyCallback -> (FunPtr DestroyCallback -> IO a) -> IO a
withDestroyCallback func act = do
  fnPtr <- makeDestroyCallback func
  act fnPtr

-- |
-- Post an event to the client's object referred to by 'resource'.
-- 'opcode' is the event number generated from the protocol XML
-- description (the event name). The variable arguments are the event
-- parameters, in the order they appear in the protocol XML specification.
--
-- The variable arguments' types are:
-- - type=uint:  uint32_t
-- - type=int:    int32_t
-- - type=fixed:  wl_fixed_t
-- - type=string:  (const char *) to a nil-terminated string
-- - type=array:  (struct wl_array *)
-- - type=fd:    int, that is an open file descriptor
-- - type=new_id:  (struct wl_object *) or (struct wl_resource *)
-- - type=object:  (struct wl_object *) or (struct wl_resource *)
--
{# fun unsafe resource_post_event_array as ^ {
    `Resource',
    `Word32',
    `ArgumentPtr'
  } -> `()' #}

-- | No documentation.
--
-- > struct wl_resource *
-- > wl_resource_create(struct wl_client *client,
-- >   const struct wl_interface *interface,
-- >   int version, uint32_t id);
{# fun unsafe resource_create as ^ {
    `Client',
    `Interface',
    `Int',
    `Word32'
  } -> `Resource' #}

-- | No documentation.
--
-- > void
-- > wl_resource_set_dispatcher(struct wl_resource *resource,
-- >         wl_dispatcher_func_t dispatcher,
-- >         const void *implementation,
-- >         void *data,
-- >         wl_resource_destroy_func_t destroy);
{# fun unsafe resource_set_dispatcher as ^ {
    `Resource',
    withDispatcher* `Dispatcher',
    `Ptr ()',
    `Ptr ()',
    withDestroyCallback* `DestroyCallback'
  } -> `()' #}

-- | No documentation.
--
-- > struct wl_client *
-- > wl_resource_get_client(struct wl_resource *resource);
{# fun unsafe resource_get_client as ^ {
    `Resource'
  } -> `Client' #}
