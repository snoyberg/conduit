#define INLINE_RULE0(new,old)             ;\
    new = old                            ;\
    {-# INLINE [0] new #-}               ;\
    {-# RULES "inline new" new = old #-}

#define INLINE_RULE(new,vars,body)                           ;\
    new vars = body                                         ;\
    {-# INLINE [0] new #-}                                  ;\
    {-# RULES "inline new" forall vars. new vars = body #-}

#define STREAMING0(name)                                 ;\
    name = name/**/C                                     ;\
    {-# INLINE [0] name #-}                              ;\
    {-# RULES "unstream name"                             \
      name = unstream (streamConduit name/**/C name/**/S) \
      #-}

#define STREAMING(name,vars)                                                ;\
    name = name/**/C                                                        ;\
    {-# INLINE [0] name #-}                                                 ;\
    {-# RULES "unstream name" forall vars.                                   \
      name vars = unstream (streamConduit (name/**/C vars) (name/**/S vars)) \
      #-}
