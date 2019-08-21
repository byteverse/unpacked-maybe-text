# unpacked-maybe-text

This is kind of like the `unpacked-maybe` library. However, the type in this
library is specialized to handle `ShortText`, which eliminates an unnecessary
source of allocations when the `Maybe` is unpack into another data constructor.
