#include <stdint.h>

typedef union {
    struct { uint32_t a, b; };
    uint64_t c;
} RangeU32;

const uint64_t *range_search(uint32_t x, const uint64_t *base, unsigned long length) {
    const uint64_t *try;
    while (length > 0) {
        try = &base[length/2];
        RangeU32 r = { .c = try[0] };
        if (x < r.a) {
            length /= 2;
        } else
        if (x >= r.b) {
            base = try + 1;
            length -= length/2+1;
        } else return try;
    }
    return 0;
}
