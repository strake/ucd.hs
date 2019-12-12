#include <stdint.h>
#include "BidiBrackets.h"

uint32_t bidi_brackets(uint32_t x) {
    unsigned long length = sizeof(BidiBrackets) / sizeof(BidiBrackets[0]);
    const uint32_t *try, *base;
    base = BidiBrackets;
    while (length > 0) {
        try = &base[length/2];
        uint32_t r = try[0], y = r & 0x1FFFFF;
        switch ((x > y) - (x < y)) {
        case -1: length /= 2; break;
        case  1: base = try + 1; length -= length/2+1; break;
        default: return r;
        }
    }
    return x;
}
