/**
 * @file
 * Type conversion routines
 * @copyright
 * syn
 * Copyright (c) 2013-2017, Joshua Scoggins and Contributors
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


#ifndef _SYN_TYPES_H
#define _SYN_TYPES_H
#include <cstdint>
#include <climits>

using int8 = int8_t;
using uint8 = uint8_t;
using byte = uint8_t;
using int16 = int16_t;
using uint16 = uint16_t;
using int32 = int32_t;
using uint32 = uint32_t;
using int64 = int64_t;
using uint64 = uint64_t;
// lock into gcc and other targets which have access to 128-bit integers
using int128 = __int128;
using uint128 = unsigned __int128;

namespace syn {
/**
 * Retrieve the number of bits that a given type consumes. This is different
 * from plain sizeof in that this is CHAR_BIT * the number of bytes that make
 * up the target type.
 * @tparam T the type to find the size of
 */
template<typename T>
constexpr auto bitwidth = CHAR_BIT * sizeof(T);

/**
 * A little hack used to support int128 and anything other numeric constants by defining a constant one for a given
 * type.
 * @tparam T the type to coerce the number one into
 */
template<typename T>
constexpr auto numeralOne = static_cast<T>(0x1);

/**
 * Retrieve the number of positions that a given type can shift left and right by without clearing all digits
 * @tparam T the type of the thing
 */
template<typename T>
constexpr auto largestShiftValue = bitwidth<T> - 1;
} // end namespace syn

#if 0
// 128-bit printing operations
template<int mod>
std::ostream& walkString(std::ostream& stream, const uint128& value) {
    std::stringstream collect;
    for (auto copy = value; copy != 0; copy /= mod) {
        collect << static_cast<int>(copy%mod);
    }
    auto result = collect.str();
    std::reverse(result.begin(), result.end());
    stream << result;
    return stream;
}
std::ostream& operator<< (std::ostream& stream, const uint128& value) {
    if (value == 0) {
        stream << 0;
    } else {
        auto flags = stream.flags() & std::ios_base::basefield;
        if (flags == std::ios_base::hex) {
            auto upperHalf = getUpperHalf(value);
            if (upperHalf > 0) {
                stream << upperHalf;
            }
            stream << getLowerHalf(value);
        } else if (flags == std::ios_base::oct) {
            return walkString<8>(stream, value);
        } else {
            return walkString<10>(stream, value);
        }
    }
    return stream;
}
std::ostream& operator<< (std::ostream& stream, const int128& value) {
    if (value == 0) {
        stream << 0;
    } else {
        auto flags = stream.flags() & std::ios_base::basefield;
        if (flags == std::ios_base::dec) {
            if (value < 0) {
                stream << '-';
            }
            return operator<<(stream, static_cast<uint128>((~value) + 1));
        } else {
            return operator<<(stream, static_cast<uint128>(value));
        }
    }
}
#endif

#endif // end _SYN_BASE_H
