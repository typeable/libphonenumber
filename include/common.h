#pragma once

#include <string>
#include <cstddef>
#include <cstring>
#include <type_traits>

// Given `enum T { A, B, C }` and `enum S { X, Y, Z }`, we can use
// `is_same_enum<enum_values<T, A, B, C>, enum_values<S, X, Y, Z>>::value`
// to statically assert that the values A, B, C correspond respectively to the
// values X, Y, Z (i.e. that their underlying numerical values are the same),
// and thus that the listed members can be static_cast.
//
// We use this to redeclare C++ enums as C enums (which can then in turn be
// parsed by a C parser such as c2hs), and then ensure that our redeclaration
// matches the original. If the original were to be modified in a way that
// changes the numerical values of the listed members, we would get a
// static_assert. It's impossible to verify that no members other than the ones
// listed have been added, but such members will have to map to numerical values
// that are distinct, and thus not present in the other enum, thus in the worst
// case we get a `toEnum` error in haskell code.
template<typename T, T... Vals>
struct enum_values
{
	static_assert(std::is_enum<T>::value, "Must be an enum");
};

template<typename T, typename S>
struct is_same_enum: std::false_type
{};

template<typename T, T... TVals, typename S, S... SVals>
struct is_same_enum<enum_values<T, TVals...>, enum_values<S, SVals...>>:
	std::integral_constant<bool, std::is_same_v<
		std::integer_sequence<
			typename std::common_type<
				typename std::underlying_type<T>::type,
				typename std::underlying_type<S>::type
			>::type,
			TVals...
		>,
		std::integer_sequence<
			typename std::common_type<
				typename std::underlying_type<T>::type,
				typename std::underlying_type<S>::type
			>::type,
			SVals...
		>
	>>
{};

inline void unmarshal_string_size(std::string const &string, char *&c_string, size_t &c_string_size)
{
	c_string_size = string.size();
	c_string = static_cast<char *>(malloc(string.size()));
	std::memcpy(c_string, string.data(), string.size());
}
